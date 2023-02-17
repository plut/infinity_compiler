#![allow(
	clippy::single_match,
// 	unreachable_code,
// 	dead_code,
// 	unused_variables,
	unused_imports,
// 	unused_macros,
// 	unused_parens,
// 	unused_mut,
// 	unused_attributes,
// 	unused_assignments,
)]
use proc_macro::TokenStream;
use proc_macro2 as pm2;
use pm2::Span;
type TS = pm2::TokenStream;
use regex::Regex;
use quote::{quote, ToTokens};
use syn::{self, DeriveInput, parse_macro_input, Attribute};
use syn::{Data::Struct, DataStruct};
use syn::{Expr, ExprLit, ExprPath};
use syn::{Ident, Type, Field, Fields::Named, Fields::Unnamed};
use syn::{punctuated::Punctuated, token::Comma};
use extend::ext;

use std::cell::RefCell;
use std::fmt::Display;

#[derive(Debug)]
struct TopResource {
	table_name: String,
	type_name: String,
	ext: String,
	resref: u16,
}
impl TopResource {
	/// Reads a #[topresource(table_name, "itm", 0x03ed)] attribute.
	fn from(ident: &syn::Ident, mut parser: AttrParser)->Self {
		let table_name = match parser.get::<syn::Expr>() {
			Some(AttrArg::Ident(s)) => s,
			Some(AttrArg::Str(s)) => s,
			_ => panic!("bad topresource parameter 1, expected ident")
		};
		let ext = match parser.get::<syn::Expr>() {
			Some(AttrArg::Str(s)) => s,
			_ => panic!("bad topresource parameter 2, expected string")
		};
		let resref = match parser.get::<syn::Expr>() {
			Some(AttrArg::Int(n)) => n as u16,
			_ => panic!("bad topresource parameter 3, expected integer")
		};
		Self {
			type_name: ident.to_string(),
			table_name, ext, resref
		}
	}
	/// Stores this resource in the global table.
	fn store(self) {
		TOP.with(|v| v.borrow_mut().push(self))
	}
}
thread_local! {
	static TOP: RefCell<Vec<TopResource>> =
	RefCell::new(Vec::<TopResource>::new());
}
// use std::any::type_name;
// fn type_of<T>(_:&T)->&'static str { type_name::<T>() }

// macro_rules! dump {
// 	($($x: expr),*) => {
// 		panic!(concat!("dump:\n", $(stringify!($x), "={:#?}\n",)*), $($x,)* );
// 	}
// }

#[ext]
impl<T: ToTokens> T {
	fn toks_string(&self)->String { self.to_token_stream().to_string() }
}
#[ext]
impl syn::Ident {
	fn extend(&self, suffix: impl Display)->Self {
		Self::new(&format!("{self}{suffix}"), self.span())
	}
	fn into_type(&self)->syn::Type {
		syn::Type::Path(syn::TypePath {
			qself: None,
			path: syn::Path::from(self.clone())
		})
	}
}

#[ext]
impl syn::Type {
	/// From a type `crate::module::Foo<Bar>` returns `(Foo, <Bar>)`.
	fn name_and_args(&self)->Option<(&syn::Ident, &syn::PathArguments)> {
		let segments = match self {
			syn::Type::Path(syn::TypePath { path: syn::Path { segments, .. }, .. })
				=> segments,
			_ => return None
		};
		let syn::PathSegment { ident, arguments } = segments.last()?;
		Some((ident, arguments))
	}
	/// If this type is `Vec<T>` then return `Some(T)`.
	fn vec_eltype(&self)->Option<&Self> {
		let (ident, arguments) = self.name_and_args()?;
		if &ident.to_string() != "Vec" {
			return None
		}
		let args: &syn::punctuated::Punctuated<_,_> = match arguments {
			syn::PathArguments::AngleBracketed(ref g) => &g.args,
			_ => return None
		};
		match args.first()? {
			syn::GenericArgument::Type(ty) => Some(ty),
			_ => None
		}
	}
	/// From `crate::Type` return `TypeNode` identifier.
	fn extend(&self, suffix: impl Display)->Option<syn::Ident> {
		let (name, _) = self.name_and_args()?;
		Some(name.extend(suffix))
	}
}

#[derive(Debug)]
enum FieldNameRef<'a> {
	Named(&'a syn::Ident),
	Numbered(usize),
}
impl<'a> FieldNameRef<'a> {
	fn ident(&self)->&'a  syn::Ident {
		match self {
			Self::Named(ident) => ident,
			_ => panic!("only for named fields"),
		}
	}
}
impl ToTokens for FieldNameRef<'_> {
	fn to_tokens(&self, tokens: &mut TS) {
		match self {
			Self::Named(ident) => ident.to_tokens(tokens),
			Self::Numbered(n) => pm2::Literal::usize_unsuffixed(*n).to_tokens(tokens)
		}
	}
}
#[derive(Debug)]
struct FieldRef<'a> {
	name: FieldNameRef<'a>,
	ty: &'a syn::Type,
	attrs: &'a [syn::Attribute],
}
#[derive(Debug,Default)]
struct Fields {
	names: Option::<Vec<syn::Ident>>,
	types: Vec::<syn::Type>,
	attrs: Vec::<Vec<syn::Attribute>>,
}
impl Fields {
	fn iter(&self)->FieldsIter<'_> { FieldsIter(self, 0) }
	fn build_from(&self, vals: &[TS])->TS {
		let mut r = TS::new();
		if let Some(v) = &self.names {
			for (ident, val) in std::iter::zip(v.iter(), vals.iter()) {
				quote!(#ident: #val,).to_tokens(&mut r);
			}
			quote!( Self { #r } )
		} else {
			for val in vals.iter() {
				quote!(#val,).to_tokens(&mut r);
			}
			quote!( Self( #r ) )
		}
	}
}
impl From<syn::Data> for Fields {
	fn from(source: syn::Data)->Self {
		let fields = match source {
			Struct(DataStruct{ fields, .. }) => fields,
		_ => panic!("expected a struct"),
		};
		match fields {
		syn::Fields::Unit => Self::default(),
		syn::Fields::Named(s) => {
			let mut f = Self { names: Some(Vec::<_>::new()), ..Default::default() };
			for syn::Field { ident, ty, attrs, .. } in s.named {
				f.names.as_mut().unwrap().push(ident.unwrap());
				f.types.push(ty);
				f.attrs.push(attrs);
			}
			f
		},
		syn::Fields::Unnamed(s) => {
			let mut f = Self::default();
			for syn::Field { ty, attrs, .. } in s.unnamed {
				f.types.push(ty);
				f.attrs.push(attrs);
			}
			f
		},
		} // match
	}
}
struct FieldsIter<'a>(&'a Fields, usize);
impl<'a> Iterator for FieldsIter<'a> {
	type Item = FieldRef<'a>;
	fn next(&mut self)->Option<Self::Item> {
		let i = self.1;
		if i >= self.0.types.len() { return None }
		let r = FieldRef {
			name: match &self.0.names {
				Some(v) => FieldNameRef::Named(&v[i]),
				None => FieldNameRef::Numbered(i),
			},
			ty: &self.0.types[i],
			attrs: &self.0.attrs[i],
		};
		self.1 = i+1;
		Some(r)
	}
}
#[derive(Debug)]
enum AttrArg {
	Str(String),
	Int(isize),
	Ident(String),
	Type(syn::Type),
	Other(),
}
impl From<Expr> for AttrArg {
	fn from(arg: Expr)->Self {
		match arg {
		Expr::Lit(ExprLit{lit, ..}) => match lit {
			syn::Lit::Bool(syn::LitBool { value, .. }) =>
				AttrArg::Ident(value.to_string()),
			syn::Lit::Str(s) => AttrArg::Str(s.value()),
			syn::Lit::Int(s) => AttrArg::Int(s.base10_parse::<isize>()
				.expect("not an integer")),
			_ => AttrArg::Other()
			},
			Expr::Path(ExprPath{path, ..}) => AttrArg::Ident(path.toks_string()),
			_ => AttrArg::Other()
		}
	}
}
impl From<syn::Type> for AttrArg {
	fn from(arg: syn::Type)->Self { Self::Type(arg) }
}
// not an impl because we want to map this.
fn parse_attr(Attribute{path, tokens, ..}: &Attribute)->(String, AttrParser) {
	(path.get_ident().unwrap().to_string(), (*tokens).clone().into())
}

struct AttrParser(std::vec::IntoIter<pm2::TokenStream>);
impl AttrParser {
	fn split(stream: pm2::TokenStream)->Vec<pm2::TokenStream> {
		let mut v = vec![pm2::TokenStream::new()];
		for tree in stream {
			if match tree {
				pm2::TokenTree::Punct(ref p) => p.as_char() == ',',
				_ => false } {
				v.push(pm2::TokenStream::new());
			} else {
				let n = v.len();
				v[n-1].extend(std::iter::once(tree));
			}
		}
		v
	}
	fn get<T: Into<AttrArg> + syn::parse::Parse>(&mut self)->Option<AttrArg> {
		let a = match self.0.next() { None => return None, Some(a)=>a };
		match syn::parse2::<T>(a) { Err(_) => None, Ok(b)=>Some(b.into()) }
// 	let b: T = syn::parse2(a).expect("cannot parse!");
// 	Some(b.into())
	}
#[allow(dead_code)]
	fn get_ident(&mut self)->Option<String> {
		match self.get::<syn::Expr>() {
			Some(AttrArg::Ident(i)) => Some(i),
			Some(AttrArg::Str(s)) => Some(s),
			_ => None,
		}
	}
#[allow(dead_code)]
	fn get_int(&mut self)->Option<isize> {
		match self.get::<syn::Expr>() {
			Some(AttrArg::Int(n)) => Some(n),
			_ => None
		}
	}
}
impl From<pm2::TokenStream> for AttrParser {
	fn from(tokens: pm2::TokenStream)->Self {
// 		println!("tokens = {tokens:?}");
			Self(match tokens.into_iter().next() {
			Some(pm2::TokenTree::Group(g)) => Self::split(g.stream()).into_iter(),
			_ => { Vec::<pm2::TokenStream>::new().into_iter() },
// 			x => panic!("unknown attributes: {:?}", x),
		})
	}
}

/// Derivation of `Pack`.
#[proc_macro_derive(Pack, attributes(header))]
pub fn derive_pack(tokens: TokenStream) -> TokenStream {
	let DeriveInput{ ident, generics, data, .. } = parse_macro_input!(tokens);
	let fields = Fields::from(data);

	let mut buildv = Vec::<TS>::new();
	let mut readf = TS::new();
	let mut writef = TS::new();
	let mut where_pack = quote!{ where };
	for gen in generics.params.iter() {
		if let syn::GenericParam::Type(t) = gen {
			quote!{ #t: Pack, }.to_tokens(&mut where_pack);
		}
	}

	for (i, FieldRef { name, attrs, ty }) in fields.iter().enumerate() {
		let tmp = syn::Ident::new(&format!("tmp{i}"), Span::call_site());
		for (name, mut args) in attrs.iter().map(parse_attr) {
			match name.as_str() {
				"header" => {
					if let Some(AttrArg::Str(s)) = args.get::<syn::Expr>() {
						quote!{ Self::unpack_header(f, #s)?; }.to_tokens(&mut readf);
						quote!{ f.write_all(#s.as_bytes())?; }.to_tokens(&mut writef);
					}
				},
				&_ => () };
		}
		quote!{ let #tmp = #ty::unpack(f)?; }.to_tokens(&mut readf);
		quote!{ self.#name.pack(f)?; }.to_tokens(&mut writef);
		buildv.push(quote!{ #tmp });
	}
	let buildf = fields.build_from(&buildv);
	let code = quote! {
		impl #generics crate::pack::Pack for #ident #generics
		#where_pack {
			fn unpack(f: &mut impl std::io::Read)->std::io::Result<Self> {
				#readf Ok(#buildf)
			}
			fn pack(&self, f: &mut impl std::io::Write)->std::io::Result<()> {
				#writef Ok(())
			}
		}
	};
// 	if ident.to_string() == "KeyHdr" {
// 		println!("\x1b[32m{code}\x1b[m");
// 	}
	code.into()
} // Pack
/// This macro derives several traits for a newtype:
///  - `Debug` when all fields are `Debug`,
///  - `Display` when all fields are `Display`,
///  - `From` for a length-1 newtype,
///  - `unpack()` implem.,
#[proc_macro_derive(Newtype)]
pub fn derive_newtype(tokens: TokenStream)->TokenStream {
	let DeriveInput{ ident, generics, data, .. } = parse_macro_input!(tokens);
	let fields = Fields::from(data);
	if fields.types.len() != 1 {
		panic!("Newtype must be used only for length-1 wrappers...");
	}
	let FieldRef { name, ty, .. } = fields.iter().next().unwrap();
	let mut gen_params = TS::new();
	let mut generic_types = Vec::<&syn::Ident>::new();
	for gen in generics.params.iter() {
		match gen {
		syn::GenericParam::Type(t) => {
			t.ident.to_tokens(&mut gen_params);
			generic_types.push(&t.ident);
		},
		syn::GenericParam::Lifetime(l) => l.lifetime.to_tokens(&mut gen_params),
		syn::GenericParam::Const(c) => c.ident.to_tokens(&mut gen_params),
		}//match
		quote!{,}.to_tokens(&mut gen_params);
	}
	let mut code = quote!();
	let mut add_trait = |trait_name, trait_impl| {
		let mut clause = TS::new();
		for t in generic_types.iter() {
			quote!{ #t: #trait_name, }.to_tokens(&mut clause);
		}
		quote!{
			impl #generics #trait_name for #ident<#gen_params> where #clause {
				#trait_impl
			}
		}.to_tokens(&mut code);
	};
	add_trait(quote!(Debug), quote!{
		fn fmt(&self, f: &mut Formatter<'_>)->fmt::Result {
			write!(f, "{}({:?})", stringify!(#ident), self.#name)
		}
	});
	add_trait(quote!(Display), quote!{
		fn fmt(&self, f: &mut Formatter<'_>)->fmt::Result {
			Display::fmt(&self.#name, &mut f)
		}
	});
	add_trait(quote!(std::fmt::LowerHex), quote!{
			fn fmt(&self, f: &mut Formatter<'_>)->fmt::Result {
				std::fmt::LowerHex::fmt(&self.#name, f)
			}
	});
	add_trait(quote!(rusqlite::ToSql), quote!{
		fn to_sql(&self)->rusqlite::Result<rusqlite::types::ToSqlOutput<'_>> {
			self.#name.to_sql()
		}
	});
	let f_default = fields.build_from(&[quote!{ #ty::default() }]);
	add_trait(quote!(Default), quote!{
		fn default()->Self { #f_default }
	});
	let f_clone = fields.build_from(&[quote!{ self.#name.clone() }]);
	add_trait(quote!(Clone), quote!{
		fn clone(&self)->Self { #f_clone }
	});
	let f_from = fields.build_from(&[quote!{ source }]);
	add_trait(quote!(Copy), quote!{});
	let f_from_sql = fields.build_from(&[quote! { x }]);
	add_trait(quote!(rusqlite::types::FromSql), quote!{
		fn column_result(v: rusqlite::types::ValueRef<'_>)->rusqlite::types::FromSqlResult<Self> {
			T::column_result(v).map(|x| #f_from_sql)
		}
	});
	quote! {
		impl #generics From<#ty> for #ident<#gen_params> {
			fn from(source: #ty)->Self { #f_from }
		}
		impl #generics AsRef<#ty> for #ident<#gen_params> {
			fn as_ref(&self)->&#ty { &self.#name }
		}
		impl #generics AsMut<#ty> for #ident<#gen_params> {
			fn as_mut(&mut self)->&mut #ty { &mut self.#name }
		}
		impl #generics Deref for #ident<#gen_params> {
			type Target = #ty;
			fn deref(&self)->&Self::Target { &self.#name }
		}
		impl #generics #ident<#gen_params> {
			pub fn unwrap(self)->#ty { self.#name }
		}
	}.to_tokens(&mut code);
// 	println!("\x1b[35;1m{code}\x1b[m");
	code.into()
}

/// Information attached to the `column` attribute.
#[derive(Default,Debug)]
struct FieldInfo {
	create: String,
	/// is this field hidden from sql?
	hidden: bool,
}
impl FieldInfo {
	fn update(&mut self, parser: &mut AttrParser) {
		match parser.get::<syn::Expr>() {
			None => (),
			Some(AttrArg::Str(s)) => self.create = s,
			Some(AttrArg::Ident(i)) if i.as_str() == "false" => self.hidden = true,
			Some(a) => panic!("unknown argument for 'column' attribute: {a:?}"),
		}
	}
}
/// The macro deriving `SqlRow`.
#[proc_macro_derive(SqlRow, attributes(sql))]
pub fn derive_sql_row(tokens: TokenStream)->TokenStream {
	let mut fields_def = quote!{};
	let mut bind_at = quote!{};
	let mut collect_at = quote!{};
	let DeriveInput{ ident, data, .. } = parse_macro_input!(tokens);
	let fields = Fields::from(data);
	let mut field_index = 0usize;
	for FieldRef { name, attrs, ty, .. } in fields.iter() {
		// Read attributes for this field
		let mut field_info = FieldInfo::default();
		for (name, mut args) in attrs.iter().map(parse_attr) {
			match name.as_str() {
				"sql" => field_info.update(&mut args),
				_ => ()
			} // match
		} // for
		if ty.vec_eltype().is_some() {
			field_info.hidden = true;
		}
		if field_info.hidden {
			quote!{ #name: #ty::default(), }.to_tokens(&mut collect_at);
			continue
		}
		let create = field_info.create;
		quote!{
			crate::schemas::Field { fname: stringify!(#name),
				ftype: <#ty as crate::sql_rows::SqlLeaf>::FIELD_TYPE,
				create: #create },
		}.to_tokens(&mut fields_def);
		quote!{
			s.raw_bind_parameter(#field_index + offset + 1, &self.#name)?;
		}.to_tokens(&mut bind_at);
		quote!{
			#name: row.get::<_,#ty>(#field_index + offset)?,
		}.to_tokens(&mut collect_at);
		field_index+= 1;
	}
	let code = quote! {
		impl crate::sql_rows::SqlRow for #ident {
			const FIELDS: crate::sql_rows::SqlRowData = crate::sql_rows::SqlRowData {
				fields: crate::schemas::Fields(&[ #fields_def ]),
				ext: ""
			};
			const FIELDS9: crate::schemas::Fields =
				crate::schemas::Fields(&[ #fields_def ]);
			fn bind_at(&self, s: &mut Statement<'_>, offset: usize)->Result<()> {
				#bind_at
				Ok(())
			}
			fn collect_at(row: &Row<'_>, offset: usize)->Result<Self> {
				Ok(Self { #collect_at })
			}
		}
	};
// 	println!("{}", code);
	code.into()
}

/// The structure that does the work of producing the `FooNode` code.
#[derive(Debug)]
struct DeriveResourceTree {
	ident: syn::Ident,
	field: Vec<syn::Ident>,
	ty: Vec<syn::Type>,
}
impl DeriveResourceTree {
	fn new(ident: &syn::Ident)->Self {
		Self {
			ident: ident.clone(),
			field: Vec::new(),
			ty: Vec::new(),
		}
	}
	fn push(&mut self, fr: &syn::Ident, ft: &syn::Type) {
		self.field.push(fr.clone());
		self.ty.push(ft.clone());
	}
}
impl ToTokens for DeriveResourceTree {
	fn to_tokens(&self, dest: &mut TS) {
		let Self { ident, field, ty } = self;
		let forestname = ident.extend("Forest");
		let subforest = ty.iter().map(|x| x.extend("Forest"))
			.collect::<Vec<_>>();
		quote! {
			/// A `Node` impl. derived by `derive(ResourceTree)`.
			#[derive(Debug)]
			pub struct #forestname<X: Debug> {
				#(pub #field: crate::resources::Tree<#subforest::<X>>,)*
				_marker: PhantomData<X>
			}
			/// Runtime search in the tree.
			/// this would be a bit hard to do with `recurse` — the lifetimes are
			/// a mess, and we want to interrupt search as soon as we find *and*
			/// cut branches with a non-matching name:
			impl<X: Debug> crate::resources::Forest for #forestname<X> {
				type In = X;
				fn by_name<'a>(&'a self, s: &str)->Option<&'a X> {
					#(if let Some(tail) = s.strip_prefix(stringify!(#field)) {
						return self.#field.by_name(tail)
					})*
					None
				}
				/// Same, with mutable reference.
				fn by_name_mut<'a>(&'a mut self, s: &str)->Option<&'a mut X> {
					#(if let Some(tail) = s.strip_prefix(stringify!(#field)) {
						return self.#field.by_name_mut(tail)
					})*
					None
				}
			}
			impl<X: Debug, Y:Debug> crate::resources::TreeRecurse<Y> for #forestname<X> {
				type To = #forestname<Y>;
				fn recurse<'a,'n,S,E,F>(&'a self, f: F, name: &'n str, state: &S)
					->Result<Self::To,E>
				where F: Fn(&'a Self::In, &'n str, &S)->Result<(S,Y),E> {
					Ok(#forestname {
					#(#field:
						self.#field.recurse(&f, stringify!(#field), &state)?,)*
						_marker: PhantomData
					})
				}
				fn recurse_mut<'a,'n,S,E,F>(&'a self, mut f: F, name: &'n str, state: &S)
					->Result<Self::To,E>
				where F: FnMut(&'a Self::In, &'n str, &S)->Result<(S,Y),E> {
					Ok(#forestname {
					#(#field:
						self.#field.recurse_mut(&mut f, stringify!(#field), &state)?,)*
						_marker: PhantomData
					})
				}
			}
			impl crate::resources::ResourceTree for #ident {
				type FieldsTree = crate::resources::Tree<#forestname<crate::sql_rows::SqlRowData>>;
				const FIELDS_TREE: Self::FieldsTree = Self::FieldsTree {
					content: <Self as crate::sql_rows::SqlRow>::FIELDS,
					branches: #forestname {
					#(#field: <#ty as crate::resources::ResourceTree>::FIELDS_TREE,)*
						_marker: PhantomData
					}
				};
			}
// -- end of quote:
		}.to_tokens(dest)
	}
}

/// The macro deriving `Resource`.
#[proc_macro_derive(ResourceTree,attributes(topresource))]
pub fn derive_resource(tokens: TokenStream)->TokenStream {
	let DeriveInput{ ident, data, attrs,.. } = parse_macro_input!(tokens);
	let fields = Fields::from(data);
	let mut node_struct = quote!{};
	let mut recurse = quote!{};
	let mut recurse_mut = quote!{};
	let mut by_name = quote!{};
	let mut by_name_mut = quote!{};
	let mut fields_node = quote!{};
	let mut primary = quote!{ i64 };
	let mut insert_sub = quote!{};
	let mut select_sub = quote!{};
	let mut ext = String::new();
	for (name, args) in attrs.iter().map(parse_attr) {
		match name.as_str() {
			"topresource" => {
				let top = TopResource::from(&ident, args);
				ext = top.ext.clone();
				top.store();
				primary = quote!{ crate::gamefiles::Resref };
			},
			_ => ()
		}
	}
	let mut derive_forest = DeriveResourceTree::new(&ident);
	for FieldRef { name, ty, .. } in fields.iter() {
		// Read attributes for this field
// 		for (name, mut _args) in attrs.iter().map(parse_attr) {
// 			match name.as_str() {
// 				_ => ()
// 			} // match
// 		} // for
		if let Some(eltype) = ty.vec_eltype() {
			derive_forest.push(&name.ident(), eltype);
			let subnode = eltype.extend("Node");
			quote!{ pub #name: #subnode::<T>, }.to_tokens(&mut node_struct);
			quote!{ #name:
				self.#name.recurse(&f, stringify!(#name), &new_state)?, }
				.to_tokens(&mut recurse);
			quote!{ #name:
				self.#name.recurse_mut(&mut f, stringify!(#name), &new_state)?, }
				.to_tokens(&mut recurse_mut);
			quote!{ #name: <#eltype as crate::resources::RecResource>::FIELDS_NODE, }
				.to_tokens(&mut fields_node);
			quote!{ for (index, sub) in self.#name.iter().enumerate() {
					sub.insert_as_subresource(db, &mut node.#name, primary, index)?;
				}
			}.to_tokens(&mut insert_sub);
			quote!{ self.#name = #eltype::collect_all(&mut node.#name, (primary,))?; }
				.to_tokens(&mut select_sub);
			quote!{
				if let Some(new_target) = tail.strip_prefix(stringify!(#name)) {
					return self.#name.by_name(new_target)
				}
			}.to_tokens(&mut by_name);
			quote!{
				if let Some(new_target) = tail.strip_prefix(stringify!(#name)) {
					return self.#name.by_name_mut(new_target)
				}
			}.to_tokens(&mut by_name_mut);
		}
	}
	let node_ty = ident.extend("Node");
	let _code0 = quote!{
		impl crate::resources::RecResource for #ident {
			type FieldNode = #node_ty<(&'static str, crate::schemas::Fields)>;
			const FIELDS_NODE: Self::FieldNode = #node_ty {
				#fields_node
				content: (#ext, <Self as crate::sql_rows::SqlRow>::FIELDS9)
			};
			type Primary = #primary;
			type StatementNode<'a> = #node_ty<Statement<'a>>;
			fn insert_subresources(&self, db: &Connection, node: &mut #node_ty<Statement<'_>>,
				primary: impl rusqlite::ToSql+Copy)->Result<()> { #insert_sub Ok(()) }
			fn select_subresources(&mut self, node: &mut #node_ty<Statement<'_>>,
				primary: #primary)->Result<()> { #select_sub Ok(()) }
		}
// 		impl<T: Debug> Node<T> for #ident {
// 			type Output = #node_ty<T>
// 		}
	};
// 	println!("{}", code);
	let code = quote! { #derive_forest
		/// A `Node` impl. derived by `derive(ResourceTree)`.
		#[derive(Debug)]
		pub struct #node_ty<T: Debug> { #node_struct content: T }
		impl<X: Debug> Deref for #node_ty<X> {
			type Target = X;
			fn deref(&self)->&X { &self.content }
		}
		impl<X: Debug> DerefMut for #node_ty<X> {
			fn deref_mut(&mut self)->&mut X { &mut self.content }
		}
		/// Runtime search in the tree.
		/// this would be a bit hard to do with `recurse` — the lifetimes are
		/// a mess, and we want to interrupt search as soon as we find *and*
		/// cut branches with a non-matching name:
		impl<X: Debug> #node_ty<X> {
			pub fn by_name<'a>(&'a self, target: &str)->Option<&'a X> {
				if target.is_empty() { return Some(&self.content) }
				if target.as_bytes()[0] != b'_' { return None }
				let tail = &target[1..];
				#by_name
				None
			}
			/// Same, with mutable reference.
			pub fn by_name_mut<'a>(&'a mut self, target: &str)->Option<&'a mut X> {
				if target.is_empty() { return Some(&mut self.content) }
				if target.as_bytes()[0] != b'_' { return None }
				let tail = &target[1..];
				#by_name_mut
				None
			}
		}
		impl<X: Debug, Y:Debug> crate::resources::Recurse<Y> for #node_ty<X> {
			type To = #node_ty<Y>;
			fn recurse<'a,'n,S,E,F>(&'a self, f: F, name: &'n str, state: &S)
				->Result<Self::To,E>
			where F: Fn(&'a Self::Target, &'n str, &S)->Result<(S,Y),E> {
				let (new_state, content) = f(&self.content, name, state)?;
				Ok(#node_ty { #recurse content })
			}
			fn recurse_mut<'a,'n,S,E,F>(&'a self, mut f: F, name: &'n str, state: &S)
				->Result<Self::To,E>
			where F: FnMut(&'a Self::Target, &'n str, &S)->Result<(S,Y),E> {
				let (new_state, content) = f(&self.content, name, state)?;
				Ok(#node_ty { #recurse_mut content })
			}
		}
		impl crate::resources::RecResource for #ident {
			type FieldNode = #node_ty<(&'static str, crate::schemas::Fields)>;
			const FIELDS_NODE: Self::FieldNode = #node_ty {
				#fields_node
				content: (#ext, <Self as crate::sql_rows::SqlRow>::FIELDS9)
			};
			type Primary = #primary;
			type StatementNode<'a> = #node_ty<Statement<'a>>;
			fn insert_subresources(&self, db: &Connection, node: &mut #node_ty<Statement<'_>>,
				primary: impl rusqlite::ToSql+Copy)->Result<()> { #insert_sub Ok(()) }
			fn select_subresources(&mut self, node: &mut #node_ty<Statement<'_>>,
				primary: #primary)->Result<()> { #select_sub Ok(()) }
		}
// 		impl<T: Debug> Node<T> for #ident {
// 			type Output = #node_ty<T>
// 		}
	};
// 	println!("{code}");
	code.into()
}

#[proc_macro]
pub fn top_resources(_: TokenStream)->TokenStream {
	let mut data = quote!{};
	let mut recurse = quote!{};
	let mut recurse_mut = quote!{};
	let mut by_name = quote!{};
	let mut by_name_mut = quote!{};
	let mut const_def = quote!{};
	let mut derive_root = DeriveResourceTree::new(&syn::Ident::new("RootNode", Span::call_site()));
	TOP.with(|v| {
		for TopResource { type_name, table_name, ext: _ext, resref: _res }
				in v.borrow().iter() {
			let field = syn::Ident::new(table_name, Span::call_site());
			let ty = syn::Ident::new(type_name, Span::call_site());
			let ty2 = syn::Ident::new(type_name, Span::call_site()).into_type();
			derive_root.push(&field, &ty2);
			let subnode = ty.extend("Node");
			quote!{ pub #field: #subnode<X>, }.to_tokens(&mut data);
			quote!{ #field: self.#field.recurse(f, stringify!(#field), init)?, }
				.to_tokens(&mut recurse);
			quote!{ #field: self.#field.recurse_mut(f, stringify!(#field), init)?, }
				.to_tokens(&mut recurse_mut);
			quote!{
				#field: <#ty as crate::resources::RecResource>::FIELDS_NODE,
			}.to_tokens(&mut const_def);
			quote!{
				if let Some(new_target) = tail.strip_prefix(stringify!(#field)) {
					return self.#field.by_name(new_target)
				}
			}.to_tokens(&mut by_name);
			quote!{
				if let Some(new_target) = tail.strip_prefix(stringify!(#field)) {
					return self.#field.by_name_mut(new_target)
				}
			}.to_tokens(&mut by_name_mut);
		}
	});
	let code = quote!{
		/// A `Node` impl. derived by `derive(ResourceTree)`.
		#[derive(Debug)] pub struct RootNode<X: Debug> { #data
			_marker: PhantomData<X>
		}
		impl<X: Debug> Deref for RootNode<X> {
			type Target = X;
			fn deref(&self)->&X { unimplemented!() }
		}
		impl<X: Debug> DerefMut for RootNode<X> {
			fn deref_mut(&mut self)->&mut X { unimplemented!() }
		}
		impl<X: Debug> RootNode<X> {
			/// Runtime search in the tree.
			/// this would be a bit hard to do with `recurse` — the lifetimes are
			/// a mess, and we want to interrupt search as soon as we find *and*
			/// cut branches with a non-matching name:
			pub fn by_name<'a>(&'a self, target: &str)->crate::Result<&'a X> {
				self.by_name1(target).ok_or(Error::UnknownTable(target.into()).into())
			}
			fn by_name1<'a>(&'a self, tail: &str)->Option<&'a X> {
				#by_name None }
			/// Same, with mutable reference.
			pub fn by_name_mut<'a>(&'a mut self, target: &str)->crate::Result<&'a mut X> {
				self.by_name_mut1(target).ok_or(Error::UnknownTable(target.into()).into())
			}
			fn by_name_mut1<'a>(&'a mut self, tail: &str)->Option<&'a mut X> {
				#by_name_mut None }
		}
		impl<X: Debug, Y:Debug> crate::resources::Recurse<Y> for RootNode<X> {
			type To = RootNode<Y>;
			fn recurse<'a,'n,S,E,F>(&'a self, f: F, _: &'n str, init: &S)
				->Result<Self::To,E>
			where F: Fn(&'a Self::Target, &'n str, &S)->Result<(S,Y),E> {
				Ok(RootNode { #recurse _marker: PhantomData })
			}
			fn recurse_mut<'a,'n,S,E,F>(&'a self, f: F, _: &'n str, init: &S)
				->Result<Self::To,E>
			where F: FnMut(&'a Self::Target, &'n str, &S)->Result<(S,Y),E> {
				Ok(RootNode { #recurse_mut _marker: PhantomData })
			}
		}
		pub const TOP_FIELDS: RootNode<(&'static str, crate::schemas::Fields)> =
		RootNode { #const_def _marker: PhantomData };
	};
// 	println!("{}", code);
	code.into()
}
