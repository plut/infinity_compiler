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
type TS = pm2::TokenStream;
use regex::Regex;
use quote::{quote, ToTokens};
use syn::{self, DeriveInput, parse_macro_input, Attribute};
use syn::{Data::Struct, DataStruct};
use syn::{Expr, ExprLit, ExprPath};
use syn::{Field, Fields::Named, Fields::Unnamed};
use syn::{punctuated::Punctuated, token::Comma};

#[derive(Debug)] struct ResourceDef(String, String);
thread_local! {
	static RESOURCES: std::cell::RefCell<Vec<ResourceDef>> =
		std::cell::RefCell::new(Vec::<ResourceDef>::new());
}
fn push_resource(rdef: ResourceDef) {
	RESOURCES.with(|v| { v.borrow_mut().push(rdef); })
}
// use std::any::type_name;
// fn type_of<T>(_:&T)->&'static str { type_name::<T>() }

// macro_rules! dump {
// 	($($x: expr),*) => {
// 		panic!(concat!("dump:\n", $(stringify!($x), "={:#?}\n",)*), $($x,)* );
// 	}
// }

fn toks_to_string(a: &impl quote::ToTokens) -> String {
	let mut ts = TS::new();
	a.to_tokens(&mut ts);
	ts.to_string()
}
#[derive(Debug)]
enum FieldNameRef<'a> {
	Named(&'a syn::Ident),
	Numbered(usize),
}
impl quote::ToTokens for FieldNameRef<'_> {
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
	fn iter<'a>(&'a self)->FieldsIter<'_> { FieldsIter(&self, 0) }
	fn build_from(&self, vals: &[TS])->TS {
		let mut r = TS::new();
		if let Some(v) = &self.names {
			for (ident, val) in std::iter::zip(v.iter(), vals.iter()) {
				quote!(#ident: #val,).to_tokens(&mut r);
			}
			quote!( Self { #r } ).into()
		} else {
			for val in vals.iter() {
				quote!(#val,).to_tokens(&mut r);
			}
			quote!( Self( #r ) ).into()
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
fn struct_fields(data: syn::Data)->Vec<syn::Field> {
	match data {
		Struct(DataStruct{ fields: Named(f), ..}) =>
			f.named.into_iter().collect(),
// 		Struct(DataStruct{ fields: Unnamed(f), .. }) => f.unnamed,
		_ => panic!("only struct with named fields!") }
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
			Expr::Path(ExprPath{path, ..}) => AttrArg::Ident(toks_to_string(&path)),
			_ => AttrArg::Other()
		}
	}
}
impl From<syn::Type> for AttrArg {
	fn from(arg: syn::Type)->Self { Self::Type(arg) }
}
fn parse_attr(Attribute{path, tokens, ..}: &Attribute)->(String, AttrParser) {
	(path.get_ident().unwrap().to_string(), (*tokens).clone().into())
}

struct AttrParser(std::vec::IntoIter<pm2::TokenStream>);

/// Information attached to the `column` attribute.
#[derive(Default,Debug)]
struct ColumnInfo {
	extra: String,
	no_column: bool,
}
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
	// The following functions implement the parsing of individual
	// attributes; the name of the function matches that of the attribute:
	fn column(&mut self)->ColumnInfo {
		let mut field_info = ColumnInfo::default();
		match self.get::<syn::Expr>() {
			None => (),
			Some(AttrArg::Str(s)) => field_info.extra = s,
// 			Some(AttrArg::Ident(i)) if i.as_str() == "false" =>
// 				field_info.no_column = true,
			Some(a) => panic!("unknown argument for 'column' attribute: {:?}", a),
		}
		return field_info
	}
	/// Reads the `resource` attribute for a top-level resource.
	fn top_resource(&mut self)->ResourceInfo {
		let mut resource_info = ResourceInfo::default();
		resource_info.name = match self.get::<syn::Expr>() {
			Some(AttrArg::Ident(i)) => i,
			Some(AttrArg::Str(s)) => s,
			_ => return resource_info,
		};
		resource_info.extension = match self.get::<syn::Expr>() {
			Some(AttrArg::Str(s)) => s,
			_ => return resource_info,
		};
		resource_info.restype = match self.get::<syn::Expr>() {
			Some(AttrArg::Int(n)) => n as u16,
			x => { println!("got attribute: {x:?}"); return resource_info; }
		};
		return resource_info;
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

#[proc_macro_derive(Pack, attributes(header))]
pub fn derive_pack(tokens: TokenStream) -> TokenStream {
	let DeriveInput{ ident, generics, data, .. } = parse_macro_input!(tokens);
	let fields = Fields::from(data);

	let mut buildv = Vec::<TS>::new();
	let mut readf = TS::new();
	let mut writef = TS::new();
	let mut where_pack = TS::from(quote!{ where });
	for gen in generics.params.iter() {
		if let syn::GenericParam::Type(t) = gen {
			quote!{ #t: Pack, }.to_tokens(&mut where_pack);
		}
	}

	for (i, FieldRef { name, attrs, ty }) in fields.iter().enumerate() {
		let tmp = syn::Ident::new(&format!("tmp{i}"), pm2::Span::call_site());
		for (name, mut args) in attrs.iter().map(parse_attr) {
			match name.as_str() {
				"header" => pack_attr_header(&mut readf, &mut writef, &mut args),
				&_ => () };
		}
		quote!{ let #tmp = #ty::unpack(f)?; }.to_tokens(&mut readf);
		quote!{ self.#name.pack(f)?; }.to_tokens(&mut writef);
		buildv.push(quote!{ #tmp }.into());
	}
	let buildf = fields.build_from(&buildv);
	let code = quote! {
		impl #generics crate::struct_io::Pack for #ident #generics
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
fn pack_attr_header(readf: &mut TS, writef: &mut TS, args: &mut AttrParser) {
	if let Some(AttrArg::Str(s)) = args.get::<syn::Expr>() {
		quote!{ Self::unpack_header(f, #s)?; }.to_tokens(readf);
		quote!{ f.write_all(#s.as_bytes())?; }.to_tokens(writef);
	}
}

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
				Display::fmt(&self.#name, f)
			}
	});
	add_trait(quote!(std::fmt::LowerHex), quote!{
			fn fmt(&self, f: &mut Formatter<'_>)->fmt::Result {
				std::fmt::LowerHex::fmt(&self.#name, f)
			}
	});
	let f_clone = fields.build_from(&[quote!{ self.#name.clone() }]);
	add_trait(quote!(Clone), quote!{
		fn clone(&self)->Self { #f_clone }
	});
	let f_from = fields.build_from(&[quote!{ source }]);
	add_trait(quote!(Copy), quote!{});
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
	code.into()
}

#[derive(Default,Debug)]
struct ResourceInfo {
	name: String,
	extension: String,
	restype: u16,
}
#[proc_macro_derive(SqlRow, attributes(column))]
pub fn derive_sql_row(tokens: TokenStream)->TokenStream {
	let mut get_fields = TS::new();
	let mut from_row_at_v = Vec::<TS>::new();
	let mut fieldtype = TS::new();
	let mut fieldname_ctx = TS::new();
	let mut field_create_text = TS::new();
	let mut primary_index = TS::from(quote!(None));
	let mut offset = TS::from(quote!(0usize));
	let DeriveInput{ ident, data, .. } = parse_macro_input!(tokens);
	let fields = Fields::from(data);
	for FieldRef { name, attrs, ty, .. } in fields.iter() {
		// Read attributes for this field
		let mut field_info = ColumnInfo::default();
		for (name, mut args) in attrs.iter().map(parse_attr) {
			match name.as_str() {
				"column" => field_info = args.column(),
				_ => ()
			} // match
		} // for
		// before generating the code, `offset` contains the offset at the
		// start of this field:
		quote!{
			if i < #offset + #ty::WIDTH {
				return self.#name.get_field(i - (#offset))
			}
		}.to_tokens(&mut get_fields);
		quote!{
			if i < #offset + #ty::WIDTH {
				return #ty::fieldname_ctx(stringify!(#name), i - (#offset))
			}
		}.to_tokens(&mut fieldname_ctx);
		quote!{
			if i < #offset + #ty::WIDTH {
				return #ty::fieldtype(i - (#offset))
			}
		}.to_tokens(&mut fieldtype);
		// this will use the highest-numbered field labeled as "primary" as
		// the primary key:
		if field_info.extra.contains("primary") {
			quote!{ .or(Some(#offset)) }.to_tokens(&mut primary_index);
		}
		let create_text = field_info.extra;
		quote!{
			if i < #offset + #ty::WIDTH {
				return #create_text
			}
		}.to_tokens(&mut field_create_text);
		from_row_at_v.push(quote!{ #ty::from_row_at(r, offset + (#offset))? });
		// we now update `offset` so that it points to the offset at the start
		// of next field:
		quote!{ + #ty::WIDTH }.to_tokens(&mut offset)
	}
	let from_row_at = fields.build_from(&from_row_at_v);
	let code = quote! (impl crate::struct_io::SqlRow for #ident {
		const WIDTH: usize = #offset;
		fn get_field(&self, i: usize)->rusqlite::types::ToSqlOutput {
			#get_fields
			panic!("invalid field {} for resource type {}", i, stringify!(ident))
		} // get_field
		fn from_row_at(r: &rusqlite::Row<'_>, offset: usize)->Result<Self> {
			Ok(#from_row_at)
		}
		fn fieldtype(i: usize)->crate::struct_io::FieldType {
			#fieldtype
			panic!("invalid field {} for resource type {}", i, stringify!(ident))
		}
		fn fieldname_ctx(_ctx: &'static str, i: usize)->&'static str {
			#fieldname_ctx
			panic!("invalid field {} for resource type {}", i, stringify!(ident))
		}
		fn field_create_text(i: usize)->&'static str {
			#field_create_text
			panic!("invalid field {} for resource type {}", i, stringify!(ident))
		}
		fn primary_index()->Option<usize> { #primary_index }
	});
	code.into()
}
/// Top resource
/// 
/// Attributes:
/// resource(items, "itm", 0x03ed)
#[proc_macro_derive(TopResource, attributes(resource))]
pub fn derive_top_resource(tokens: TokenStream)->TokenStream {
	let DeriveInput{ ident, attrs, .. } = parse_macro_input!(tokens);
	let mut resource_info = ResourceInfo::default();
	for (name, mut args) in attrs.iter().map(parse_attr) {
		match name.as_str() {
			"resource" => resource_info = args.top_resource(),
			_ => ()
		} // match
	}
	if resource_info.name.is_empty() || resource_info.extension.is_empty() {
		panic!("top-level resource must have a full `resource` attribute; we got {resource_info:?}")
	}
	let mut code = quote!{
	};
	code.into()
}

#[proc_macro_derive(Resource, attributes(column,resource))]
pub fn derive_resource(tokens: TokenStream) -> TokenStream {
	let mut context = Vec::<(String, syn::Type, String)>::new();
	let mut schema = TS::new();
	let mut params = TS::new();
	let mut build = TS::new();
	let mut ncol = 0usize;
// 	let ty_i64: syn::Type = syn::parse_str("i64").unwrap();
	let add_schema = |fieldname: &str, ty: &syn::Type, extra: &str| {
		quote!{ crate::schemas::Field::new::<#ty>(#fieldname, #extra), }
		}; //.to_tokens(&mut schema);
	let DeriveInput{ ident, data, attrs, generics, .. } = parse_macro_input!(tokens);
	let payload = struct_fields(data);
	let mut resource_info = ResourceInfo::default();
// 	println!("parsing attributes: {:?}", attrs.iter().map(toks_to_string).collect::<Vec<_>>());
	for (name, mut args) in attrs.iter().map(parse_attr) {
		match &name[..] {
			"resource" => table_attr_resource(&mut args, &mut resource_info),
			_ => () };
	}
	// Build the payload part of the schema from the struct fields:
	for Field { attrs, ident, ty, .. } in payload {
		let mut current = ColumnInfo::default();
		for (name, mut args) in attrs.iter().map(parse_attr) {
			match name.as_str() {
				"column" => table_attr_column(&mut context, &mut current, &mut args),
				_ => () }
		}
		if current.no_column {
			quote!{ #ident: #ty::default(), }.to_tokens(&mut build);
			continue
		}
		let fieldname = ident.as_ref().unwrap().to_string();
// 		let fieldtype = toks_to_string(&ty);
		add_schema(fieldname.as_str(), &ty, current.extra.as_str())
			.to_tokens(&mut schema);
		quote!{ self.#ident, }.to_tokens(&mut params);
		quote!{ #ident: row.get::<_,#ty>(#ncol)
			.context(concat!("cannot read field ", stringify!(#ncol)))?, }
			.to_tokens(&mut build);
		ncol+= 1;
	}
	let payload_len = ncol;
	// Build the context part of the schema from the attributes:
	let mut ctx = TS::new();
	let mut build_key = TS::new();
	for (keycol_in, (fieldname, ty, extra)) in context.iter().enumerate() {
		add_schema(fieldname.as_str(), ty, extra.as_str()).to_tokens(&mut schema);
		let kc = pm2::Literal::isize_unsuffixed(keycol_in as isize);
		let sty = toks_to_string(ty);
		quote!{ #ty, }.to_tokens(&mut ctx);
		quote!{ k.#kc, }.to_tokens(&mut params);
		quote!{ row.get::<_,#ty>(#ncol)
			.context(concat!("cannot read field ", stringify!(#ncol), ": ", #fieldname, " as " , #sty))?, }
			.to_tokens(&mut build_key);
		ncol+= 1;
	}
	// Search through foreign key constraints to locate the parent resource:
	let mut parent = resource_info.name.clone();
	let mut parent_key = payload_len;
	let foreign_key = Regex::new(r#"references\s*"?(\w+)"?\s*\("#).unwrap();
	for (i, (_, _, attr)) in context.iter().enumerate() {
		let c = match foreign_key.captures(attr) { None => continue, Some(c) => c };
		// We successfully identified this resource as a sub-resource.
		// We now insert the parent key, parent table, and primary key.
		parent = String::from(c.get(1).unwrap().as_str());
		parent_key = payload_len + i;
		assert!(i == 0);
		break
	}
	if context.len() > 1 {
		// we are dealing with a subresource: insert the "id" key
		quote!{ crate::schemas::Field::new::<i64>("id", "primary key"),
		} .to_tokens(&mut schema);
	}
	let ResourceInfo { name, extension, restype, .. } = resource_info;
	let mut code = quote! {
		impl #generics crate::resources::Resource for #ident #generics {
			type Context = (#ctx);
			const SCHEMA: crate::schemas::Schema<'static> = crate::schemas::Schema{
				name: #name,
				extension: #extension,
				restype: crate::gamefiles::Restype { value: #restype },
				resref_key: #parent_key, parent_table: #parent,
				fields: &[#schema]};
			fn ins(&self, s: &mut crate::rusqlite::Statement, k: &Self::Context)->crate::rusqlite::Result<()> {
				s.execute(rusqlite::params![#params])?; Ok(())
			}
			fn sel(row: &crate::rusqlite::Row)->Result<(Self, Self::Context)> {
				Ok((Self{ #build }, (#build_key)))
			}
		}
	};
// 	println!("\x1b[31m Context for {}: {}\x1b[m",
// 		toks_to_string(&ident), toks_to_string(&ctx));
// 		println!("\x1b[36m{}\x1b[m", code);
	if !name.is_empty() {
		// The type name as a string:
		let type_name = toks_to_string(&ident);
		// which field of `AllResources` we are attached to:
		let field = pm2::Ident::new(&name, pm2::Span::call_site());
		push_resource(ResourceDef(type_name, name));
		quote! {
			impl #generics NamedTable for #ident #generics {
	fn find_field<T: Debug>(all: &AllResources<T>)->&T { &all.#field }
	fn find_field_mut<T: Debug>(all: &mut AllResources<T>)->&mut T { &mut all.#field }
			}
		}.to_tokens(&mut code);
	}
	code.into()
} // Resource
/// `column` attribute:
/// `[column(itemref, i32, "references items", etc.)]` pushes on table
/// `[column(false)]` suppresses next column
fn table_attr_column(fields2: &mut Vec<(String,syn::Type,String)>, current: &mut ColumnInfo, args: &mut AttrParser) {
	use AttrArg::{Ident, Str};
	let a = match args.get::<syn::Expr>() { None=>return, Some(a)=> a};
	match a {
		Str(s) => current.extra = s,
		Ident(i) => {
		if i == "false" { current.no_column = true; return }
		let t = match args.get::<syn::Type>() { Some(AttrArg::Type(t)) => t,
			_ => {
			println!("\x1b[31mdon't know what to do with lone identifier: {i}\x1b[m");
			return } };
		let s = match args.get::<syn::Expr>() { Some(AttrArg::Str(s)) => s,
			_ => String::new() };
		fields2.push((i, t, s));
		},
		_ => ()
	}
}
/// `[resource]` attribute:
/// - if this attribute is not used then this table will not be stored in
///   the global `RESOURCES` constant. This is used for `GameStrings`.
/// - `[resource(items)]` declares the name of a global table
///   and creates an entry for this table in `RESOURCES`.
/// - `[resource(items, "itm", 0x03ed)]` declares a file extension for this
///   resource and an associated magic number in key files.
fn table_attr_resource(args: &mut AttrParser, resource_info: &mut ResourceInfo) {
	let i = match args.get::<syn::Expr>() { Some(AttrArg::Ident(i)) => i,
		Some(AttrArg::Str(s)) => s, _ => return };
	resource_info.name = i;
	let ext = match args.get::<syn::Expr>() { Some(AttrArg::Str(s)) => s,
		_ => return };
	let restype = match args.get::<syn::Expr>() { Some(AttrArg::Int(n)) => n,
		x => { println!("got attribute: {x:?}"); return } };
	resource_info.extension = ext;
	resource_info.restype = restype as u16;
// 	let pk = match args.get::<syn::Expr>() { Some(AttrArg::Ident(i)) => i,
// 		Some(AttrArg::Str(s)) => s, _ => return };
// 	*parent_key = pk;
// 	let pn = match args.get::<syn::Expr>() { Some(AttrArg::Ident(i)) => i,
// 		Some(AttrArg::Str(s)) => s, _ => { *parent = name.clone(); return }};
// 	*parent = pn;
}

#[proc_macro]
pub fn produce_resource_list(_: proc_macro::TokenStream)->proc_macro::TokenStream {
	let mut fields = pm2::TokenStream::new();
	let mut map = pm2::TokenStream::new();
	let mut data = pm2::TokenStream::new();
	let mut table_schema = pm2::TokenStream::new();
	let mut n = 0usize;
	RESOURCES.with(|v| {
		use pm2::{Span,Ident};
		n = v.borrow().len();
	for ResourceDef (type_name, name) in v.borrow().iter() {

		let ty = Ident::new(type_name, Span::call_site());
		let field = Ident::new(name, Span::call_site());
		let q = quote!{ #[allow(missing_docs)] pub #field: T, };
		q.to_tokens(&mut fields);
		quote!{ #field: f(&<#ty as crate::resources::Resource>::SCHEMA,&self.#field)?, }
			.to_tokens(&mut map);
		quote!{ #field: (), }.to_tokens(&mut data);
		quote!{ let sch = &<#ty as crate::resources::Resource>::SCHEMA;
			if s == sch.to_string() { return Ok(sch) } }
			.to_tokens(&mut table_schema);
	}
	});

	let code = quote!{
/// A struct holding one field for each game resource type.
///
/// This is used for iterating similar code for each resources.
		#[derive(Debug)] #[allow(clippy::missing_docs)]
		pub struct AllResources<T: Debug> {
#[allow(clippy::missing_docs)]
			_marker: std::marker::PhantomData<T>, #fields }
/// An heterogeneous iterator over the constant list of all game resource types.
		pub const RESOURCES: AllResources<()> = AllResources {
			_marker: std::marker::PhantomData, #data };
		#[allow(clippy::len_without_is_empty)]
		impl<T: Debug> AllResources<T> {
			/// Number of resources in this table.
			pub fn len(&self)->usize { #n }
			/// Calls a closure for each resource type in the game.
			pub fn map<U: Debug,E,F:Fn(&'static crate::schemas::Schema,&T)->Result<U,E>>(&self, f: F)->Result<AllResources<U>,E> {
				Ok(AllResources { _marker: std::marker::PhantomData, #map })
			}
			/// Calls a closure for each resource type in the game.
			pub fn map_mut<U: Debug,E,F:FnMut(&crate::schemas::Schema,&T)->Result<U,E>>(&self, mut f: F)->Result<AllResources<U>,E> {
				Ok(AllResources { _marker: std::marker::PhantomData, #map })
			}
			/// Given a SQL table name, returns the schema for this table,
			/// or throws an appropriate error.
			pub fn table_schema(&self, s: &str)->Result<&'static crate::schemas::Schema> {
				#table_schema;
				Err(Error::UnknownTable(s.to_owned()).into())
				}
		}
	};
	code.into()
}
