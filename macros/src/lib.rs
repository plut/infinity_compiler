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
type TS2 = pm2::TokenStream;
use regex::Regex;
use quote::{quote, ToTokens};
use syn::{self, DeriveInput, parse_macro_input, Attribute};
use syn::{Data::Struct, DataStruct};
use syn::{Expr, ExprLit, ExprPath};
use syn::{Field, Fields::Named};
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
	let mut ts = TS2::new();
	a.to_tokens(&mut ts);
	ts.to_string()
}
fn struct_fields(data: syn::Data) -> Punctuated<Field, Comma> {
	match data {
		Struct(DataStruct{ fields: Named(f), ..}) => f.named,
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
fn parse_attr(Attribute { path, tokens, .. }: Attribute)->(String, AttrParser) {
	(path.get_ident().unwrap().to_string(), tokens.into())
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
// 	fn column(&mut self)->FieldInfo {
// 		let mut field_info = FieldInfo::default();
// 		match self.get::<syn::Expr>() {
// 			None => (),
// 			Some(AttrArg::Str(s)) => field_info.extra = s,
// 			Some(AttrArg::Ident(i)) if i.as_str() == "false" =>
// 				field_info.no_column = true,
// 			Some(a) => panic!("unknown argument for 'column' attribute: {:?}", a),
// 		}
// 		return field_info
// 	}
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
	let DeriveInput{ ident, data, .. } = parse_macro_input!(tokens);
	let flist = struct_fields(data);

	let mut readf = TS2::new();
	let mut build = TS2::new();
	let mut writef = TS2::new();

	for Field { attrs, ident, ty, .. } in flist {
		for (name, mut args) in attrs.into_iter().map(parse_attr) {
			match name.as_str() {
				"header" => pack_attr_header(&mut readf, &mut writef, &mut args),
				&_ => () };
		}
		quote!{ let #ident = #ty::unpack(f)?; }.to_tokens(&mut readf);
		quote!{ #ident, }.to_tokens(&mut build);
		quote!{ self.#ident.pack(f)?; }.to_tokens(&mut writef);
	}
	let code = quote! {
		impl crate::struct_io::Pack for #ident {
			fn unpack(f: &mut impl std::io::Read)->std::io::Result<Self> {
				#readf; Ok(Self{ #build })
			}
			fn pack(&self, f: &mut impl std::io::Write)->std::io::Result<()> {
				#writef; Ok(())
			}
		}
	};
// 	println!("{}", code);
	code.into()
} // Pack
fn pack_attr_header(readf: &mut TS2, writef: &mut TS2, args: &mut AttrParser) {
	if let Some(AttrArg::Str(s)) = args.get::<syn::Expr>() {
		quote!{ Self::unpack_header(f, #s)?; }.to_tokens(readf);
		quote!{ f.write_all(#s.as_bytes())?; }.to_tokens(writef);
	}
}

#[derive(Default,Debug)]
struct FieldInfo {
	extra: String,
	no_column: bool,
}
#[derive(Default,Debug)]
struct ResourceInfo {
	name: String,
	extension: String,
	restype: u16,
}
#[proc_macro_derive(SqlMapped)]
pub fn derive_sql_row(tokens: TokenStream)->TokenStream {
	let mut get_fields = TS2::new();
	let mut from_row_at = TS2::new();
	let mut offset = TS2::from(quote!(0usize));
	let DeriveInput{ ident, data, .. } = parse_macro_input!(tokens);
	for Field {  ident, ty, .. } in struct_fields(data) {
		// Read attributes for this field
// 		let mut field_info = FieldInfo::default();
// 		for (name, mut args) in attrs.into_iter().map(parse_attr) {
// 		} // for
		// before generating the code, `offset` contains the offset at the
		// start of this field:
		quote!{
			if i < #offset + #ty::WIDTH {
				return self.#ident.get_field(i - (#offset))
			}
		}.to_tokens(&mut get_fields);
		quote!{
			#ident: #ty::from_row_at(r, offset + (#offset))?,
		}.to_tokens(&mut from_row_at);
		// we now update `offset` so that it points to the offset at the start
		// of next field:
		quote!{ + #ty::WIDTH }.to_tokens(&mut offset)
	}
	let code = quote! (impl crate::struct_io::SqlMapped for #ident {
		const WIDTH: usize = #offset;
		fn get_field(&self, i: usize)->rusqlite::types::ToSqlOutput {
			#get_fields
			panic!("invalid field {} for resource type {}", i, stringify!(ident))
		} // get_field
		fn from_row_at(r: &rusqlite::Row<'_>, offset: usize)->Result<Self> {
			Ok(Self{ #from_row_at })
		}
	});
	code.into()
}

#[proc_macro_derive(Resource, attributes(column,resource))]
pub fn derive_resource(tokens: TokenStream) -> TokenStream {
	let mut context = Vec::<(String, syn::Type, String)>::new();
	let mut schema = TS2::new();
	let mut params = TS2::new();
	let mut build = TS2::new();
	let mut get_fields = TS2::new();
	let mut ncol = 0usize;
// 	let ty_i64: syn::Type = syn::parse_str("i64").unwrap();
	let add_schema = |fieldname: &str, ty: &syn::Type, extra: &str| {
		quote!{ crate::schemas::Field::new::<#ty>(#fieldname, #extra), }
		}; //.to_tokens(&mut schema);
	let DeriveInput{ ident, data, attrs, generics, .. } = parse_macro_input!(tokens);
	let payload = struct_fields(data);
	let mut resource_info = ResourceInfo::default();
// 	println!("parsing attributes: {:?}", attrs.iter().map(toks_to_string).collect::<Vec<_>>());
	for (name, mut args) in attrs.into_iter().map(parse_attr) {
		match &name[..] {
			"resource" => table_attr_resource(&mut args, &mut resource_info),
			_ => () };
	}
	// Build the payload part of the schema from the struct fields:
	for Field { attrs, ident, ty, .. } in payload {
		let mut current = FieldInfo::default();
		for (name, mut args) in attrs.into_iter().map(parse_attr) {
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
		quote!{ #ncol => self.#ident.to_sql()
			.expect(concat!("could not convert value to SQL: ", stringify!(#ident))),
			}
			.to_tokens(&mut get_fields);
		quote!{ #ident: row.get::<_,#ty>(#ncol)
			.context(concat!("cannot read field ", stringify!(#ncol)))?, }
			.to_tokens(&mut build);
		ncol+= 1;
	}
	let payload_len = ncol;
	// Build the context part of the schema from the attributes:
	let mut ctx = TS2::new();
	let mut build_key = TS2::new();
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
			fn get_field(&self, i: usize)->rusqlite::types::ToSqlOutput {
				match i { #get_fields
					_ => panic!("invalid field {} for resource type {}", i, stringify!(ident))
				}
			}
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
fn table_attr_column(fields2: &mut Vec<(String,syn::Type,String)>, current: &mut FieldInfo, args: &mut AttrParser) {
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
