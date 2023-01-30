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
			syn::Lit::Int(s) =>
				AttrArg::Int(toks_to_string(&s).parse::<isize>().unwrap()),
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
		impl crate::gameindex::Pack for #ident {
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
struct RowCurrent {
	extra: String,
	no_column: bool,
}

#[proc_macro_derive(Table, attributes(column,resource))]
pub fn derive_table(tokens: TokenStream) -> TokenStream {
	let mut fields2 = Vec::<(String, syn::Type, String)>::new();
	let mut schema = TS2::new();
	let mut params = TS2::new();
	let mut build = TS2::new();
	let mut ncol = 0usize;
	let mut primary_key = String::from("rowid");
	let ty_i64: syn::Type = syn::parse_str("i64").unwrap();
	let mut add_schema = |fieldname: &str, ty: &syn::Type, extra: &str| {
		quote!{ crate::database::Column {
			fieldname: #fieldname,
			fieldtype: <#ty as crate::database::SqlType>::SQL_TYPE,
			extra: #extra},
		}.to_tokens(&mut schema);
	};
	let DeriveInput{ ident, data, attrs, generics, .. } = parse_macro_input!(tokens);
	let flist = struct_fields(data);
	let mut table_name = String::new();
	let type_name = toks_to_string(&ident);
	let mut parent_key = String::new();
	let mut parent = String::new();
// 	println!("parsing attributes: {:?}", attrs.iter().map(toks_to_string).collect::<Vec<_>>());
	for (name, mut args) in attrs.into_iter().map(parse_attr) {
		match &name[..] {
			"resource" => table_attr_resource(&mut args, &mut table_name, &mut parent,
				&mut parent_key),
			_ => () };
	}
	for Field { attrs, ident, ty, .. } in flist {
		let mut current = RowCurrent::default();
		for (name, mut args) in attrs.into_iter().map(parse_attr) {
			match name.as_str() {
				"column" => table_attr_column(&mut fields2, &mut current, &mut args),
				_ => () }
		}
		if current.no_column {
			quote!{ #ident: #ty::default(), }.to_tokens(&mut build);
			continue
		}
		let fieldname = ident.as_ref().unwrap().to_string();
// 		let fieldtype = toks_to_string(&ty);
		add_schema(fieldname.as_str(), &ty, current.extra.as_str());
		quote!{ self.#ident, }.to_tokens(&mut params);
		quote!{ #ident: row.get::<_,#ty>(#ncol)?, }.to_tokens(&mut build);
		ncol+= 1;
	}
	for (fieldname, _, attr) in &fields2 {
		if attr.contains("primary key") {
			primary_key = fieldname.to_owned(); break
		}
	}
	let re: regex::Regex=Regex::new(r#"references\s*"?(\w+)"?\s*\("#).unwrap();
	if parent.is_empty() {
		for (fieldname, _, attr) in &fields2 {
			let c = match re.captures(attr) { None => continue, Some(c) => c };
			parent = String::from(c.get(1).unwrap().as_str());
			parent_key = String::from(fieldname);
			break
		}
		if parent.is_empty() {
			parent = toks_to_string(&ident);
			parent_key = String::from(&primary_key);
		}
	}
	let mut key_in = TS2::new();
	let mut keycol_in = 0;
	let mut key_out = TS2::new();
	let mut build2 = TS2::new();
	for (fieldname, ty, extra) in fields2 {
		if toks_to_string(&ty) == "auto" {
			// we use i64 since this is the return type of last_insert_rowid()
			add_schema(fieldname.as_str(), &ty_i64, extra.as_str());
			quote!{ crate::rusqlite::types::Null, }.to_tokens(&mut params);
			quote!{ row.get::<_,i64>(#ncol)?, }.to_tokens(&mut build2);
			quote!{ i64, }.to_tokens(&mut key_out);
		} else {
			add_schema(fieldname.as_str(), &ty, extra.as_str());
			let kc = pm2::Literal::isize_unsuffixed(keycol_in);
			quote!{ #ty, }.to_tokens(&mut key_in);
			quote!{ k.#kc, }.to_tokens(&mut params);
			quote!{ #ty, }.to_tokens(&mut key_out);
			quote!{ row.get_unwrap::<_,#ty>(#ncol), }.to_tokens(&mut build2);
			keycol_in+= 1;
		}
		ncol+= 1;
	}
// 	let field = pm2::Ident::new(&table_name, pm2::Span::call_site());
	let code = quote! {
		impl#generics crate::database::Table for #ident#generics {
			type KeyIn = (#key_in);
			type KeyOut = (#key_out);
			type Res = anyhow::Result<(Self, Self::KeyOut)>;
			const SCHEMA: crate::database::Schema<'static> = crate::database::Schema{
				table_name: #table_name, primary_key: #primary_key,
				parent_key: #parent_key, parent_table: #parent,
				fields: &[#schema]};
			fn ins(&self, s: &mut crate::rusqlite::Statement, k: &Self::KeyIn)->crate::rusqlite::Result<()> {
				s.execute(rusqlite::params![#params])?; Ok(())
			}
			fn sel(row: &crate::rusqlite::Row)->crate::rusqlite::Result<(Self, Self::KeyOut)> {
				Ok((Self{ #build }, (#build2)))
			}
		}
	};
	if table_name.is_empty() {
		println!("\x1b[36m{}\x1b[m", code);
	}
	if !table_name.is_empty() {
		push_resource(ResourceDef(type_name, table_name));
	}
	code.into()
} // Table
fn table_attr_column(fields2: &mut Vec<(String,syn::Type,String)>, current: &mut RowCurrent, args: &mut AttrParser) {
	//! Column attribute:
	//! `[column(itemref, i32, "references items", etc.)]` pushes on table
	//! `[column(false)]~ suppresses next column
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
fn table_attr_resource(args: &mut AttrParser, table_name: &mut String, parent: &mut String, parent_key: &mut String) {
	//! `[table]` attribute:
	//! - `[table("")]` prevents storing this table in the global [default]
	//! RESOURCES constant,
	//! - `[table(item_abilities, "itemref", "items")]` stores with a
	//! relation to the parent resource,
	//! - `[table(items, "itemref")]` stores as a parent table (using given
	//! primary key);
	//! - `[table(items)]` tries to guess the link to parent resource
	//! (either use a key reference if there is one, or take this table as
	//! a root resource if not).
	let i = match args.get::<syn::Expr>() { Some(AttrArg::Ident(i)) => i,
		Some(AttrArg::Str(s)) => s, _ => return };
	*table_name = i;
	let pk = match args.get::<syn::Expr>() { Some(AttrArg::Ident(i)) => i,
		Some(AttrArg::Str(s)) => s, _ => return };
	*parent_key = pk;
	let pn = match args.get::<syn::Expr>() { Some(AttrArg::Ident(i)) => i,
		Some(AttrArg::Str(s)) => s, _ => { *parent = table_name.clone(); return }};
	*parent = pn;
}

#[proc_macro]
pub fn produce_resource_list(_: proc_macro::TokenStream)->proc_macro::TokenStream {
	let mut fields = pm2::TokenStream::new();
	let mut map = pm2::TokenStream::new();
	let mut map_mut = pm2::TokenStream::new();
	let mut data = pm2::TokenStream::new();
	let mut n = 0usize;
	RESOURCES.with(|v| {
		use pm2::{Span,Ident};
		n = v.borrow().len();
	for ResourceDef (type_name, table_name) in v.borrow().iter() {

		let ty = Ident::new(type_name, Span::call_site());
		let field = Ident::new(table_name, Span::call_site());
		quote!{ pub #field: T, }.to_tokens(&mut fields);
		quote!{ #field: f(&<#ty as crate::database::Table>::SCHEMA,&self.#field)?, }
			.to_tokens(&mut map);
		quote!{ #field: f(&<#ty as crate::database::Table>::SCHEMA,&self.#field)?, }
			.to_tokens(&mut map_mut);
		quote!{ #field: (), }.to_tokens(&mut data);
	}
	});

	let code = quote!{
/// A struct holding one field for each game resource type.
///
/// This is used for iterating similar code for each resources.
		pub struct AllResources<T> { _marker: std::marker::PhantomData<T>, #fields }
/// An heterogeneous iterator over the constant list of all game resource types.
		pub const RESOURCES: AllResources<()> = AllResources {
			_marker: std::marker::PhantomData::<()>, #data };
		impl<T> AllResources<T> {
			pub fn len(&self)->usize { #n }
			pub fn map<U,E,F:Fn(&crate::database::Schema,&T)->Result<U,E>>(&self, f: F)->Result<AllResources<U>,E> {
				Ok(AllResources { _marker: std::marker::PhantomData::<U>, #map })
			}
			pub fn map_mut<U,E,F:FnMut(&crate::database::Schema,&T)->Result<U,E>>(&self, mut f: F)->Result<AllResources<U>,E> {
				Ok(AllResources { _marker: std::marker::PhantomData::<U>, #map_mut })
			}
		}
	};
	code.into()
}
