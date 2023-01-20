#![allow(
// 	unreachable_code,
// 	dead_code,
// 	unused_variables,
// 	unused_imports,
// 	unused_macros,
// 	unused_parens,
// 	unused_mut,
// 	unused_attributes,
// 	unused_assignments,
)]
use proc_macro::TokenStream;
use proc_macro2 as pm2;
type TS2 = pm2::TokenStream;
use quote::{quote, ToTokens};
use syn::{self, DeriveInput, parse_macro_input, Attribute};
use syn::{Data::Struct, DataStruct};
use syn::{Expr, ExprLit, ExprPath};
use syn::{Field, Fields::Named};
use syn::{punctuated::Punctuated, token::Comma};

struct ResourceDef {
	name: String,
	ty: String,
	dirtykey: String,
	dirtyparent: String,
}
thread_local! {
	static RESOURCES: std::cell::RefCell<Vec<ResourceDef>> =
		std::cell::RefCell::new(Vec::<ResourceDef>::new());
}
fn push_resource(rdef: ResourceDef) {
	RESOURCES.with(|v| { v.borrow_mut().push(rdef); })
}

use std::any::type_name;
fn type_of<T>(_:&T)->&'static str { type_name::<T>() }

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
fn read_struct_fields(d: syn::DeriveInput) -> (syn::Ident, Punctuated<Field, Comma>, Vec<syn::Attribute>) {
	let DeriveInput{ ident, data, attrs, .. } = d; // parse_macro_input!(tokens);
	let flist = match data {
		Struct(DataStruct{ fields: Named(f), ..}) => f.named,
// 		Struct(DataStruct{ fields: Unnamed(f), .. }) => f.unnamed,
		_ => panic!("only struct with named fields!") };
	(ident, flist, attrs)
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
		match syn::parse2::<T>(a) { Err(_) => return None, Ok(b)=>Some(b.into()) }
// 	let b: T = syn::parse2(a).expect("cannot parse!");
// 	Some(b.into())
	}
}
impl From<pm2::TokenStream> for AttrParser {
	fn from(tokens: pm2::TokenStream)->Self {
			Self(match tokens.into_iter().next() {
			None => { Vec::<pm2::TokenStream>::new().into_iter() },
			Some(pm2::TokenTree::Group(g)) => Self::split(g.stream()).into_iter(),
			_ => unimplemented!()
		})
	}
}

#[proc_macro_derive(Pack, attributes(header))]
pub fn derive_pack(tokens: TokenStream) -> TokenStream {
	let (ident, flist, _) = read_struct_fields(parse_macro_input!(tokens));

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
			fn pack(self, f: &mut impl std::io::Write)->std::io::Result<()> {
				#writef; Ok(())
			}
		}
	};
// 	println!("{}", code);
	TokenStream::from(code)
} // Pack

fn pack_attr_header(readf: &mut TS2, writef: &mut TS2, args: &mut AttrParser) {
	if let Some(AttrArg::Str(s)) = args.get::<syn::Expr>() {
		quote!{ Self::unpack_header(f, #s)?; }.to_tokens(readf);
		quote!{ f.write_all(#s.as_bytes())?; }.to_tokens(writef);
	}
}

// [column(itemref, i32, "references items", etc.)] pushes on key
// [column(false)] suppresses next column
#[derive(Default,Debug)]
struct RowCurrent {
	extra: String,
	no_column: bool,
}

#[proc_macro_derive(Table, attributes(column,no_column))]
pub fn derive_row(tokens: TokenStream) -> TokenStream {
	let (ident, flist, attrs) = read_struct_fields(parse_macro_input!(tokens));
	let mut fields2 = Vec::<(String, syn::Type, String)>::new();
	let mut schema = TS2::new();
	let mut params = TS2::new();
	let mut build = TS2::new();
	let mut ncol = 0usize;
	let ty_i64: syn::Type = syn::parse_str("i64").unwrap();
	let mut add_schema = |fieldname: &str, ty: &syn::Type, extra: &str| {
		quote!{ crate::database::Column {
			fieldname: #fieldname,
			fieldtype: <#ty as crate::database::SqlType>::SQL_TYPE,
			extra: #extra},
		}.to_tokens(&mut schema);
	};
	for Field { attrs, ident, ty, .. } in flist {
		let mut current = RowCurrent::default();
		for (name, mut args) in attrs.into_iter().map(parse_attr) {
			match name.as_str() {
				"column" => table_attr_column(&mut fields2, &mut current, &mut args),
// 				"no_column" => no_column = true,
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
	let code = quote! {
		impl crate::database::Table for #ident {
			type KeyIn = (#key_in);
			type KeyOut = (#key_out);
			type Res = anyhow::Result<(Self, Self::KeyOut)>;
			const SCHEMA: crate::database::Schema<'static> =
				crate::database::Schema { fields: &[#schema] };
			fn insert(&self, s: &mut crate::rusqlite::Statement, k: &Self::KeyIn)->crate::rusqlite::Result<()> {
				s.execute(rusqlite::params![#params])?; Ok(())
			}
			fn select(row: &crate::rusqlite::Row)->crate::rusqlite::Result<(Self, Self::KeyOut)> {
				Ok((Self{ #build }, (#build2)))
			}
// 			fn find_statement_mut(s: &mut TypedStatements)->&mut Statement {
// 				s.#table_name
// 			}
// 			fn find_statement(s: &TypedStatements)->&Statement {
// 				s.#table_name
// 			}
		}
	};
// 	println!("\x1b[36m{}\x1b[m", code);
	TokenStream::from(code)
} // Table
fn table_attr_column(fields2: &mut Vec<(String,syn::Type,String)>, current: &mut RowCurrent, args: &mut AttrParser) {
	use AttrArg::{Ident, Str, Type};
	let a = match args.get::<syn::Expr>() { None=>return, Some(a)=> a};
	match a {
		Str(s) => current.extra = s.to_owned(),
		Ident(i) => {
		if i == "false" { current.no_column = true; return }
		let t = match args.get::<syn::Type>() { Some(AttrArg::Type(t)) => t,
			_ => {
			println!("\x1b[31mdon't know what to do with lone identifier: {i}\x1b[m");
			return } };
		let s = match args.get::<syn::Expr>() { Some(AttrArg::Str(s)) => s,
			_ => String::new() };
		fields2.push((i.to_owned(), t.clone(), s.to_owned()));
		},
		_ => ()
	}
// 	println!("got column with {} fields: {args:?}", args.len());
// 	for (i, name) in args.iter().enumerate() {
// 		println!("  arg {i} = \x1b[32m{name:?}\x1b[m");
// 	}
// 	match args {
// 		[ Ident(i), Type(t), Str(s) ] =>
// 			fields2.push((i.to_owned(), t.clone(), s.to_owned())),
// 		[ Ident(i), Type(t) ] =>
// 			fields2.push((i.to_owned(), t.clone(), "".to_owned())),
// 		_ => (),
// 	}
}
// fn ty_to_sql(s: &str)->&'static str {
// 	match s {
// 		"i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" => "Integer",
// 		"String" | "str" => "String",
// 		_ => "null"
// 	}
// }
