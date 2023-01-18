#![allow(
// 	unreachable_code,
	dead_code,
// 	unused_variables,
	unused_imports,
// 	unused_macros,
// 	unused_parens,
// 	unused_mut,
// 	unused_attributes,
// 	unused_assignments,
)]
extern crate proc_macro;
// extern crate proc_macro2;
use proc_macro::TokenStream;
use proc_macro2 as pm2;
type TS2 = pm2::TokenStream;
type Ident2 = pm2::Ident;
type Type2 = pm2::Ident;
use pm2::Span;
use quote::quote;
use quote::ToTokens;
use syn;
use syn::Attribute;
use syn::Data;
use syn::Data::Struct;
use syn::DataStruct;
use syn::DeriveInput;
use syn::Expr;
use syn::{ExprLit, ExprParen, ExprPath, ExprTuple};
use syn::Field;
use syn::Fields::{Named, Unnamed};
use syn::Path;
use syn::Token;
use syn::Type;
use syn::parse::Parser;
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::parse;
use syn::parse_macro_input;

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
fn read_struct_fields(d: syn::DeriveInput) -> (syn::Ident, Punctuated<Field, Comma>) {
	let DeriveInput{ ident, data, .. } = d; // parse_macro_input!(tokens);
	let flist = match data {
		Struct(DataStruct{ fields: Named(f), ..}) => f.named,
// 		Struct(DataStruct{ fields: Unnamed(f), .. }) => f.unnamed,
		_ => panic!("only struct with named fields!") };
	(ident, flist)
}
fn to_vector(itr: &Punctuated<Expr,Comma>)->Vec<Expr> {
	let mut v = Vec::<Expr>::new();
	for e in itr.iter() {
		v.push(e.clone());
	}
	return v
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
fn parse_attribute(Attribute{path, tokens, ..}: Attribute) -> (String, Vec<AttrArg>) {
	(path.get_ident().unwrap().to_string(), parse_attribute_args(tokens))
}

fn split_stream(stream: pm2::TokenStream)->Vec<pm2::TokenStream> {
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

fn parse_attribute_args(tokens: TS2)->Vec<AttrArg> {
	// TODO: make this generic, by
	// 1. turning into an iterator,
	// 2. making the return type into a parameter
	let mut r = Vec::<AttrArg>::new();
	let stream = match tokens.into_iter().next() {
		None => { return r },
		Some(pm2::TokenTree::Group(g)) => g.stream(),
		_ => unimplemented!()
	};
	let mut iter = split_stream(stream).into_iter();

	let a = match iter.next() { None=>return r, Some(a)=>a };
	let b: syn::Expr = syn::parse2(a).expect("an expression");
	r.push(AttrArg::from(b));

	// 2nd argument is a type:
	let a = match iter.next() { None=>return r, Some(a)=>a };
	let b: syn::Type = syn::parse2(a).expect("a type");
	r.push(AttrArg::Type(b));

	// 3rd argument is an expression:
	let a = match iter.next() { None=>return r, Some(a)=>a };
	let b: syn::Expr = syn::parse2(a).expect("an expression");
	r.push(AttrArg::from(b));

	return r
}

#[proc_macro_derive(Pack, attributes(header))]
pub fn derive_pack(tokens: TokenStream) -> TokenStream {
	let (ident, flist) = read_struct_fields(parse_macro_input!(tokens));

	let mut readf = TS2::new();
	let mut build = TS2::new();
	let mut writef = TS2::new();

	for Field { attrs, ident, ty, .. } in flist {
		for (name, args) in attrs.into_iter().map(parse_attribute) {
			match name.as_str() {
				"header" => pack_attr_header(&mut readf, &mut writef, args),
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

fn pack_attr_header(readf: &mut TS2, writef: &mut TS2, args: Vec<AttrArg>) {
	match &args[..] {
		[ AttrArg::Str(s) ] => {
	quote!{ Self::unpack_header(f, #s)?; }.to_tokens(readf);
	quote!{ f.write_all(#s.as_bytes())?; }.to_tokens(writef);
		},
		_ => ()
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
	use pm2::Literal;
	let (ident, flist) = read_struct_fields(parse_macro_input!(tokens));
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
		for (name, args) in attrs.into_iter().map(parse_attribute) {
			match name.as_str() {
				"column" => row_attr_column(&mut fields2, &mut current, &args[..]),
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
			const SCHEMA: crate::database::Schema<'static> =
				crate::database::Schema { fields: &[#schema] };
				fn execute(&self, s: &mut crate::rusqlite::Statement, k: &Self::KeyIn)->crate::rusqlite::Result<()> {
					s.execute(rusqlite::params![#params])?; Ok(())
				}
				fn read(row: &crate::rusqlite::Row)->crate::rusqlite::Result<(Self, Self::KeyOut)> {
					Ok((Self{ #build }, (#build2)))
				}
		}
	};
// 	println!("\x1b[36m{}\x1b[m", code);
	TokenStream::from(code)
} // Table
fn row_attr_column(fields2: &mut Vec<(String,syn::Type,String)>, current: &mut RowCurrent, args: &[AttrArg]) {
	use AttrArg::{Ident, Str, Type};
// 	println!("got column with {} fields: {args:?}", args.len());
// 	for (i, name) in args.iter().enumerate() {
// 		println!("  arg {i} = \x1b[32m{name:?}\x1b[m");
// 	}
	match args {
		[ Ident(i), ] =>
			if i == "false" { current.no_column = true; }
			else {
				println!("\x1b[31mdon't know what to do with identifier: {i}\x1b[m");
			},
		[ Str(s), ] => current.extra = s.to_owned(),
		[ Ident(i), Type(t), Str(s) ] =>
			fields2.push((i.to_owned(), t.clone(), s.to_owned())),
		[ Ident(i), Type(t) ] =>
			fields2.push((i.to_owned(), t.clone(), "".to_owned())),
		_ => (),
	}
}
// fn ty_to_sql(s: &str)->&'static str {
// 	match s {
// 		"i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" => "Integer",
// 		"String" | "str" => "String",
// 		_ => "null"
// 	}
// }
