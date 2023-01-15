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
type TS2 = proc_macro2::TokenStream;
use proc_macro2::Span;
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
	(path.get_ident().unwrap().to_string(),
	parse_attribute_args(tokens).into_iter().map(AttrArg::from).collect())
}
fn parse_attribute_args(tokens: TS2)->Vec<Expr> {
	let args = Punctuated::<Expr, Token![,]>::parse_terminated
		.parse(TokenStream::from(tokens)).unwrap();
	if args.len() == 0 { return Vec::<Expr>::new() }
	if args.len() > 1 { panic!("args expression should have length 0 or 1"); }
	match args.first().unwrap() {
		Expr::Paren(ExprParen{ expr, ..}) => vec![*expr.clone()],
		Expr::Tuple(ExprTuple{ elems, .. }) => to_vector(elems),
		_ => panic!("unknown expression") }
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
		impl crate::resources::Pack for #ident {
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

#[proc_macro_derive(Row, attributes(column,no_column))]
pub fn derive_row(tokens: TokenStream) -> TokenStream {
	use proc_macro2::Literal;
	let (ident, flist) = read_struct_fields(parse_macro_input!(tokens));
	let mut fields2 = Vec::<(String, String, String)>::new();
	let mut col: usize = 0;
	let mut schema = TS2::new();
	let mut params = TS2::new();
	let mut add_schema = |fieldname: &str, fieldtype: &str, extra: &str| {
		let ty = proc_macro2::Ident::new(fieldtype, Span::call_site());
		quote!{ crate::resources::Column {
			fieldname: #fieldname,
			fieldtype: <#ty as crate::resources::SqlType>::SQL_TYPE,
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
		if current.no_column { continue }
		col+= 1;
		let fieldname = ident.as_ref().unwrap().to_string();
		let fieldtype = toks_to_string(&ty);
		add_schema(fieldname.as_str(), fieldtype.as_str(), current.extra.as_str());
		quote!{ self.#ident, }.to_tokens(&mut params);
	}
	let mut keyf = TS2::new();
	let keycol = 0;
	for (fieldname, fieldtype, extra) in fields2 {
		col+= 1;
		let kc = proc_macro2::Literal::isize_unsuffixed(keycol);
		let ty = proc_macro2::Ident::new(fieldtype.as_str(), Span::call_site());
		add_schema(fieldname.as_str(), fieldtype.as_str(), extra.as_str());
		quote!{ #ty, }.to_tokens(&mut keyf);
		quote!{ k.#kc, }.to_tokens(&mut params);
	}
	let code = quote! {
		impl crate::resources::Row for #ident {
			type Key = (#keyf);
			const SCHEMA: crate::resources::Schema<'static> =
				crate::resources::Schema { fields: &[#schema] };
				fn execute(&self, s: &mut crate::rusqlite::Statement, k: &Self::Key) {
					s.execute(rusqlite::params![#params]).unwrap();
				}
// 			fn bind(&self, s: &mut sqlite::Statement, k: &Self::Key)->sqlite::Result<()> {
// 				use crate::resources::ToBindable;
// 				#bind; Ok(())
// 			}
		}
	};
// 	println!("\x1b[36m{}\x1b[m", code);
	TokenStream::from(code)
} // Row
fn row_attr_column(fields2: &mut Vec<(String,String,String)>, current: &mut RowCurrent, args: &[AttrArg]) {
	use AttrArg::{Ident, Str};
	println!("got column with {} fields: {args:?}", args.len());
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
		[ Ident(i), Ident(t), Str(s) ] =>
			fields2.push((i.to_owned(), t.to_owned(), s.to_owned())),
		[ Ident(i), Ident(t) ] =>
			fields2.push((i.to_owned(), t.to_owned(), "".to_owned())),
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
