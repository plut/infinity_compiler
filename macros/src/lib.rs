#![allow(
	unreachable_code, dead_code,
	unused_variables, unused_imports, unused_macros, unused_parens,
	unused_mut,unused_attributes,unused_assignments,
)]
extern crate proc_macro;
// extern crate proc_macro2;
use proc_macro::TokenStream;
type TS2 = proc_macro2::TokenStream;
use quote::quote;
use quote::ToTokens;
use syn;
use syn::Attribute;
use syn::Data;
use syn::Data::Struct;
use syn::DataStruct;
use syn::DeriveInput;
use syn::Expr;
use syn::ExprLit;
use syn::ExprParen;
use syn::ExprPath;
use syn::ExprTuple;
use syn::Field;
use syn::Fields::Named;
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
		impl resources::Pack for #ident {
			fn unpack(f: &mut impl io::Read)->io::Result<Self> {
				#readf; Ok(Self{ #build })
			}
			fn pack(self, f: &mut impl io::Write)->io::Result<()> {
				#writef; Ok(())
			}
		}
	};
// 	println!("{}", code);
	TokenStream::from(code)
}

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
#[proc_macro_derive(Row, attributes(column,no_column))]
pub fn derive_row(tokens: TokenStream) -> TokenStream {
	let (ident, flist) = read_struct_fields(parse_macro_input!(tokens));
	let mut keyf = TS2::new();
	let mut schema = TS2::new();
	let mut col = 0;
	for Field { attrs, ident, ty, .. } in flist {
		let mut no_column = false;
		for (name, args) in attrs.into_iter().map(parse_attribute) {
			match name.as_str() {
				"column" => row_attr_column(&mut keyf, args.as_slice()),
				"no_column" => no_column = true,
				_ => () }
		}
		if no_column { continue }
		col+= 1;
		let fieldname = proc_macro2::Literal::string(ident.unwrap().to_string().as_str());
		quote!{ Column { fieldname: #fieldname,
			fieldtype: <#ty>::SQL_TYPE, }, }
			.to_tokens(&mut schema);
	}
	let code = quote! {
		impl resources::Row for #ident {
			type Key = (#keyf);
			const SCHEMA: Schema<'static> = Schema { fields: &[#schema] };
			fn bind(&self, s: &mut Statement, k: &Self::Key)->sqlite::Result<()> {
				Ok(())
			}
		}
	};
	println!("{}", code);
// 	TokenStream::new()
	TokenStream::from(code)
}
fn row_attr_column(keyf: &mut TS2, args: &[AttrArg]) {
	if args.is_empty() { return }
	println!("got column with {} fields", args.len());
	match args {
		[ AttrArg::Ident(i), ] =>
		println!("identifier {i}"),
		_ => (),
	}
	for (i, name) in args.into_iter().enumerate() {
		println!("  arg {i} = \x1b[32m{name:?}\x1b[m");
	}
}
fn ty_to_sql(s: &str)->&'static str {
	match s {
		"i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" => "Integer",
		"String" | "str" => "String",
		_ => "null"
	}
}
