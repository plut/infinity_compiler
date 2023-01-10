#![allow(
	unreachable_code, dead_code,
	unused_variables, unused_imports, unused_macros, unused_parens,
	unused_mut,unused_attributes,
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
use syn::ExprParen;
use syn::ExprLit;
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

fn read_struct_fields(d: syn::DeriveInput)
		-> (syn::Ident, Punctuated<Field, Comma>) {
	let DeriveInput{ ident, data, .. } = d; // parse_macro_input!(tokens);
	let flist = match data {
		Struct(DataStruct{ fields: Named(f), ..}) => f.named,
		_ => panic!("only struct with named fields!") };
	(ident, flist)
}

#[proc_macro_derive(Pack, attributes(header))]
pub fn derive_pack(tokens: TokenStream) -> TokenStream {
	let (ident, flist) = read_struct_fields(parse_macro_input!(tokens));

	let mut readf = TS2::new();
	let mut build = TS2::new();
	let mut writef = TS2::new();

	for Field { attrs, ident, ty, .. } in flist {
// 		println!("\x1b[32m{}\x1b[m : \x1b[31m{}\x1b[m", toks_to_string(&ident),
// 			toks_to_string(&ty));
		for Attribute { path, tokens, .. } in attrs {
// 			println!("\x1b[35mParsing attribute: {}\x1b[m", toks_to_string(&a));
			let name = path.get_ident().unwrap().to_string();
			let args = Punctuated::<Expr, Token![,]>::parse_terminated
				.parse(TokenStream::from(tokens)).unwrap();
			let expr = args_expr(&args);
			match name.as_str() {
				"header" => pack_attr_header(&mut readf, &mut writef, expr),
				&_ => () };
// 			println!("\x1b[32m{:?}\x1b[m: \x1b[31m{:?}\x1b[m", ident, ty);
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
	println!("{}", code);
	TokenStream::from(code)
}
fn toks_to_string(a: &impl quote::ToTokens) -> String {
	let mut ts = TS2::new();
	a.to_tokens(&mut ts);
	ts.to_string()
}

fn pack_attr_header(readf: &mut TS2, writef: &mut TS2, expr: &Expr) {
	let expr = match expr {
		Expr::Paren(ExprParen{ expr, ..}) => expr, _ => return };
	let lit = match &**expr { Expr::Lit(ExprLit{lit, ..}) => lit, _ => return };
	quote!{ Self::unpack_header(f, #lit)?; }.to_tokens(readf);
	quote!{ f.write_all(#lit.as_bytes())?; }.to_tokens(writef);
}

// [column(itemref, i32, "references items", etc.)] pushes on key
// [column(false)] suppresses next column
#[proc_macro_derive(Row, attributes(column))]
pub fn derive_row(tokens: TokenStream) -> TokenStream {
	let (ident, flist) = read_struct_fields(parse_macro_input!(tokens));
	for Field { attrs, ident, ty, .. } in flist {
		for Attribute { path, tokens, .. } in attrs {
			let name = path.get_ident().unwrap().to_string();
			let args = Punctuated::<Expr, Token![,]>::parse_terminated
				.parse(TokenStream::from(tokens)).unwrap();
			println!("\x1b[34m{name}\x1b[m");
			println!("\x1b[32m{}\x1b[m:", toks_to_string(&args));
			println!("{args:?}");
			match name.as_str() {
				"column" => row_attr_column(args),
				_ => () }
		}
	}
	let code = quote! {
		impl resources::Row for #ident {
			type Key = f64;
			const SCHEMA: Schema<'static> = Schema { fields: &[] };
		}
	};
	println!("{}", code);
	TokenStream::from(code)
}

fn args_expr(args: &Punctuated<Expr, Comma>)->&Expr {
	if args.len() != 1 { panic!("args expression should have length 1"); }
	args.first().unwrap()
}
fn row_attr_column(args: Punctuated<Expr, Comma>) {
	let a = args_expr(&args);
	println!("\x1b[1m***{}***\x1b[m {:?}: {}", 1, a, 1);
}
