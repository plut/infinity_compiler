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
fn type_of<T>(_:T)->&'static str { type_name::<T>() }

// macro_rules! dump {
// 	($($x: expr),*) => {
// 		panic!(concat!("dump:\n", $(stringify!($x), "={:#?}\n",)*), $($x,)* );
// 	}
// }

#[proc_macro_derive(Pack, attributes(header))]
pub fn derive_pack(tokens: TokenStream) -> TokenStream {
	let DeriveInput{ ident, data, .. } = parse_macro_input!(tokens);
	let mut flist = match data {
		Struct(DataStruct{ fields: Named(f), ..}) => f.named,
		_ => panic!("only struct with named fields!") };
// 	println!("\x1b[35m{}\x1b[m", toks_to_string(&ident));
	let (readf, build, writef) = unpack_code(&flist);
	let code = quote! {
		impl resources::Pack for #ident {
			fn unpack(f: &mut impl io::Read)->io::Result<Self> {
				#readf; Ok(Self{ #build })
			}
			fn pack(self, f: &mut impl io::Write)->io::Result<()> {
				#writef; Ok(())
			}
		}
// 		impl resources::Row for #ident {
// 			type Key = f64;
// 			const SCHEMA: Schema<'static> = Schema { fields: &[] };
// 		}
	};
	println!("{}", code.to_string());
	TokenStream::from(code)
}

fn toks_to_string(a: &impl quote::ToTokens) -> String {
	let mut ts = TS2::new();
	a.to_tokens(&mut ts);
	ts.to_string()
}
fn unpack_code(flist: &Punctuated<Field,Comma>) -> (TS2, TS2, TS2) {
	let mut readf = TS2::new();
	let mut build = TS2::new();
	let mut writef = TS2::new();
	for f in flist {
		let Field { attrs, ident, ty, .. } = f;
// 		println!("\x1b[32m{}\x1b[m : \x1b[31m{}\x1b[m", toks_to_string(&ident),
// 			toks_to_string(&ty));
		for a in attrs {
// 			println!("\x1b[35mParsing attribute: {}\x1b[m", toks_to_string(&a));
			let Attribute { path, tokens, .. } = a;
			let t = TokenStream::from(tokens.clone());
			let args = Punctuated::<Expr, Token![,]>::parse_terminated
				.parse(t).unwrap();
			let name = path.get_ident().unwrap().to_string();
			match name.as_str() {
				"header" => append_header(&mut readf, &mut writef, args),
				&_ => panic!("unknown attribute"),
			};
// 			println!("\x1b[32m{:?}\x1b[m: \x1b[31m{:?}\x1b[m", ident, ty);
		}
		quote!{ let #ident = #ty::unpack(f)?; }.to_tokens(&mut readf);
		quote!{ #ident, }.to_tokens(&mut build);
		quote!{ self.#ident.pack(f)?; }.to_tokens(&mut writef);
	}
	(readf, build, writef)
}
fn append_header(readf: &mut TS2, writef: &mut TS2,
		args: Punctuated<Expr,Comma>) {
	if args.len() != 1 { return }
	let expr = match args.first().unwrap() {
		Expr::Paren(ExprParen{ expr, ..}) => expr, _ => return };
	let lit = match &**expr { Expr::Lit(ExprLit{lit, ..}) => lit, _ => return };
	quote!{ Self::unpack_header(f, #lit)?; }.to_tokens(readf);
	quote!{ f.write_all(#lit.as_bytes())?; }.to_tokens(writef);
}
