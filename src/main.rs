#![allow(
	unused_imports,
	unused_macros,
	unused_variables, dead_code,
	unused_mut,
	)]
use std::path::Path;
use std::fs::File;
use std::io;
use std::fmt;
use std::cmp::min;
use macros::{Pack, Row};
extern crate sqlite;
use sqlite::Statement;

mod resources {
use std::io;
use std::io::Read;
use std::io::Write;
use std::marker::Sized;
use sqlite::Statement;
pub struct Resref { pub name: i64, }
pub struct Strref { pub value: i32, }
#[derive(Debug,Clone,Copy)]
pub enum FieldType { Integer, String, Resref, Strref, Other }
impl FieldType {
	pub const fn sql(self)->&'static str {
		match self {
			FieldType::Integer | FieldType::Strref => "integer",
			FieldType::String | FieldType::Resref => "text",
			FieldType::Other => "null",
		}
	}
}
#[derive(Debug)]
pub struct Column<'a> {
	pub fieldname: &'a str,
	pub fieldtype: FieldType,
}
#[derive(Debug)]
pub struct Schema<'a> {
	pub fields: &'a[Column<'a>], // fat pointer
// 	pub fields: &'a[&'a str], // fat pointer
}
trait AddColumns { fn add_columns(&mut self, r:&Schema)->&Self; }
impl AddColumns for String {
	fn add_columns(&mut self, r: &Schema)->&Self {
		let mut isfirst = true;
		for Column { fieldname, .. } in r.fields.iter() {
			if isfirst { isfirst = false; } else { self.push_str(","); }
// 			write!(self, "\n \"{fieldname}\" ");
		}
		self
	}
}

impl<'a> Schema<'a> {
	pub fn create(&self, name: &str)->String {
		use std::fmt::Write;
		let mut s = String::from(format!("create table \"{name}\" ("));
		let mut isfirst = true;
		for Column { fieldname, fieldtype } in self.fields.iter() {
			if isfirst { isfirst = false; }
			else { s.push_str(","); }// XXX use format_args! instead
			write!(&mut s, "\n \"{fieldname}\" {}", fieldtype.sql()).unwrap();
		}
		s.push_str(");");
		s
	}
	fn add_columns(&mut self, s: String)->String {
		let mut isfirst = true;
		for Column { fieldname, .. } in self.fields.iter() {
// 			if isfirst { isfirst = false; } else { s.push_str(","); }
// 			s.push_str("\n \""); s.push_str(fieldname); s.push_str("\" ");
		}
		s
	}
	pub fn insert(&self, name: &str)->String {
		let mut s = String::from("insert into \"");
		s.push_str(name);
		s.push_str("\" (");
// 		let s = self.add_columns(s);
		s
	}
}
pub trait Pack: Sized {
	fn unpack(f: &mut impl Read)->io::Result<Self>;
	fn pack(self, f: &mut impl io::Write)->io::Result<()> { unimplemented!() }

	fn unpack_header(f: &mut impl io::Read, hdr: &str)->io::Result<()> {
		let mut buf = Vec::<u8>::with_capacity(hdr.len());
		unsafe { buf.set_len(hdr.len()); }
		f.read_exact(&mut buf)?;
		println!(" found {buf:?}; is equal to hdr? {}", buf == hdr.as_bytes());
		Ok(())
	}
// 	fn pack(&self, f: &mut impl Write)->Result<()>;
}
macro_rules! unpack_int {
	($($T:ty),*) => { $(impl Pack for $T {
			fn unpack(f: &mut impl Read)->io::Result<Self> {
				let mut buf = [0u8; std::mem::size_of::<$T>()];
				f.read_exact(&mut buf)?;
				Ok(<$T>::from_le_bytes(buf))
			}
			fn pack(self, f: &mut impl Write)->io::Result<()> {
				f.write_all(&self.to_le_bytes())
			}
	})* }
}
unpack_int!{i8,i16,i32,i64,u8,u16,u32,u64}
pub trait ToBindable {
	type SQLType: sqlite::BindableWithIndex;
	const SQL_TYPE: FieldType;
	fn to_bindable(&self)->Self::SQLType;
}
macro_rules! bind_int {
	($($T:ty),*) => { $(impl ToBindable for $T {
		type SQLType = i64;
		const SQL_TYPE: FieldType = FieldType::Integer;
		fn to_bindable(&self)->Self::SQLType { *self as i64 }
	})* }
}
bind_int!{i8,i16,i32,u8,u16,u32}

pub trait Row {
	type Key;
	const SCHEMA: Schema<'static>;
// 	fn schema()->Schema<'static>;
	fn bind(&self, s: &mut Statement, k: &Self::Key) -> sqlite::Result<()>;
// 	fn read(s: &mut Statement<'_>)->Result<(Self, Self::Key)>
// 		where Self: Sized;
}
}

use resources::Row;
use resources::Schema;
use resources::Pack;
use resources::Strref;
use resources::Resref;

#[derive(Debug)]
struct StaticString<const N: usize>{ bytes: [u8; N], }
impl<const N: usize> PartialEq<&str> for StaticString<N> {
	fn eq(&self, other: &&str) -> bool {
		for (i, c) in other.chars().enumerate() {
			if i >= N { return false }
			if !c.is_ascii() { return false }
			if self.bytes[i] != (c as u8) { return false }
			if c == '\0' { return true }
		}
		true
	}
}
impl<const N: usize> From<&str> for StaticString<N> {
// 	let s = StaticString::<8>::from("ABC");
	fn from(s: &str) -> Self {
		let s_bytes = s.as_bytes();
		let mut bytes = [0u8; N];
		for i in 0..min(s_bytes.len(), N-1) { bytes[i] = s_bytes[i]; }
		Self { bytes: bytes, }
	}
}
impl<const N: usize> std::fmt::Display for StaticString<N> {
	fn fmt(&self, f:&mut fmt::Formatter) -> fmt::Result {
		use std::fmt::Write;
		f.write_char('"')?;
		for c in &self.bytes {
			if *c == 0u8 { break }
			f.write_char(*c as char)?;
		}
		f.write_char('"')?;
		f.write_fmt(format_args!("{}", N))?;
		Ok(())
	}
}
impl<const N: usize> resources::Pack for StaticString<N> {
	fn unpack(f: &mut impl io::Read)->io::Result<Self> {
		let mut x = [0u8; N];
		f.read_exact(&mut x)?;
		Ok(Self{ bytes: x, })
	}
	fn pack(self, f: &mut impl io::Write)->io::Result<()> {
		f.write_all(&self.bytes)
	}
}
impl<const N: usize> Default for StaticString<N> {
	fn default()->Self { Self { bytes: [0u8; N] } }
}

#[derive(Default, Debug, Pack, Row)]
struct KeyHdr {
	#[header("KEY V1  ")]
	nbif: i32,
#[column(false, "toto", 3, abcd)]
#[column(itemref)]
	nres: i32,
#[no_column]
	bifoffset: u32,
	resoffset: u32,
}
use resources::{FieldType, Column, ToBindable};

// impl resources::Row for KeyHdr {
// 	type Key = (i32,);
// 	const SCHEMA: Schema<'static> = Schema { fields: &[
// 		Column{ fieldname: "nbif", fieldtype: ToBindable<i32>::SQL_TYPE, },
// 		Column{ fieldname: "nres", fieldtype: FieldType::Integer, },
// 	] };
// 	fn bind(&self, s: &mut Statement, k: &Self::Key)->sqlite::Result<()> {
// 		s.bind((1, self.nbif.to_bindable()))?;
// 		s.bind((2, self.nres.to_bindable()))?;
// 		Ok(())
// 	}
// }

// use std::string::ToString;
// use std::iter::Iterator;
// fn foo(a: Vec<i32>)->String { format!("{:?}", a) }
// fn foo2(a: &Vec<i32>)->String {
// 	a.into_iter()
// 	.map(std::string::ToString::to_string)
// 	.collect::<Vec<String>>()
// 	.join(":")
// }

fn main() -> io::Result<()> {
// 	println!("s = {}", s);
// 	println!("s == \"ABC\": {}, s==\"ABCDEFGHI\": {}", s == "ABC", s == "ABCDEFGHI");
// 	println!("Hello, world!");
// 	println!("{}", foo2(&vec![1,2,3]));
	let gamepath = Path::new("/home/jerome/jeux/bg/game");
	let index = gamepath.join("chitin.key");
	println!("game index is {:#?}", index);
	let mut f = File::open(index).expect("file not found");
	let blah = KeyHdr::unpack(&mut f)?;
	println!("{:?}", KeyHdr::SCHEMA);
	println!("{}", KeyHdr::SCHEMA.create("foo"));
	println!("read Blah succeeded: {blah:?}");
	
	let mut f = File::create("a")?;
	blah.pack(&mut f)?;
// 	let hdr = KeyHdr::unpack(&mut f)?;
// 	println!("read succeeded buf: {:?}", hdr);
// 	println!("schema is {}", Bytes8::schema());
	Ok(())
}
