#![allow(unused_imports)]
#![allow(dead_code)]
use std::path::Path;
use std::fs::File;
use std::io;
use std::io::Read;
use std::fmt;
use std::fmt::Write;
use resources::Unpack;

mod pack;
use pack::Pack;

mod res {
use std::io;
use io::Result;
use std::io::Read;
use std::io::Write;
use std::marker::Sized;
use sqlite::Statement;
struct Resref { name: i64, }
struct Strref { value: i32, }
#[derive(Debug)]
enum FieldType { Integer, String, Resref, Strref }
#[derive(Debug)]
struct Column<'a> {
	fieldname: &'a str,
	fieldtype: FieldType,
}
#[derive(Debug)]
pub struct Schema<'a> {
// 	fields: &'a[Column<'a>], // fat pointer
	pub fields: &'a[&'a str], // fat pointer
}

// pub trait Pack: Sized {
// 	fn unpack(f: &mut impl Read)->Result<Self>;
// // 	fn pack(&self, f: &mut impl Write)->Result<()>;
// }
// 

pub trait Row {
	type Key;
	fn schema()->Schema<'static>;
// 	fn bind(&self, s: &mut Statement<'_>, k: &Self::Key) -> Result<()>;
// 	fn read(s: &mut Statement<'_>)->Result<(Self, Self::Key)>
// 		where Self: Sized;
}
}

#[derive(Debug)]
struct StaticString<const N: usize>{
	bytes: [u8; N],
}

use res::Row;
use res::Schema;

impl<const N: usize> res::Row for StaticString<N> {
	type Key = ();
	fn schema()->Schema<'static> {
		Schema{ fields: &["ab", "cd", "ef"] }
	}
}
impl<const N: usize> std::fmt::Display for StaticString<N> {
#[allow(unused_must_use)]
	fn fmt(&self, f:&mut fmt::Formatter) -> fmt::Result {
		std::fmt::Write::write_char(f, '"')?;
		for c in &self.bytes {
			*c == 0u8 && break;
			f.write_char(*c as char)?;
		}
		f.write_char('"')?;
		Ok(())
	}
}

impl<const N: usize> pack::Pack for StaticString<N> {
	fn unpack(f: &mut impl Read)->io::Result<Self> {
		let mut x = [0u8; N];
		f.read_exact(&mut x)?;
		Ok(Self{ bytes: x, })
	}
}

/* macro must define the following:
 * - Pack and unpack methods
 * - a schema value (const if possible)
 * - bind to SQL statement (insert)
 * - take from SQL statement (select)
 *
 * XXX use derive(Pack) and derive(bind)
 */

pack::unpack!{ struct KeyHdr {
	constant: StaticString<8>,
	nbif: i32,
	nres: i32,
	bifoffset: u32,
	resoffset: u32,
} }

// macro_rules! test {
// 	($a:ty) => { impl $a {
// 		const fn schema()->&'static str { concat!("abcd", "xyz") }
// 	} }
// }
// test!{Bytes8}

use std::string::ToString;
use std::iter::Iterator;
fn foo(a: Vec<i32>)->String { format!("{:?}", a) }
fn foo2(a: &Vec<i32>)->String {
	a.into_iter()
	.map(std::string::ToString::to_string)
	.collect::<Vec<String>>()
	.join(":")
}

#[derive(Unpack)]
struct Blah {
	#[ign]
	x: f64,
}


fn main() -> io::Result<()> {
	Blah::say_hello();
	println!("Hello, world!");
	println!("{}", 2.to_string());
	println!("{}", foo2(&vec![1,2,3]));
	let gamepath = Path::new("/home/jerome/jeux/bg/game");
	let index = gamepath.join("chitin.key");
	println!("game index is {:#?}", index);
	let mut f = File::open(index).expect("file not found");
	let hdr = KeyHdr::unpack(&mut f)?;
	println!("read succeeded buf: {:?}", hdr);
// 	println!("schema is {}", Bytes8::schema());
	Ok(())
}
