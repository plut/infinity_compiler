#![allow(unused_imports)]
extern crate proc_macro;
use std::path::Path;
use std::fs::File;
use std::io;
use std::io::Read;
use std::fmt;
use std::fmt::Write;

mod pack;
use pack::Pack;

// extern crate byteorder;
// use byteorder::BigEndian;
// == Pack
#[derive(Debug)]
struct Bytes8 {
	bytes: [u8; 8],
}
impl std::fmt::Display for Bytes8 {
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

impl pack::Pack for Bytes8 {
	fn unpack(f: &mut impl Read)->io::Result<Self> {
		let mut x = [0u8; 8];
		f.read_exact(&mut x)?;
		Ok(Self{ bytes: x, })
	}
}

macro_rules! test {
	($a:ty) => { impl $a {
		const fn schema()->&'static str { concat!("abcd", "xyz") }
	} }
}
test!{Bytes8}

pack::unpack!{ struct KeyHdr {
	constant: Bytes8,
	nbif: i32,
	nres: i32,
	bifoffset: u32,
	resoffset: u32,
} }

fn main() -> io::Result<()> {
	let gamepath = Path::new("/home/jerome/jeux/bg/game");
	let index = gamepath.join("chitin.key");
	println!("game index is {:#?}", index);
	let mut f = File::open(index).expect("file not found");
	let hdr = KeyHdr::unpack(&mut f)?;
	println!("read succeeded buf: {:?}", hdr);
	println!("schema is {}", Bytes8::schema());
	Ok(())
}
