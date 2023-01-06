#![allow(unused_imports)]
use std::path::Path;
use std::fs::File;
use std::io;
use std::io::Read;
use std::fmt;
use std::fmt::Write;
use std::fmt::Display;
// extern crate byteorder;
// use byteorder::BigEndian;
// == Pack
pub trait Pack {
	fn unpack(f: &mut impl Read)->io::Result<Self> where Self: std::marker::Sized;
}
macro_rules! unpack_int {
	($($T:ty,)*) => { $(impl Pack for $T {
			fn unpack(f: &mut impl Read)->io::Result<Self> {
				let mut buf = [0u8; std::mem::size_of::<$T>()];
				f.read_exact(&mut buf)?;
				Ok(<$T>::from_le_bytes(buf))
			}
	})* }
}
unpack_int!{i8,i16,i32,i64,u8,u16,u32,u64,}

#[derive(Debug)]
struct Bytes8 {
	bytes: [u8; 8],
}
impl std::fmt::Display for Bytes8 {
#[allow(unused_must_use)]
	fn fmt(&self, f:&mut fmt::Formatter) -> fmt::Result {
		f.write_char('"')?;
		for c in &self.bytes {
			*c == 0u8 && break;
			f.write_char(*c as char)?;
		}
		f.write_char('"')?;
		Ok(())
	}
}
impl Pack for Bytes8 {
	fn unpack(f: &mut impl Read)->io::Result<Self> {
		let mut x = [0u8; 8];
		f.read_exact(&mut x)?;
		Ok(Self{ bytes: x, })
	}
}

macro_rules! unpack {
	($(struct $name:ident { $($fn:ident: $ft:ty,)* })*) => {
		$(#[derive(Debug)]
		struct $name { $($fn: $ft,)* }
		impl Pack for $name {
			fn unpack(mut f: &mut impl Read)->io::Result<Self> {
				Ok(Self{$($fn: <$ft>::unpack(&mut f)?,)* })
			}
		}
		)*
	}
}

unpack!{ struct KeyHdr {
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
	Ok(())
}
