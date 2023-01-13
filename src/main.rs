#![allow(
	unused_imports,
	unused_macros,
	unused_variables, dead_code,
	unused_must_use,
	unused_mut,
	)]

use std::any::type_name;
fn type_of<T>(_:&T)->&'static str { type_name::<T>() }
use std::path::Path;
use std::fs::File;
use std::io;
use std::fmt;
use std::cmp::min;
use macros::{Pack, Row};
extern crate sqlite;
use sqlite::{Connection,Statement};

mod resources {
use std::io;
use std::io::Read;
use std::io::Write;
use std::marker::Sized;
use std::iter::{IntoIterator,Iterator};
use sqlite::Statement;
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
	pub extra: &'a str,
}
#[derive(Debug)]
pub struct Schema<'a> {
	pub fields: &'a[Column<'a>], // fat pointer
// 	pub fields: &'a[&'a str], // fat pointer
}

impl<'a> Schema<'a> {
	fn write_columns(&self, s: &mut String) {
		let mut isfirst = true;
		for Column { fieldname, .. } in self.fields.iter() {
			if isfirst { isfirst = false; } else { s.push_str(","); }
			s.push_str("\n \""); s.push_str(fieldname); s.push_str("\" ");
		}
	}
	pub fn create_statement(&self, name: &str)->String {
		use std::fmt::Write;
		let mut s = String::from(format!("create table \"{name}\" ("));
		let mut isfirst = true;
		for Column { fieldname, fieldtype, extra } in self.fields.iter() {
			if isfirst { isfirst = false; }
			else { s.push_str(","); }// XXX use format_args! instead
			write!(&mut s, "\n \"{fieldname}\" {} {extra}", fieldtype.sql()).unwrap();
		}
		s.push_str(")");
		s
	}
	pub fn insert_statement(&self, name: &str)->String {
		let mut s = String::from(format!("insert into \"{name}\" ("));
		self.write_columns(&mut s);
		s.push_str(") values (");
		for c in 0..self.fields.iter().count() {
			if c > 0 { s.push_str(","); }
			s.push_str("?");
		}
		s.push_str(")");
		s
	}
	pub fn select_statement(&self, name: &str)->String {
		use std::fmt::Write;
		let mut s = String::from("select ");
		self.write_columns(&mut s);
		write!(&mut s, "\nfrom \"{name}\" ").unwrap();
		s
	}
}
pub trait Pack: Sized {
	fn unpack(f: &mut impl Read)->io::Result<Self>;
	fn pack(self, f: &mut impl io::Write)->io::Result<()> { unimplemented!() }

	// associated functions:
	fn read_bytes(f: &mut impl Read, n: usize)->io::Result<Vec<u8>> {
		let mut buf = Vec::<u8>::with_capacity(n);
		unsafe { buf.set_len(n); }
		f.read(&mut buf)?; Ok(buf)
	}
	fn unpack_header(f: &mut impl Read, hdr: &str)->io::Result<()> {
		let buf = Self::read_bytes(f, hdr.len() as usize);
// 		println!(" found {buf:?}; is equal to hdr? {}", buf == hdr.as_bytes());
		Ok(())
	}
	fn vecunpack(mut f: &mut impl Read, n: usize)->io::Result<Vec<Self>> {
		(0..n).map(|_| { Self::unpack(&mut f) }).collect()
	}
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
} // mod resources
mod gametypes {
extern crate sqlite;
use macros::{Pack, Row};
use super::resources;
use resources::{Pack, Row};
use sqlite::Statement;
use std::io;
#[derive(Debug)] pub struct Resref { pub name: super::StaticString::<8>, }
impl Pack for Resref {
	fn unpack(f: &mut impl Read)->io::Result<Self> {
		let mut name = super::StaticString::<8>::unpack(f)?;
		name.bytes.make_ascii_lowercase();
		Ok(Self { name, })
	}
}
#[derive(Debug,Pack,Clone,Copy)] pub struct Strref { pub value: i32, }
#[derive(Debug,Pack,Clone,Copy,PartialEq)] pub struct Restype { pub value: u16, }
#[derive(Debug,Pack,Clone,Copy)] pub struct BifIndex { pub data: u32, }
impl BifIndex {
	fn sourcefile(&self)->usize { (self.data >> 20) as usize }
	fn resourceindex(&self)->usize { (self.data & 0x3fff) as usize }
	fn tilesetindex(&self)->usize { ((self.data >> 14) & 0x3f) as usize }
}
#[derive(Debug,Pack)] struct KeyHdr {
	#[header("KEY V1  ")]
	nbif: i32,
	nres: i32,
	bifoffset: u32,
	resoffset: u32,
}
#[derive(Debug,Pack)] struct KeyBif {
	filelength: u32,
	offset: u32,
	namelength: u16,
	location: u16,
}
#[derive(Debug,Pack)] pub struct KeyRes {
	resref: Resref,
	restype: Restype,
	location: BifIndex,
}
#[derive(Debug)] pub struct GameIndex<'a> {
	gamedir: &'a str,
	pub bifnames: Vec<String>,
	bifsizes: Vec<u32>,
	pub resources: Vec<KeyRes>,
}

use std::path::Path;
use std::fs::File;
use std::io::{Read, Seek, SeekFrom};
fn gameindex_from<'a>(gamedir: &'a str)->io::Result<GameIndex<'a>> {
	let gameindex = Path::new(&gamedir).join("chitin.key");
	let mut f = File::open(gameindex)?;
	let hdr = KeyHdr::unpack(&mut f)?;
	println!("{hdr:?}");
	let bifentries = KeyBif::vecunpack(&mut f, hdr.nbif as usize)?;
	let mut bifnames = Vec::<String>::new();
	let mut bifsizes = Vec::<u32>::new();
	for KeyBif{offset, namelength, filelength, ..} in bifentries {
		f.seek(SeekFrom::Start(offset as u64))?;
		let buf = KeyBif::read_bytes(&mut f, namelength as usize - 1)?;
		bifnames.push(String::from_utf8(buf).unwrap());
		bifsizes.push(filelength);
		let l = bifnames.len();
	};
	f.seek(SeekFrom::Start(hdr.resoffset as u64))?;
	let resources = <KeyRes>::vecunpack(&mut f, hdr.nres as usize)?;
	Ok(GameIndex{ gamedir, bifnames, resources, bifsizes })
}
impl<'a> From<&'a str> for GameIndex<'a> {
	fn from(dir: &'a str)->Self { gameindex_from(dir).unwrap() }
}
#[derive(Debug)] pub struct GameIndexItr<'a> {
	index: &'a GameIndex<'a>,
	state: usize,
}
#[derive(Debug)] pub struct BifView<'a> { // element of GameIndex
	pub path: std::path::PathBuf,
	pub sourcefile: usize,
	pub resources: &'a Vec<KeyRes>,
	pub file: Option<std::io::BufReader<std::fs::File>>,
}
impl<'a> IntoIterator for &'a GameIndex<'a> {
	type Item = BifView<'a>;
	type IntoIter = GameIndexItr<'a>;
	fn into_iter(self)->Self::IntoIter { GameIndexItr{ index: self, state: 0, } }
}
impl<'a> Iterator for GameIndexItr<'a> {
	type Item = BifView<'a>;
	fn next(&mut self)->Option<Self::Item> {
		if self.state >= self.index.bifnames.len() { return None }
		let filename = &self.index.bifnames[self.state];
		let r = BifView{ path: Path::new(self.index.gamedir).join(filename),
			sourcefile: self.state, resources: &self.index.resources, file: None };
		self.state+= 1;
		Some(r)
	}
}
#[derive(Debug)] pub struct BifViewItr<'a> {
	pub bifview: &'a BifView<'a>,
	pub state: usize,
}
impl<'a> BifView<'a> {
	fn open(&mut self)->&mut impl Read {
		if self.file.is_none() {
			self.file = Some(std::io::BufReader::new(File::open(&self.path).unwrap()));
		}
		self.file.as_mut().unwrap()
	}
}
#[derive(Debug)] pub struct ResItem<'a> {
	pub restype: Restype,
	pub resref: &'a Resref,
	pub index: usize,
	pub bif: &'a BifView<'a>,
}
impl<'a> IntoIterator for &'a BifView<'a> {
	type Item = ResItem<'a>;
	type IntoIter = BifViewItr<'a>;
	fn into_iter(self)->Self::IntoIter { BifViewItr{ bifview: self, state: 0, } }
}
#[derive(Debug,Pack)] pub struct BifHdr {
	#[header("BIFFV1  ")]
	nres: u32,
	ntilesets: u32,
	offset: u32,
}
#[derive(Debug,Pack)] pub struct BifResource {
	locator: BifIndex,
	offset: u32,
	size: u32,
	restype: Restype,
	_unknown: u16,
}
fn biffile_from<'a>(file: impl AsRef<Path>+std::fmt::Debug)->io::Result<Vec<BifResource>> {
	let mut f = File::open(file)?;
	let hdr = BifHdr::unpack(&mut f)?;
// 	println!("bif hdr for {file:?}: {hdr:?}");
	f.seek(SeekFrom::Start(hdr.offset as u64))?;
	BifResource::vecunpack(&mut f, hdr.nres as usize)
}
impl<'a> Iterator for BifViewItr<'a> {
	type Item = ResItem<'a>;
	fn next(&mut self)->Option<Self::Item> {
		loop {
			// iterate (self.state) over the *global* set of resources
			// (self.bifview.resources):
			if self.state >= self.bifview.resources.len() { return None }
			let r = &self.bifview.resources[self.state];
			self.state+= 1;
			// filter out all resources not belonging to current file:
			if r.location.sourcefile() != self.bifview.sourcefile { continue }

// 			if self.resources.is_none() {
// 				println!("\x1b[31;1mOpening file {}\x1b[m", self.bifview.filename.to_str().unwrap());
// 				self.resources = Some(biffile_from(&self.bifview.filename).unwrap());
// 				println!("this file has {} resources", self.resources.as_ref().unwrap().len());
// 			}
			let index = r.location.resourceindex();
			return Some(ResItem { restype: r.restype, resref: &r.resref, index,
				bif: &self.bifview })
		}
	}
}
impl<'a> ResItem<'a> {
	fn to_cursor(&self) {
	}
}
} // mod gametypes

use resources::{Schema, Row, Pack};
use gametypes::{Strref, Resref};

// #[derive(Debug)]
pub struct StaticString<const N: usize>{ bytes: [u8; N], }
impl<const N: usize> PartialEq<&str> for StaticString<N> {
	fn eq(&self, other: &&str) -> bool {
		for (i, c) in other.bytes().enumerate() {
			if i >= N { return false }
			if !c.is_ascii() { return false }
			if self.bytes[i] != c { return false }
			if c == 0u8 { return true }
		}
		true
	}
}
impl<const N: usize> PartialEq<StaticString<N>> for StaticString<N> {
	fn eq(&self, other: &StaticString<N>)->bool { self.bytes == other.bytes }
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
impl<const N: usize> std::fmt::Debug for StaticString<N> {
	fn fmt(&self, f:&mut fmt::Formatter) -> fmt::Result {
		std::fmt::Display::fmt(self, f)
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
struct Blah {
	#[header("KEY V1  ")]
	nbif: i32,
#[column("primary key")]
#[column(itemref, i32, "")]
	nres: i32,
#[column(false)]
	bifoffset: u32,
	resoffset: u32,
}
use resources::{FieldType, Column, ToBindable};

const DB_FILE: &str = "game.sqlite";

fn create_db(db_file: &str)->io::Result<Connection> {
	if std::path::Path::new(&db_file).exists() {
		std::fs::remove_file(&db_file)?;
	}
	let db = sqlite::open(&db_file).unwrap();
	Ok(db)
}

fn main() -> io::Result<()> {
	let gamepath = Path::new("/home/jerome/jeux/bg/game");
	let index = gamepath.join("chitin.key");
	let game = gametypes::GameIndex::from("/home/jerome/jeux/bg/game");
	for bif in &game {
// 		println!("in file {:?} {}", a.filename, type_of(&a.filename));
		for r in &bif {
			if r.restype.value == 0x03ed {
				println!("found an item {} in {:?} ", r.resref.name, bif.path);
			}
// 				println!("{r:?}");
		}
	}

	create_db(&DB_FILE)?;
	println!("{}", Blah::SCHEMA.create_statement("hdr"));
	println!("{}", Blah::SCHEMA.insert_statement("hdr"));
	println!("{}", Blah::SCHEMA.select_statement("hdr"));

	Ok(())
}
