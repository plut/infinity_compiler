//! Compiler between IE game files and a SQLite database.
#![allow(
	unused_attributes,
// 	unused_imports,
// 	dead_code,
// 	unreachable_code,
// 	unused_macros,
// 	unused_variables,
// 	unused_must_use,
// 	unused_mut,
)]
#![warn(
	explicit_outlives_requirements,
// 	single_use_lifetimes,
	unused_lifetimes,
	trivial_casts,
	trivial_numeric_casts,
	keyword_idents,
	unused_crate_dependencies,
	unused_qualifications,
	variant_size_differences,
	noop_method_call,
	missing_copy_implementations,
	missing_debug_implementations,
	elided_lifetimes_in_paths,
	missing_docs,
)]
// #![feature(trace_macros)]
// trace_macros!(false);
const BACKUP_DIR: &str = "simod-backup";

pub(crate) mod prelude {
//! The set of symbols we want accessible from everywhere in the crate.
pub(crate) use anyhow::{Result,Context};
#[allow(unused_imports)]
pub(crate) use log::{trace,debug,info,warn,error};
pub(crate) use rusqlite::{self,Connection,Statement};

#[allow(unused_imports)]
pub(crate) use std::fmt::{self,Display,Debug,Formatter};
pub(crate) use std::fs::{self,File};
#[allow(unused_imports)]
pub(crate) use std::io::{self,Cursor,Read,Write,Seek,SeekFrom};
pub(crate) use std::path::{Path, PathBuf};

pub(crate) use crate::gamefiles::{Resref,Strref};
pub(crate) use macros::{Pack,Table};
}
pub(crate) mod gamefiles {
//! Access to the KEY/BIF side of the database.
//!
//! Main interface:
//!  - [`Pack`] trait: defines binary I/O for game structures;
//!  - [`Resref`], [`Strref`]: indices used in game structures;
//!  - [`GameIndex`] type and iterator: abstraction used for reading game files.
//!
//! A lot of structs in this file (e.g. [`KeyHdr`], [`BifIndex`]) are
//! exact mirrors of entries in game files and left undocumented.
use crate::prelude::*;
use rusqlite::types::{FromSql,ToSql, ValueRef};

use std::cmp::min;
use std::io::{self, BufReader};

use crate::progress::{Progress};
use crate::database::{SqlType,FieldType};

// I. Basic types: StaticString, Resref, Strref etc.
#[derive(Clone,Copy)]
/// A fixed-length string.
///
/// This is *slightly* different from (TODO) standard implementations in
/// that (a) no ending zero is necessary (although one can be present,
/// thus shortening the string), and (b) these strings (used for indexing
/// game resources) are case-insensitive. (Since the game uses mostly
/// uppercase, we convert on purpose to lowercase: this facilitates
/// spotting bugs).
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
	fn from(s: &str) -> Self { Self::from(s.as_bytes()) }
}
impl<const N: usize> From<&[u8]> for StaticString<N> {
	fn from(s: &[u8])->Self {
		let mut bytes = [0u8; N];
		let n = min(s.len(), N);
		bytes[..n].copy_from_slice(&s[..n]);
		Self { bytes, }
	}
}
impl<const N: usize> AsRef<str> for StaticString<N> {
	fn as_ref(&self)->&str {
		let r = self.bytes.iter().enumerate().find_map(|(i, c)| {
			if *c == 0 { Some(i) } else { None } });
		let n = r.unwrap_or(N);
		std::str::from_utf8(&self.bytes[..n]).unwrap()
	}
}
impl<const N: usize> Debug for StaticString<N> {
	fn fmt(&self, f:&mut Formatter<'_>) -> fmt::Result {
		use std::fmt::Write;
		f.write_char('"')?;
		for c in &self.bytes {
			if *c == 0u8 { break }
			f.write_char(*c as char)?;
		}
		f.write_char('"')?;
		f.write_fmt(format_args!("{N}"))?;
		Ok(())
	}
}
impl<const N: usize> Display for StaticString<N> {
	fn fmt(&self, f:&mut Formatter<'_>) -> fmt::Result {
		Display::fmt(&self.as_ref(), f)
	}
}
impl<const N: usize> Pack for StaticString<N> {
	fn unpack(f: &mut impl Read)->io::Result<Self> {
		let mut x = [0u8; N];
		f.read_exact(&mut x)?;
		Ok(Self{ bytes: x, })
	}
	fn pack(&self, f: &mut impl Write)->io::Result<()> {
		f.write_all(&self.bytes)
	}
}
impl<const N: usize> Default for StaticString<N> {
	fn default()->Self { Self { bytes: [0u8; N] } }
}

/// Binary I/O for game structures; implemented by derive macro.
///
/// We have a default implementation for fixed-width
/// integer types (little-endian) and for strings (they are ignored on
/// output and empty on input). For composite types, the implementation
/// is produces by the `[Pack]` derive macro.
pub trait Pack: Sized {
	/// Reads this object from a binary source.
	fn unpack(f: &mut impl Read)->io::Result<Self>;
	/// Writes this objects to a binary sink.
	fn pack(&self, _f: &mut impl Write)->io::Result<()> {
		error!("Missing Pack for type: {}", crate::type_of(&self));
		unimplemented!() }
	/// Reads a vector of objects (of known size) from a binary source.
	fn vecunpack(mut f: &mut impl Read, n: usize)->io::Result<Vec<Self>> {
		(0..n).map(|_| { Self::unpack(&mut f) }).collect()
	}

	// associated functions:
	/// Helper function used to check forced headers.
	fn read_bytes(f: &mut impl Read, n: usize)->io::Result<Vec<u8>> {
		let mut buf = vec![0u8; n];
// 		let mut buf = Vec::<u8>::with_capacity(n);
// 		unsafe { buf.set_len(n); }
		f.read_exact(&mut buf)?; Ok(buf)
	}

	/// function which checks that a header is correct.
	fn unpack_header(f: &mut impl Read, hdr: &str)->io::Result<()> {
		let buf = Self::read_bytes(f, hdr.len())?;
		assert_eq!(&buf[..], hdr.as_bytes());
		Ok(())
	}
}
macro_rules! pack_tuple {
	( )=> { };
	($A:ident$(,$B:ident)*$(,)?) => {
		impl<A: Pack,$($B: Pack),*> Pack for (A,$($B,)*) {
			fn unpack(f: &mut impl Read)->io::Result<Self> {
				Ok((A::unpack(f)?, $($B::unpack(f)?,)*))
			}
			fn pack(&self, f: &mut impl Write)->io::Result<()> {
				#[allow(non_snake_case)]
				let (A, $($B,)*) = self;
				A.pack(f)?; $($B.pack(f)?;)* Ok(())
			}
		}
		pack_tuple!{$($B),*} // recursive call for tail of tuple
	};
}
pack_tuple!(A,B,C,D,E,F);
macro_rules! unpack_int {
	// this would be better with a trait, but to_le_bytes (etc.) is not
	// part of a trait (even in crate `num`).
	($($T:ty),*) => { $(impl Pack for $T {
			fn unpack(f: &mut impl Read)->io::Result<Self> {
				let mut buf = [0u8; std::mem::size_of::<$T>()];
				f.read_exact(&mut buf)?;
				Ok(<$T>::from_le_bytes(buf))
			}
			fn pack(&self, f: &mut impl Write)->io::Result<()> {
				f.write_all(&self.to_le_bytes())
			}
	})* }
}
unpack_int!(i8,i16,i32,i64,u8,u16,u32,u64);
impl Pack for String { // String is ignored on pack/unpack:
	fn unpack(_f: &mut impl Read)->io::Result<Self> { Ok(Self::new()) }
	fn pack(&self, _f: &mut impl Write)->io::Result<()> { Ok(()) }
}

/// A resource reference as used by the game: case-insensitive 8-bytes
/// ascii string.
///
/// (Note that, since `Pack` does not know how to work with
/// unnamed structs, we use a named struct here).
#[derive(Clone,Copy,Default)] pub struct Resref { pub name: StaticString::<8>, }
impl Debug for Resref {
	fn fmt(&self, f:&mut Formatter<'_>)->fmt::Result { Debug::fmt(&self.name, f) }
}
impl Display for Resref {
	fn fmt(&self, f:&mut Formatter<'_>)->fmt::Result {Display::fmt(&self.name, f)}
}
impl Pack for Resref {
	fn unpack(f: &mut impl Read)->io::Result<Self> {
		let mut name = StaticString::<8>::unpack(f)?;
		name.bytes.make_ascii_lowercase();
		Ok(Self { name, })
	}
	fn pack(&self, f: &mut impl Write)->io::Result<()> { self.name.pack(f) }
}
impl SqlType for Resref {
	const SQL_TYPE: FieldType = crate::database::FieldType::Resref;
}
impl ToSql for Resref {
	fn to_sql(&self)->rusqlite::Result<rusqlite::types::ToSqlOutput<'_>> { self.name.as_ref().to_sql() }
}
impl FromSql for Resref {
	fn column_result(v: ValueRef<'_>)->rusqlite::types::FromSqlResult<Self> {
		match v {
			ValueRef::Text(s) => Ok(Resref { name: s.into() }),
			ValueRef::Null => Ok(Resref { name: "".into() }),
			_ => Err(rusqlite::types::FromSqlError::InvalidType)
		}
	}
}
impl Resref {
	// iterates until `test` returns FALSE
	pub fn fresh(source: &str, mut is_used: impl FnMut(&Resref)->rusqlite::Result<bool> )->rusqlite::Result<Self> {
// 		let mut n = std::cmp::min(source.len(), 8)-1;
		let mut l = 0;
		let mut buf = Self { name: StaticString { bytes: [0u8; 8] } };
		let mut n = 0;
		// truncate to 8 bytes, keeping only allowed characters, and lowercase
		// (note: this might be slightly too restrictive — e.g. parentheses
		// are likely allowed)
		for c in source.as_bytes() {
			if c.is_ascii_alphanumeric() || "!#@-_".as_bytes().contains(c) {
				buf.name.bytes[n] = c.to_ascii_lowercase();
				n+= 1;
				if n >= 8 { break }
			}
		}
		n-= 1;
		for j in 1..111_111_111 {
			// This panics after all 111_111_111 possibilities have been
			// exhausted. Unlikely to happen irl (and then we cannot do much
			// useful either).
			// The last number written at any length is always (9*);
			// we detect this to increase the length.
			let mut s = j+888_888_888;
			let mut i = n;
			let mut is_nines = true;
			for _ in 0..l {
				let c = (s % 10) as u8;
				s/= 10;
				is_nines = is_nines && (c == 9);
				buf.name.bytes[i] = 48u8 + c;
				i-= 1;
			}
			if !is_used(&buf)? { return Ok(buf) }
			if is_nines { if n < 7 { n+= 1; } l+= 1; }
		}
		panic!("Iteration exhausted");
	}
}
/// A string reference as used by the game: 32-bit integer.
#[derive(Debug,Pack,Clone,Copy)] pub struct Strref { value: i32, }
impl Display for Strref {
	fn fmt(&self, mut f: &mut Formatter<'_>)->std::result::Result<(),std::fmt::Error>{
		use std::fmt::Write;
		write!(&mut f, "@{}", self.value)
	}
}
impl SqlType for Strref {
	const SQL_TYPE: FieldType = FieldType::Strref;
}
impl ToSql for Strref {
	fn to_sql(&self)->rusqlite::Result<rusqlite::types::ToSqlOutput<'_>> { self.value.to_sql() }
}
impl FromSql for Strref {
	fn column_result(v: ValueRef<'_>)->rusqlite::types::FromSqlResult<Self> {
		i32::column_result(v).map(|x| Strref { value: x })
	}
}
#[derive(Debug,Pack,Clone,Copy,PartialEq,Eq)] pub struct Restype { pub value: u16, }
#[derive(Debug,Pack,Clone,Copy)] pub struct BifIndex { data: u32, }
/// A reference to a resource inside a BIF file as encoded in
/// `chitin.key`.
impl BifIndex {
	fn sourcefile(&self)->usize { (self.data >> 20) as usize }
	fn resourceindex(&self)->usize { (self.data & 0x3fff) as usize }
#[allow(dead_code)]
	fn tilesetindex(&self)->usize { ((self.data >> 14) & 0x3f) as usize }
}

// II. Key/Bif indexing:
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
#[derive(Debug,Pack)] struct KeyRes {
	resref: Resref,
	restype: Restype,
	location: BifIndex,
}
#[derive(Debug,Pack)] struct BifHdr {
	#[header("BIFFV1  ")]
	nres: u32,
	ntilesets: u32,
	offset: u32,
}
#[derive(Debug,Pack)] struct BifResource {
	locator: BifIndex,
	offset: u32,
	size: u32,
	restype: Restype,
	_unknown: u16,
}

// IV. Game index main structure:
/// The main structure accessing game files.
#[derive(Debug)] pub struct GameIndex {
	/// The root directory (containing "chitin.key").
	pub root: PathBuf,
	/// The names of the BIF files, as found in chitin.key.
	bifnames: Vec<String>,
	_bifsizes: Vec<u32>,
	resources: Vec<KeyRes>,
	/// The set of languages in the file, as a list of (language code, path
	/// to dialog.tlk).
	pub languages: Vec<(String,PathBuf)>,
	/// The backup directory (full path) for this tool.
	pub backup: PathBuf,
}
impl GameIndex {
	/// Initializes the structure from the path containing "chitin.key".
	pub fn open(gamedir: impl AsRef<Path>)->Result<Self> {
		let gamedir = gamedir.as_ref().to_path_buf();
		let indexfile = Path::new(&gamedir).join("chitin.key");
		let mut f = File::open(&indexfile)
			.with_context(|| format!("cannot open game index: {indexfile:?}"))?;
		let hdr = KeyHdr::unpack(&mut f)
			.with_context(|| format!("bad KEY header in file: {indexfile:?}"))?;
		let bifentries = KeyBif::vecunpack(&mut f, hdr.nbif as usize)
			.with_context(|| format!("cannot read {} BIF entries in file: {indexfile:?}", hdr.nbif))?;
		let mut bifnames = Vec::<String>::new();
		let mut _bifsizes = Vec::<u32>::new();
		for KeyBif{offset, namelength, filelength, ..} in bifentries {
			f.seek(SeekFrom::Start(offset as u64))?;
			let buf = KeyBif::read_bytes(&mut f, namelength as usize - 1)?;
			bifnames.push(String::from_utf8(buf)?);
			_bifsizes.push(filelength);
		};
		f.seek(SeekFrom::Start(hdr.resoffset as u64))?;
		let resources = <KeyRes>::vecunpack(&mut f, hdr.nres as usize)
			.with_context(|| format!("cannot read {} BIF resources in file: {}",
				hdr.nres, indexfile.to_str().unwrap()))?;
		let languages = Self::languages(&gamedir)?;
		let backup = gamedir.join(crate::BACKUP_DIR);
		Ok(GameIndex{ root: gamedir, bifnames, resources, _bifsizes, languages,
			backup })
	}
	/// Initializes the set of languages used in the game.
	fn languages(gamedir: &impl AsRef<Path>)->Result<Vec<(String,PathBuf)>> {
		let langdir = gamedir.as_ref().join("lang");
		let mut r = Vec::<(String,PathBuf)>::new();
		for x in fs::read_dir(&langdir)
			.with_context(|| format!("cannot read lang directory: {langdir:?}"))? {
			let entry = x?;
			let lang = &entry.file_name().into_string().unwrap()[0..2];
			// TMP: this is to speed up execution (a bit) during test runs.
			if lang != "en" && lang != "fr" && lang != "frF" { continue }
			let dir = entry.path();
			let dialog = dir.join("dialog.tlk");
			if dialog.is_file() { r.push((lang.to_owned(), dialog)); }
			let dialog_f = dir.join("dialogF.tlk");
			if dialog_f.is_file() { r.push((format!("{lang}F"), dialog_f)); }
		}
		Ok(r)
	}
	/// Iterates over game resources (grouped by BIF file).
	///
	/// We cannot be a true `Iterator` because of the “streaming iterator”
	/// problem (aka cannot return references to iterator-owned data),
	/// so we perform internal iteration by this `for_each` method:
	pub fn for_each<F>(&self, mut f: F)->Result<()> where F: (FnMut(Restype, ResHandle<'_>)->Result<()>) {
		let pb = Progress::new(self.resources.len(), "resources");
		for (sourcefile, filename) in self.bifnames.iter().enumerate() {
			let path = self.root.join(filename);
			let mut bif = Option::<BifFile>::default();
			let pb1 = Progress::new(self.resources.len(), filename);
			for res in self.resources.iter() {
				let rread = ResHandle{ bif: &mut bif, path: &path,
					location: res.location, restype: res.restype, resref: res.resref, };
				if res.location.sourcefile() != sourcefile { continue }
				pb.inc(1);
				pb1.inc(1);
				f(res.restype, rread)?
			}
		}
		Ok(())
	}
	/// Backs up a game file (if not already done) while possibly changing
	/// the name.
	///
	/// (This is used for backing up all the "dialog.tlk" files).
	pub fn backup_as(&self, file: impl AsRef<Path>, to: impl AsRef<Path>)->Result<()> {
		let backup_dir = &self.backup;
		let file = file.as_ref();
		if !backup_dir.exists() {
			fs::create_dir(backup_dir)
				.with_context(||format!("cannot create backup directory: {backup_dir:?}"))?;
			info!("created backup directory {backup_dir:?}");
		} else {
			debug!("skipped creating backup directory {backup_dir:?}: directory exists");
		}
		let dest = backup_dir.join(to.as_ref());
		if !dest.exists() {
			fs::copy(file, &dest)
				.with_context(||format!("cannot backup {file:?} to {dest:?}"))?;
			info!("backed up {file:?} as {dest:?}");
		} else {
			debug!("skipped backup of {file:?} to {dest:?}: file already backed up");
		}
		Ok(())
	}
	/// Backs up a game file (if not already done).
	///
	/// This backs up resource files under the same name.
	pub fn backup(&self, file: impl AsRef<Path>)->Result<()> {
		self.backup_as(&file, file.as_ref().file_name().unwrap())
	}
}
/// A (lazy) accessor to a game resource.
///
/// This opens the BIF file only when its [`ResHandle::open`] method is invoked.
/// TODO: convert this into a trait, and add an implementation which
/// looks in override files instead.
#[derive(Debug)]
pub struct ResHandle<'a> {
	bif: &'a mut Option<BifFile>,
	path: &'a PathBuf,
	location: BifIndex,
	restype: Restype,
	/// The name associated to this buffer, either as an index in bif file,
	/// or as (TODO) the name of an override file.
	pub resref: Resref,
}
impl ResHandle<'_> {
	pub fn open(&mut self)->Result<Cursor<Vec<u8>>> {
		biffile(self.bif, &self.path)
			.context("cannot open BIF file")?;
		let biffile = self.bif.as_mut().unwrap();
		let j = self.location.resourceindex();
		let BifResource{ offset, size, restype, .. } = &biffile.resources[j];
		assert_eq!(restype, &self.restype);
		biffile.buf.seek(SeekFrom::Start(*offset as u64))?;
		Ok(Cursor::new(BifHdr::read_bytes(&mut biffile.buf, *size as usize)?))
	}
}

/// Unpacks the list of resources present in a given BIF file.
fn bifresources(file: impl AsRef<Path>+Debug)->Result<Vec<BifResource>> {
	let mut f = File::open(&file)
		.with_context(|| format!("cannot open bifresources in {file:?}"))?;
	let hdr = BifHdr::unpack(&mut f)
		.with_context(|| format!("cannot open BIF header in {file:?}"))?;
	f.seek(SeekFrom::Start(hdr.offset as u64))?;
	BifResource::vecunpack(&mut f, hdr.nres as usize)
		.map_err(|e| e.into())
}
/// Abstract, higher-level representation of a BIF file.
#[derive(Debug)]
struct BifFile {
	buf: BufReader<File>,
	resources: Vec<BifResource>,
}
/// Opens a BIF file (to cache) if not already open.
///
/// This converts a `None` option to a `Some()` where the BIF file is
/// loaded to memory. This avoids opening several times the same file,
/// or opening it when we have nothing to read in it.
fn biffile<'a>(o: &'a mut Option<BifFile>, path: &'a (impl AsRef<Path>+Debug))->Result<&'a BifFile> {
	if o.is_none() {
		let resources = bifresources(path).context("cannot read BIF resources")?;
		let buf = BufReader::new(File::open(path)?);
		*o = Some(BifFile{buf, resources});
	}
	Ok(o.as_ref().unwrap())
}
} // mod gamefiles
pub(crate) mod database {
//! Access to the SQL side of the database.
//!
//! Main exports are:
//!  - [`Schema`] type: description of a particular SQL table;
//!  - [`Table`] trait: connect a Rust structure to a SQL row.
use crate::prelude::*;
use rusqlite::{Row};
use std::marker::PhantomData;

// I. Low-level stuff: basic types and extensions for `rusqlite` traits.
pub trait ConnectionExt {
	fn exec(&self, s: impl AsRef<str>)->Result<()>;
}
impl ConnectionExt for Connection {
/// Shorthand for execution of a single SQL statement without parameters.
	fn exec(&self, s: impl AsRef<str>)->Result<()> {
		self.execute(s.as_ref(), ())
			.with_context(|| format!("failed SQL statement:\n{}", s.as_ref()))?;
		Ok(())
	}
}
pub trait RowExt {
	fn dump(&self);
}
impl RowExt for Row<'_> {
	/// Dumps all values found in a single row to stdout.
	fn dump(&self) {
		for (i, c) in self.as_ref().column_names().iter().enumerate() {
			use rusqlite::types::ValueRef::*;
			match self.get_ref(i) {
				Ok(Null) => println!("  [{i:3}] {c} = \x1b[31mNull\x1b[m"),
				Ok(Text(s)) => println!("  [{i:3}] {c} = Text(\"{}\")",
					std::str::from_utf8(s).unwrap()),
				Ok(x) => println!("  [{i:3}] {c} = {x:?}"),
				_ => break
			}
		}
	}
}
/// Connection::open, but with some extra logging
pub fn open(f: impl AsRef<Path>)->Result<Connection> {
	let path = f.as_ref();
	let db = Connection::open(path)
		.with_context(|| format!("cannot open database: {path:?}"))?;
	info!("opened database {path:?}");
	Ok(db)
}
/// Detecting recoverable errors due to incorrect SQL typing.
pub trait DbTypeCheck {
	/// Returns `true` when a `Result` is an `Err` variant corresponding to
	/// a recoverable error due to a wrong SQL type.
	fn is_db_malformed(&self)->bool;
}
impl<T> DbTypeCheck for Result<T,anyhow::Error> {
	fn is_db_malformed(&self)-> bool {
		use rusqlite::types::FromSqlError::{self,*};
		let err = match self { Ok(_) => return false, Err(e) => e };
		matches!(err.downcast_ref::<FromSqlError>(), Some(InvalidType))
	}
}
impl<T> DbTypeCheck for Result<T, rusqlite::Error> {
	fn is_db_malformed(&self)-> bool {
		matches!(self, Err(rusqlite::Error::FromSqlConversionFailure(_,_,_)))
	}
}


#[derive(Debug,Clone,Copy,PartialEq)]
/// An utility enum defining SQL behaviour for a given resource field type.
///
/// This is somewhat different from [`rusqlite::types::Type`]:
///  - not all SQLite types exist here;
///  - the `Strref` variant can accept either a string or an integer,
/// etc.
pub enum FieldType { Integer, Text, Resref, Strref }
impl FieldType {
	pub const fn affinity(self)->&'static str {
		match self {
			FieldType::Integer | FieldType::Strref => r#"integer default 0"#,
			FieldType::Text | FieldType::Resref => r#"text default """#,
		}
	}
}
/// A correspondence between (some) Rust types and SQL types.
pub trait SqlType { const SQL_TYPE: FieldType; }
impl<T> SqlType for Option<T> where T: SqlType {
	const SQL_TYPE: FieldType = T::SQL_TYPE;
}
impl SqlType for &str {
	const SQL_TYPE: FieldType = FieldType::Text;
}
impl SqlType for String {
	const SQL_TYPE: FieldType = FieldType::Text;
}
macro_rules! sqltype_int {
	($($T:ty),*) => { $(impl SqlType for $T {
		const SQL_TYPE: FieldType = FieldType::Integer;
	})* }
}
sqltype_int!{i8,i16,i32,i64,u8,u16,u32,u64,usize}
// impl<T> SqlType for T where T: num::Integer {
// 	const SQL_TYPE: FieldType = FieldType::Integer;
// }
// II. Everything connected to the table schemas (but not to Connection;
// only as SQL strings).
/// Description of a field in a game resource.
#[derive(Debug)] pub struct Column<'a> {
	pub fieldname: &'a str,
	pub fieldtype: FieldType,
	pub extra: &'a str,
}
/// The full database description of a game resource.
///
/// This contains all relevant information to fully define a resource
/// on the SQL side.
///
/// In practice there exists exactly one [`Schema`] instance per
/// resource, and it is compiled by the [`Table`] derive macro.
#[derive(Debug)] pub struct Schema<'a> {
	/// The stem for the SQL table name associated with this structure.
	pub table_name: &'a str,
	/// The SQL primary key of this table.
	pub primary_key: &'a str,
	/// The field pointing to the top-level resource.
	pub parent_key: &'a str,
	/// The table holding the top-level resource.
	pub parent_table: &'a str,
	/// Descriptions for all the fields of this struct.
	pub fields: &'a[Column<'a>],
}
impl<'schema> Schema<'schema> {
	// Step 0: a few useful functions
	/// Returns an object implementing [`std::fmt::Display`] to write the
	/// column headers for this schema in a SQL query.
	///
	/// Each column may be preceded by a prefix; this is used for e.g.
	/// `"new"."column"`.
	pub fn columns<T: Display>(&'schema self, s: T)->ColumnWriter<'schema, T> {
		ColumnWriter { schema: self, prefix: s }
	}
	// Step 1: creating tables
	/// Returns the SQL statement creating a given table with this schema.
	///
	/// The parameters are the table name (not necessarily matching the
	/// name of the schema) and the string defining extra columns, if any.
	fn create_table(&self, name: &str, more: &str)->String {
		use std::fmt::Write;
		let mut s = format!("create table \"{name}\" (");
		let mut isfirst = true;
		for Column { fieldname, fieldtype, extra } in self.fields.iter() {
			if isfirst { isfirst = false; }
			else { s.push(','); }
			write!(&mut s, "\n \"{fieldname}\" {} {extra}", fieldtype.affinity()).unwrap();
		}
		write!(&mut s, "{more})").unwrap();
		s
	}
	/// Creates all tables and views in the database associated with a
	/// given resource.
	///
	/// The tables are as follows:
	/// - `res_{name}`: data read from game files.
	/// - `add_{name}`: new data inserted by mods.
	/// - `edit_{name}`: data modified by mods.
	/// The views are:
	/// - `{name}`: the main view on which edits are done by mods (and
	/// propagated to `add_{name}` or `edit_{name}` by triggers as needed).
	/// - `out_{name}`: the view used to save new or modified values to
	/// game files.
	pub fn create_tables_and_views(&self, db: &Connection)->Result<()> {
		let name = &self.table_name;
		let parent_key = &self.parent_key;
		let parent_table = &self.parent_table;
		let key = &self.primary_key;

		use std::fmt::Write;
		db.exec(self.create_table(format!("res_{name}").as_str(), ""))?;
		db.exec(self.create_table(format!("add_{name}").as_str(),
			r#", "source" text"#))?;
		db.exec(format!(r#"create table "edit_{name}" ("source" text, "resource" text, "field" text, "value")"#).as_str())?;
		{ // create main view
		let mut view = format!(r#"create view "{name}" as
	with "u" as (select {0} from "res_{name}" union select {0} from "add_{name}") select "#, self.columns(""));
		for (i, Column {fieldname, ..}) in self.fields.iter().enumerate(){
			if i > 0 { writeln!(&mut view, ",").unwrap(); }
			if fieldname == key {
				write!(&mut view, r#""{fieldname}""#).unwrap();
			} else {
				write!(&mut view, r#"ifnull((select "value" from "edit_{name}" where "resource"="{key}" and "field"='{fieldname}' order by rowid desc limit 1), "{fieldname}") as "{fieldname}" "#).unwrap();
			}
		}
		write!(&mut view, r#" from "u""#).unwrap();
		db.exec(view)?;
		}
		let dirtytable = format!("dirty_{parent_table}");
		db.exec(format!(r#"create table if not exists "{dirtytable}" ("name" text primary key)"#))?;
		let mut trans_edit = String::new();
		for Column {fieldname, fieldtype, ..} in self.fields.iter() {
			// populate resref_dict or strref_dict as needed:
			let trans = match fieldtype {
			FieldType::Resref => format!(
	r#"insert or ignore into "resref_dict" values (new."{fieldname}", null);"#),
// 			FieldType::Strref => format!(
// 	r#"insert or ignore into "strref_dict" values (new."{fieldname}", null);"#),
			_ => String::new()
			};
			trans_edit.push_str(&trans);
			if fieldname == key { continue }
			let trig = format!(
	r#"create trigger "update_{name}_{fieldname}"
	instead of update on "{name}" when new."{fieldname}" is not null
		and new."{fieldname}" <> old."{fieldname}"
	begin
		{trans}
		insert or ignore into "{dirtytable}" values (new."{parent_key}");
		insert into "edit_{name}" ("source", "resource", "field", "value") values
			((select "component" from "global"), new."{key}", '{fieldname}',
			new."{fieldname}");
	end"#);
			db.exec(trig)?;
			if *fieldtype == FieldType::Strref {
			}
		}
		let trig = format!(
	r#"create trigger "insert_{name}"
	instead of insert on "{name}"
	begin
		{trans_edit}
		insert into "add_{name}" ({0}) values ({1});
		insert or ignore into "{dirtytable}" values (new."{parent_key}");
	end"#, self.columns(""), self.columns("new."));
		db.exec(trig)?;
		let trig = format!(
	r#"create trigger "delete_{name}"
	instead of delete on "{name}"
	begin
		insert or ignore into "{dirtytable}" values (old."{parent_key}");
		delete from "add_{name}" where "{key}" = old."{key}";
	end"#);
		db.exec(trig)?;
		let trig = format!(
	r#"create trigger "unedit_{name}"
	after delete on "edit_{name}"
	begin
		insert or ignore into "{dirtytable}" values (old."resource");
	end"#);
		db.exec(trig)?;
		db.exec(self.create_output_view())?;
		Ok (())
	}
	/// Returns the SQL for creating the output view of a resource.
	///
	/// This is a (large) part of [`Self::create_tables_and_views`].
	fn create_output_view(&self)->String {
		let n = '\n';
		use std::fmt::Write;
		let name = &self.table_name;
		let Self { parent_key, .. } = self;
		let mut select = format!(r#"create view "out_{name}" as select{n}  "#);
		let mut source = format!(r#"{n}from "{name}" as "a"{n}"#);
		for Column { fieldname: f, fieldtype, .. } in self.fields.iter() {
			match fieldtype {
				FieldType::Resref => {
					let a = format!(r#""a"."{f}""#);
					let b = format!(r#""b_{f}""#);
					write!(&mut select, r#"case when {a} in (select "resref" from "resref_orig") then {a} else ifnull({b}."resref", '') end as "{f}",{n}  "#).unwrap();
					write!(&mut source, r#"left join "resref_dict" as {b} on {a} = {b}."key"{n}"#).unwrap();
				},
				FieldType::Strref => {
					let a = format!(r#""a"."{f}""#);
					let b = format!(r#""b_{f}""#);
					write!(&mut select, r#"case when typeof({a}) == 'integer' then {a} else ifnull({b}."strref", 0) end as "{f}",{n}  "#).unwrap();
					write!(&mut source, r#"left join "strref_dict" as {b} on {a} = {b}."key"{n}"#).unwrap();
				},
				_ => write!(&mut select, r#""a"."{f}",{n}  "#).unwrap()
			}
		}
		write!(&mut select, r#""a"."{parent_key}" as "key"{source}"#).unwrap();
		select
	}
	// Step 2: populating tables
	/// The SQL code for populating the database from game files.
	pub fn insert_statement(&self, prefix: impl Display)->String {
		let table_name = &self.table_name;
		let mut s = format!("insert into \"{prefix}{table_name}\" ({}) values (",
			self.columns(""));
		for c in 0..self.fields.len() {
			if c > 0 { s.push(','); }
			s.push('?');
		}
		s.push(')');
		s
	}
	fn all_keys_gen<T>(&self, db: &Connection, condition: impl Display, f: impl Fn(rusqlite::types::ValueRef<'_>)->Result<T>)->Result<Vec<T>> {
		let s = format!(r#"select "{primary_key}" from "{table}" {condition}"#,
			primary_key = self.primary_key, table = self.table_name);
		let mut stmt = db.prepare(&s)
			.context("preparing statement")?;
		let mut rows = stmt.query(())?;
		let mut v = Vec::<T>::new();
		while let Some(row) = rows.next()? {
			v.push(f(row.get_ref(self.primary_key)?)?);
		}
		Ok(v)
	}
	pub fn all_keys<T>(&self, db: &Connection, f: impl Fn(rusqlite::types::ValueRef<'_>)->Result<T>)->Result<Vec<T>> {
		self.all_keys_gen::<T>(db, "", f)
	}
	pub fn all_keys_with_parent<T>(&self, db: &Connection, value: impl Display, f: impl Fn(rusqlite::types::ValueRef<'_>)->Result<T>)->Result<Vec<T>> {
		self.all_keys_gen(db, format!(r#"where "{parent_key}"='{value}'"#,
			parent_key = self.parent_key), f)
	}
}

/// A utility type for inserting column headers from a schema in a string.
///
/// This type implements [`std::fmt::Display`], which simplifies writing
/// some SELECT statements.
#[derive(Debug)] pub struct ColumnWriter<'a,T: Display> {
	prefix: T,
	schema: &'a Schema<'a>,
}
impl<T: Display> Display for ColumnWriter<'_,T> {
	fn fmt(&self, mut f: &mut Formatter<'_>)->std::result::Result<(),std::fmt::Error>{
		use std::fmt::Write;
		let mut isfirst = true;
		for Column { fieldname, .. } in self.schema.fields.iter() {
			if isfirst { isfirst = false; } else { write!(f, ",")?; }
			write!(&mut f, r#" {}"{fieldname}""#, self.prefix)?;
		}
		Ok(())
	}
}

// III. Structure accessing directly SQL data
/// Interface for a game resource.
///
/// This trait is implemented by the corresponding derive macro.
///
/// This is the main trait for game resources, containing the low-level
/// interaction with the database (`ins`, `sel`). Concrete
/// implementations are provided by the `Table` derive macro.
pub trait Table: Sized {
	/// Additional data saved in the same row as a game object; usually
	/// some identifier for the object (e.g. its resref).
	type KeyIn;
	/// Additional data read in the same row as a game object.
	///
	/// This is almost the same as `KeyIn`, except that any `Option` types
	/// will have been unwrapped (by SQL) to the `Some` variant.
	type KeyOut;
	/// always == `anyhow::Result<(Self, Self::KeyOut)>`.
	///
	/// TODO: Res is more useful than KeyOut, replace KeyOut by Res
	type Res;
	/// The internal description of the fields of this structure, as used
	/// to produce SQL statements.
	const SCHEMA: Schema<'static>;
	/// The low-level insert function for an object to a database row.
	fn ins(&self, s: &mut Statement<'_>, key: &Self::KeyIn)->rusqlite::Result<()>;
	/// The low-level select function from a database row to an object.
	fn sel(r: &Row<'_>)->rusqlite::Result<(Self, Self::KeyOut)>;
	/// The select [`rusqlite::Statement`] for an object.
	///
	/// The name of the table is `{n1}{n2}`; since many tables are built in
	/// two parts (e.g. `res_{name}`, this avoids calling `format!` on the
	/// caller side and slightly simplifies API.
	///
	/// This returns a [`Result<TypedStatement>`]: it is possible to iterate
	/// over the result and recover structures of the original type.
	fn select_query_gen(db: &Connection, n1: impl Display,
		n2: impl Display, cond: impl Display)->rusqlite::Result<TypedStatement<'_,Self>> {
		let s = format!(r#"select {cols} from "{n1}{n2}" {cond}"#,
			cols=Self::SCHEMA.columns(""));
		debug!("running SQL query: {s}");
		Ok(TypedStatement(db.prepare(&s)?, PhantomData::<Self>))
	}
	/// Particular case of SELECT statement used for saving to game files.
	fn select_query(db: &Connection, s: impl Display)->rusqlite::Result<TypedStatement<'_, Self>> {
		Self::select_query_gen(db, "out_", Self::SCHEMA.table_name, s)
	}
}
/// A statement aware of the associated table type.
///
/// We need a few types parametrized by a Table:
///  - `TypedStatement`: this gets saved as a (mut) local variable;
///  - `TypedRows`: the iterator producing Table objects from the query.
#[derive(Debug)]
pub struct TypedStatement<'stmt, T: Table> (Statement<'stmt>, PhantomData<T>);
impl<T: Table> TypedStatement<'_, T> {
	/// Iterates over this statement, returning (possibly) Rust structs of
	/// the type associated with this table.
	pub fn iter<P: rusqlite::Params>(&mut self, params: P) ->rusqlite::Result<TypedRows<'_,T>> {
		let rows = self.0.query(params)?;
		Ok(TypedRows { rows, _marker: PhantomData::<T>, index: 0 })
	}
}
/// An enriched version of `rusqlite::Rows`.
///
/// This struct retains information about the output type,
/// as well as the current row index.
///
/// This also behaves as an `Iterator` (throwing when the underlying
/// `Row` iterator fails).
#[allow(missing_debug_implementations)]
pub struct TypedRows<'stmt,T: Table> {
	rows: rusqlite::Rows<'stmt>,
	_marker: PhantomData<T>,
	index: usize,
}
impl<T: Table> Iterator for TypedRows<'_,T> {
	type Item = Result<(T, T::KeyOut)>;
	fn next(&mut self)->Option<Self::Item> {
		loop {
			let row = match self.rows.next() {
				Ok(Some(row)) => row,
				Ok(None) => break,
				Err(e) => return Some(Err(e.into()))
			};
			self.index+= 1;
			let t = T::sel(row);
			if t.is_err() {
				println!("cannot read a {} from row {}", std::any::type_name::<T>(),
					self.index);
				row.dump();
				continue
			}
			return Some(Ok(t.unwrap()))
		}
		None
	}
}
} // mod resources
pub(crate) mod progress {
//! Generic useful functions for user interaction.
//!
//! This contains among other:
//!  - [`Progress`], a utility wrapper for a progress bar stack;
//!  - [`transaction`], wrapping a closure inside a database transaction.
use crate::prelude::*;
use std::cell::{RefCell};
use indicatif::{ProgressBar,ProgressStyle,MultiProgress};

thread_local! {
	static COUNT: RefCell<usize> = RefCell::new(0);
	static MULTI: RefCell<MultiProgress> =
		RefCell::new(MultiProgress::new());
}
/// A stacked progress bar.
///
/// This bar is removed from the stack when it is dropped.
#[derive(Debug)]
pub struct Progress { pb: ProgressBar, name: String, }
impl AsRef<ProgressBar> for Progress {
	fn as_ref(&self)->&ProgressBar { &self.pb }
}
impl Drop for Progress {
	fn drop(&mut self) {
		debug!(r#"finished task "{name}" in {elapsed:?}"#,
			name=self.name, elapsed=self.pb.elapsed());
		MULTI.with(|c| c.borrow_mut().remove(&self.pb));
		COUNT.with(|c| *c.borrow_mut()-= 1)
	}
}

const COLORS: &[&str] = &["red", "yellow", "green", "cyan", "blue", "magenta" ];

impl Progress {
	/// Appends a new progress bar to the stack.
	pub fn new(n: impl num::ToPrimitive, text: impl Display)->Self {
		let name = text.to_string();
		let s = format!("{name} {{wide_bar:.{}}}{{pos:>5}}/{{len:>5}}",
			COLORS[COUNT.with(|c| *c.borrow()) % COLORS.len()]);
		let pb0 = ProgressBar::new(<u64 as num::NumCast>::from(n).unwrap());
		pb0.set_style(ProgressStyle::with_template(&s).unwrap());
		let pb = MULTI.with(|c| c.borrow_mut().add(pb0));
		COUNT.with(|c| *c.borrow_mut()+= 1);
		debug!(r#"start task "{name}""#);
		Self { pb, name, }
	}
	/// Advances the progress bar by the given amount.
	pub fn inc(&self, n: u64) { self.as_ref().inc(n) }
}

/// Wraps a closure inside a game transaction.
///
/// The transaction aborts if the closure returns an `Err` variant, and
/// commits if it returns an `Ok` variant.
pub fn transaction<T>(db: &mut Connection, mut f: impl FnMut(&rusqlite::Transaction<'_>)->Result<T>)->Result<T> {
	let t = db.transaction().context("create new transaction")?;
	let r = f(&t)?; // automatic rollback if Err
	t.commit()?;
	Ok(r)
}

} // mod progress
pub(crate) mod gamestrings {
//! Access to game strings in database.
use crate::prelude::*;
use crate::progress::{Progress};
use crate::database::{self,ConnectionExt,Table};
use crate::gamefiles::{Pack,GameIndex};

#[derive(Debug,Pack)] struct TlkHeader {
	#[header("TLK V1  ")]
	lang: u16,
	nstr: u32,
	offset: u32,
}
/// A game string as present in a .tlk file.
#[derive(Debug,Default,Clone,Pack,Table)]
pub struct GameString {
#[column(strref, usize, "primary key")]
	flags: u16,
	sound: Resref,
	volume: i32,
	pitch: i32,
#[column(false)] delta: i32,
#[column(false)] strlen: i32,
	string: String,
}
/// The iterator used for accessing game strings.
#[derive(Debug)] pub struct GameStringsIterator<'a> {
	cursor: Cursor<&'a[u8]>,
	index: usize,
	nstr: usize,
	offset: usize,
}
impl<'a> TryFrom<&'a [u8]> for GameStringsIterator<'a> {
	type Error = anyhow::Error;
	fn try_from(bytes: &'a[u8])->Result<Self> {
		let mut cursor = Cursor::new(bytes);
		let header = TlkHeader::unpack(&mut cursor)
			.context("malformed TLK header")?;
		Ok(Self { cursor, index: 0, nstr: header.nstr as usize,
			offset: header.offset as usize })
	}
}
impl Iterator for GameStringsIterator<'_> {
	type Item = Result<GameString>;
	/// Iterates over all game strings in the file, returning a `Result`
	/// indicating whether there was an error in the file.
	fn next(&mut self)->Option<Self::Item> {
		if self.index >= self.nstr { return None }
		let mut s = match GameString::unpack(&mut self.cursor) {
			Err(e) => return Some(Err(e.into())),
			Ok(s) => s,
		};

		let start = self.offset + (s.delta as usize);
		let end = start + (s.strlen as usize);
		let buf = self.cursor.get_ref();
		// string *might* be zero-terminated, in which case we discard:
		let c = buf[end-1];
		let end2 = std::cmp::max(start, end-((c==0) as usize));
		s.string.push_str(std::str::from_utf8(&buf[start..end2]).ok()?);
		self.index+= 1;
		Some(Ok(s))
	}
}
impl ExactSizeIterator for GameStringsIterator<'_> {
	fn len(&self) -> usize { self.nstr }
}
/// Saves game strings in one language to the given file.
pub fn save_language(vec: &[GameString], path: &impl AsRef<Path>)->Result<()> {
	// FIXME: this could be done with a prepared statement and iterator
	// (saving the (rather big) memory for the whole vector of strings),
	// but rusqlite does not export `reset`...
	let mut file = fs::File::create(path)
		.with_context(|| format!("cannot create TLK file: {:?}", path.as_ref()))?;
	TlkHeader { lang: 0, nstr: vec.len() as u32,
		offset: (26*vec.len() + 18) as u32 }.pack(&mut file)?;
	// first string in en.tlk has: (flags: u16=5, offset=0, strlen=9)
	// second string has (flags: u16=1, offset=9, strlen=63) etc.
	// (offset=72) etc.
	let mut delta = 0;
	for s in vec.iter() {
		let l = s.string.len() as u32;
		(s.flags, s.sound, s.volume, s.pitch, delta, l).pack(&mut file)?;
		delta+= l;
	}
	for s in vec {
		file.write_all(s.string.as_bytes())
			.with_context(|| format!("cannot write strings to TLK file:{:?}", path.as_ref()))?;
	}
	Ok(())
}

/// Fills all game strings tables from game dialog files.
pub fn load(db: &Connection, game: &GameIndex)->Result<()> {
	db.exec("begin transaction")?;
	let pb = Progress::new(game.languages.len(), "languages");
	for (langname, dialog) in game.languages.iter() {
		pb.inc(1);
		load_language(db, langname, &dialog)
			.with_context(|| format!("cannot load language '{langname}' from '{dialog:?}'"))?;
	}
	db.exec("commit")?; Ok(())
}
/// Fills the database table for a single language.
fn load_language(db: &Connection, langname: &str, path: &(impl AsRef<Path> + Debug))->Result<()> {
	let bytes = fs::read(path)
		.with_context(|| format!("cannot open strings file: {path:?}"))?;
	let mut q = db.prepare(&format!(r#"insert into "res_strings_{langname}" ("strref", "flags", "sound", "volume", "pitch", "string") values (?,?,?,?,?,?)"#))?;
	let itr = GameStringsIterator::try_from(bytes.as_ref())?;
	let pb = Progress::new(itr.len(), "strings");
	db.execute(r#"update "global" set "strref_count"=?"#, (itr.len(),))?;
	let mut n_strings = 0;
	for (strref, x) in itr.enumerate() {
		let s = x?;
		pb.inc(1);
		q.execute((strref, s.flags, s.sound, s.volume, s.pitch, s.string))?;
		n_strings+= 1;
	}
	info!("loaded {n_strings} strings for language \"{langname}\"");
	Ok(())
}
/// Saves all database strings to "dialog.tlk" files for all game
/// languages.
///
/// This also backups those files if needed (i.e. if no backup exists
/// yet).
pub fn save(db: &Connection, game: &GameIndex)->Result<()> {
	use database::DbTypeCheck;
	let pb = Progress::new(game.languages.len(), "save translations");
	for (lang, path) in game.languages.iter() {
		pb.inc(1);
		if lang != "en" { continue } // TODO: remove; this is to speed up tests
		let mut s1 = String::new(); let mut s2 = String::new();
		for c in path.iter() {
			std::mem::swap(&mut s1, &mut s2);
			s2.clear(); s2.push_str(c.to_str().unwrap());
		}
		println!("s1='{s1}', s2='{s2}'");
		game.backup_as(path, format!("{s1}.tlk"))
			.with_context(|| format!("cannot backup strings file {path:?}"))?;
		let count = db.query_row(&format!(r#"select max("strref") from "strings_{lang}""#),
			(), |row| row.get::<_,usize>(0))?;
		let mut vec = vec![GameString::default(); count+1];
		let mut sel_str = GameString::select_query_gen(db, "strings_", lang, "")?;
		for row in sel_str.iter(())? {
			if row.is_db_malformed() { continue }
			let (gamestring, (strref,)) = row.unwrap();
			vec[strref] = gamestring;
		}
		let target = format!("./{lang}.tlk");
		save_language(&vec, &target)?;
		info!("updated strings in {target:?}; file now contains {count} entries");
	}
	Ok(())
}
}
pub(crate) mod lua_api {
//! Loads mod-supplied Lua files.
//!
//! The API for this module is pretty limited to the [`command_add`]
//! function.
use mlua::{Lua,ExternalResult};

use crate::prelude::*;
use crate::database::{Schema};
use crate::gametypes::RESOURCES;
#[derive(Debug)] struct WrappedAnyhow(anyhow::Error);
impl std::error::Error for WrappedAnyhow{ }
impl Display for WrappedAnyhow {
	fn fmt(&self, f: &mut Formatter<'_>)->std::fmt::Result {
		Display::fmt(&self.0, f)
	}
}
/// Runtime errors during interface between SQL and Lua.
#[derive(Debug)] enum Error{
	UnknownTable(String),
// 	BadType(String),
// 	BadArgumentNumber(usize, &'static str),
	MissingArgument,
	ExtraArgument,
}
impl Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>)->std::fmt::Result {
		std::fmt::Debug::fmt(&self, f)
	}
}
impl std::error::Error for Error { }

/// A simple wrapper allowing `mlua::Value` to be converted to SQL.
struct LuaToSql<'a>(mlua::Value<'a>);
impl rusqlite::ToSql for LuaToSql<'_> {
	fn to_sql(&self)->rusqlite::Result<rusqlite::types::ToSqlOutput<'_>> {
		use rusqlite::types::{ToSqlOutput::Owned,Value as SqlValue};
		let LuaToSql(value,) = self;
		match value {
		mlua::Value::Nil => Ok(Owned(SqlValue::Null)),
		mlua::Value::Boolean(b) => Ok(Owned(SqlValue::Integer(*b as i64))),
		mlua::Value::Integer(n) => Ok(Owned(SqlValue::Integer(*n))),
		mlua::Value::Number(x) => Ok(Owned(SqlValue::Real(*x))),
		mlua::Value::String(ref s) =>
			Ok(Owned(SqlValue::from(s.to_str().unwrap().to_owned()))),
		e => Err(rusqlite::Error::InvalidParameterName(format!("cannot convert {e:?} to a SQL type")))
		}
	}
}
impl<'lua> mlua::FromLua<'lua> for LuaToSql<'lua> {
	fn from_lua(v: mlua::Value<'lua>, _lua: &'lua Lua)->mlua::Result<Self> {
		Ok(Self(v))
	}
}
fn sql_to_lua<'lua>(v: rusqlite::types::ValueRef<'_>, lua: &'lua Lua)->Result<mlua::Value<'lua>> {
	use rusqlite::types::{ValueRef::*};
	match v {
		Null => Ok(mlua::Value::Nil),
		Integer(n) => Ok(mlua::Value::Integer(n)),
		Real(x) => Ok(mlua::Value::Number(x)),
		Text(s) => Ok(mlua::Value::String(lua.create_string(&s)?)),
		_ => Err(rusqlite::types::FromSqlError::InvalidType.into()),
	}
}
/// Returns the table schema for a given table (indexed by the base name
/// for this resource), or throws a Lua-compatible error.
fn table_schema(table: &str)->Result<&Schema<'_>> {
	RESOURCES.table_schema(table)
		.ok_or_else(|| Error::UnknownTable(table.to_owned()).into())
}
fn pop_arg_as<'lua, T: mlua::FromLua<'lua>>(args: &mut mlua::MultiValue<'lua>, lua:&'lua Lua)->Result<T> {
	let arg0 = args.pop_front().ok_or(Error::MissingArgument)?;
	let r = T::from_lua(arg0, lua).with_context(||
		format!("cannot convert argument to type {}", std::any::type_name::<T>()))?;
	Ok(r)
}
/// Lists all possible primary keys for one of the resource tables.
// fn list_keys(db: &Connection, (table,): (String,))->Result<Vec<String>> {
// 	Ok(table_schema(&table)?.all_keys(db)?)
// }
fn list_keys<'lua>(db: &Connection, lua: &'lua Lua, mut args: mlua::MultiValue<'lua>)->Result<Vec<mlua::Value<'lua>>> {
	trace!("callback 'simod.list' invoked with: {args:?}");
	let table_name = pop_arg_as::<String>(&mut args, lua)
		.context("'simod.list' callback")?;
	trace!(r#"  first argument is "{table_name}""#);
	let schema = table_schema(&table_name)?;
	match args.len() {
		0 => Ok(schema.all_keys(db, |v| sql_to_lua(v, lua))?),
		1 => {
			let key = pop_arg_as::<String>(&mut args, lua)
				.context("'simod.list' callback")?;
			trace!(r#"  second argument is "{key}""#);
			Ok(schema.all_keys_with_parent(db, key, |v| sql_to_lua(v,lua))?)
		},
		_ => Err(Error::ExtraArgument.into()),
	}
}
/// Implementation of the `simod.select` function exported to Lua scripts.
///
/// Reads a full row from one of the resource tables and returns it as a
/// Lua table.
fn select<'lua>(db: &Connection, lua: &'lua Lua, (table, key): (String, String))->Result<mlua::Table<'lua>> {
	let schema = table_schema(&table)?;
	let tbl = lua.create_table_with_capacity(0,
		schema.fields.len() as std::ffi::c_int)?;
	let s = format!(r#"select {cols} from "{table}" where "{primary_key}"=?"#,
		cols = schema.columns(""), primary_key = schema.primary_key);
	trace!("query: {s}; {key}");
	db.query_row_and_then(&s, (&key,), |row| {
		for (i, col) in schema.fields.iter().enumerate() {
			let key = col.fieldname;
			let val = row.get_ref(i)?;
			trace!("row {i}, got value {val:?}, expected {:?}", col.fieldtype);
			tbl.set(key, sql_to_lua(val, lua)
				.with_context(|| format!("cannot convert value {val:?} to Lua"))?)
				.with_context(|| format!("cannot set entry for {key} to {val:?}"))?;
		}
		Ok(tbl)
	})
}
/// Implementation of the `simod.update` function exported to Lua scripts.
///
/// Updates a single field in one of the resource tables.
/// The type of value is not checked (TODO: do this either here or as a SQL
/// constraint when creating the tables?).
fn update(db: &Connection, (table, key, field, value): (String, String, String, mlua::Value<'_>))->Result<()> {
	let schema = table_schema(&table)?;
	let s = format!(
		r#"update "{table}" set "{field}"=? where "{primary_key}"='{key}'"#,
		primary_key = schema.primary_key);
	db.execute(&s, (LuaToSql(value),))
		.with_context(|| format!("failed SQL query: {s}"))?;
	Ok(())
}
/// Implementation of `simod.insert` exported to Lua.
fn insert(db: &Connection, (key, vals): (String, mlua::Table<'_>))->Result<()> {
	use crate::database::Column;
	let schema = table_schema(&key)?;
	let mut stmt = db.prepare(&schema.insert_statement(""))?;
	let mut fields = Vec::<LuaToSql<'_>>::with_capacity(schema.fields.len());
	for Column { fieldname, .. } in schema.fields.iter() {
		fields.push(vals.get(*fieldname)?);
	}
	stmt.execute(rusqlite::params_from_iter(fields))?;
	Ok(())
}
/// Runs the lua script for adding a mod component to the database.
///
/// We provide the lua side with a minimal low-level interface:
///  - update("items", "sw1h34", "field", "value")
///  - select("items", "sw1h34")
///  - insert("items", ...)
///  - list("items")
///  - list("item_abilities", "sw1h34") etc.
/// All code with a higher level is written in lua and loaded from the
/// "init.lua" file.
pub fn command_add(db: Connection, _target: &str)->Result<()> {
	use crate::database::RowExt;
	let lua = Lua::new();
	let lua_file = Path::new("/home/jerome/src/infinity_compiler/init.lua");

	// We need to wrap the rusqlite calls in a Lua scope to preserve  the
	// lifetimes of the references therein:
	lua.scope(|scope| {
		let simod = lua.create_table()?;
		simod.set("dump", scope.create_function(
		|_lua, (query,): (String,)| {
			debug!("lua called exec with query {query}");
			let mut stmt = db.prepare(&query).to_lua_err()?;
			let mut rows = stmt.query(()).to_lua_err()?;
			while let Some(row) = rows.next().unwrap() {
				row.dump();
			}
			Ok::<_,mlua::Error>(())
		})?)?;
		simod.set("list", scope.create_function(
			|_lua, args| list_keys(&db, &lua, args).to_lua_err())?)?;
		simod.set("select", scope.create_function(
			|_lua, args| select(&db, &lua, args).to_lua_err())?)?;
		simod.set("update", scope.create_function(
			|_lua, args| update(&db, args).to_lua_err())?)?;
		simod.set("insert", scope.create_function(
			|_lua, args| insert(&db, args).to_lua_err())?)?;
		lua.globals().set("simod", simod)?;

		info!("loading file {lua_file:?}");
		lua.load(lua_file).exec()?;
		Ok(())
	})?;
	Ok(())
}
} // mod lua_api

pub mod gametypes;

use crate::prelude::*;
use clap::Parser;

fn type_of<T>(_:&T)->&'static str { std::any::type_name::<T>() }

use database::{ConnectionExt};
use gamefiles::{GameIndex};
use progress::{Progress};
use gametypes::*;

// I init
/// Creates and initializes the game database.
///
/// This creates all relevant tables and views in the database
/// (but does not fill them).
///
/// There are global tables, per-resource tables, and per-language tables.
/// The per-resource tables are described in the
/// [`database::Schema::create_tables_and_views`] function.
/// The per-language tables are described in [`create_language_tables`].
/// The global tables are the following:
///
/// - `global`: a single-row table holding the global variables:
///   - `component`: current mod component being installed;
///   - `strref_count`: number of strings in `"dialog.tlk"` (34000);
/// - `resref_orig`: list of all original game resrefs — this is used for
///    de-namespacing (all resrefs in this list translate to themselves);
/// - `resref_dict`: translations of resrefs (from namespaced to 8 bytes);
/// - `strref_dict`: translations of strrefs (from string key to 4-bytes int);
///
/// - `string_keys`: view of `strref_dict` primarily used for automatic
///   strref assignment — inserting into this view calls the appropriate
///   trigger selecting the next available strref.
/// - `new_strings`: (built in loop below) a view of all strref currently
///   introduced by mods. This gets compiled into `string_keys` as part
///   of the string translation process.
///
pub fn create_db(db_file: impl AsRef<Path>, game: &GameIndex)->Result<Connection> {
	use std::fmt::Write;
	let db_file = db_file.as_ref();
	info!("creating file {db_file:?}");
	if Path::new(&db_file).exists() {
		fs::remove_file(db_file)
		.with_context(|| format!("cannot remove DB file: {db_file:?}"))?;
	}
	let mut db = Connection::open(db_file)
		.with_context(|| format!("cannot open database: {db_file:?}"))?;
	info!("opened database {db_file:?}");
	progress::transaction(&mut db, |db| {
	db.execute_batch(r#"
create table "global" ("component" text, "strref_count" integer);
insert into "global"("component") values (null);
create table "resref_orig" ("resref" text primary key on conflict ignore);
create table "resref_dict" ("key" text not null primary key on conflict ignore, "resref" text);
create table "strref_dict" ("key" text not null primary key on conflict ignore, "strref" integer unique, "flags" integer);
create index "strref_dict_reverse" on "strref_dict" ("strref");

create view "string_keys" as select "key", "flags" from "strref_dict";
create trigger "strref_auto" instead of insert on "string_keys"
begin
	insert into "strref_dict"("key", "strref","flags") values
	(new."key", ifnull((select min("strref")+1 from "strref_dict" where "strref"+1 not in (select "strref" from "strref_dict")), (select "strref_count" from "global")), new."flags");
end;
	"#).context("create strings tables")?;
	let pb = Progress::new(RESOURCES.len(), "create tables");
	let mut new_strings = String::from(r#"create view "new_strings"("key","flags") as "#);
	let mut new_strings_is_first = true;
	RESOURCES.map_mut(|schema,_| {
		pb.inc(1);
		for database::Column { fieldname, fieldtype, .. } in schema.fields.iter() {
			if *fieldtype == database::FieldType::Strref {
				let name = &schema.table_name;
				if new_strings_is_first { new_strings_is_first = false; }
				else { write!(&mut new_strings, "\nunion\n")?; }
				write!(&mut new_strings, r#"select "{fieldname}", 1 from "add_{name}" union select "value", 1 from "edit_{name}" where "field" = '{fieldname}'"#,
					)?;
			}
		}
		info!("  creating tables for resource {}", schema.table_name);
		schema.create_tables_and_views(db)?; Result::<()>::Ok(())
	})?;
	db.exec(new_strings)?; Ok(())
	}).context("creating global tables")?;
	create_language_tables(&mut db, game)?;
	Ok(db)
}
/// Creates (but does not fill) all per-language tables.
///
/// The per-language tables are as follows:
/// - `translations_XX`: user-filled table containing string translations
/// - `strings_XX`: output to fill game strings
///   (last column is a bit indicating whether this is untranslated, in
///   which case we need to remove all {?..} marks)
pub fn create_language_tables(db: &mut Connection, game: &GameIndex)->Result<()> {
	crate::progress::transaction(db, |db| {
	const SCHEMA: &str = r#"("strref" integer primary key, "string" text, "flags" integer, "sound" text, "volume" integer, "pitch" integer)"#;
	const SCHEMA1: &str = r#"("key" string primary key, "string" text, "flags" integer, "sound" text, "volume" integer, "pitch" integer)"#;
	for (lang, _) in game.languages.iter() {
		db.execute_batch(&format!(r#"
create table "res_strings_{lang}"("strref" integer primary key, "string" text, "flags" integer, "sound" text, "volume" integer, "pitch" integer);
create table "translations_{lang}"("key" string primary key, "string" text, "sound" text, "volume" integer, "pitch" integer);
create view "strings_{lang}" as
select "strref", "string",  "flags", "sound", "volume", "pitch", 0 as "raw" from "res_strings_{lang}" union
select "strref", ifnull("string", "key"), "flags", ifnull("sound",''), ifnull("volume",0), ifnull("pitch",0), ("string" is null)
from "strref_dict" as "d"
left join "translations_{lang}" as "t" using ("key");
"#))?;
	}
	Ok(())
	})
}
/// Fills the database from game files.
///
/// The database must have been already created and initialized (with
/// empty tables).
fn load(db: Connection, game: &GameIndex)->Result<()> {
	let pb = Progress::new(3, "Fill database"); pb.inc(1);
	gamestrings::load(&db, game)
		.with_context(|| format!("cannot read game strings from game directory {:?}", game.root))?;
	pb.inc(1);
	db.exec("begin transaction")?;
	let mut base = DbInserter::new(&db)?;
	game.for_each(|restype, handle| {
		trace!("found resource {}.{:#04x}", &handle.resref, restype.value);
		base.register(&handle.resref)?;
		match restype {
		RESTYPE_ITM => base.load::<Item>(handle),
		_ => Ok(())
		}
	})?;
	pb.inc(1);
	db.exec("commit")?;
	Ok(())
}
// II add
// III compile
/// Before saving: fills the resref translation table.
fn translate_resrefs(db: &mut Connection)->Result<()> {
	crate::progress::transaction(db, |db|{
	let mut enum_st = db.prepare(r#"select key from "resref_dict" where "resref" is null"#)?;
	let mut enum_rows = enum_st.query(())?;
	let mut find = db.prepare(r#"select 1 from "resref_orig" where "resref"=?1 union select 1 from "resref_dict" where "resref"=?1"#)?;
	let mut update = db.prepare(r#"update "resref_dict" set "resref"=?2 where "key"=?1"#)?;
	let mut n_resrefs = 0;
	while let Some(row) = enum_rows.next()? {
		let longref = row.get::<_,String>(0)?;
		// TODO: remove spaces etc.
		let resref = Resref::fresh(&longref, |r| find.exists((r,)))?;
		update.execute((&longref, &resref))?;
		n_resrefs+= 1;
	}
	info!("translated {n_resrefs} resrefs");
	Ok(())
	})?; Ok(())
}
/// Before saving: fills the strref translation tables.
fn translate_strrefs(db: &mut Connection)->Result<()> {
	crate::progress::transaction(db,|db| {
	db.exec(r#"delete from "strref_dict" where "key" not in (select "key" from "new_strings" where "key" is not null)"#)
		.context("cannot purge stale string keys")?;
	db.exec(r#"insert into "string_keys" select "key", "flags" from "new_strings" where "key" is not null"#)
		.context("cannot generate new string keys")?;
	info!("translated strrefs");
	Ok(())
	})?; Ok(())
}
/// Saves game resources to filesystem.
///
/// This wraps [`save_resources`] so that any
/// exception throws do not prevent changing back to the previous
/// directory:
fn command_save(game: &GameIndex, mut db: Connection)->Result<()> {
	let pb = Progress::new(4, "save all"); pb.as_ref().tick();
	translate_resrefs(&mut db)?;
	pb.inc(1);
	translate_strrefs(&mut db)?;
	pb.inc(1);
	gamestrings::save(&db, game)?;
	pb.inc(1);
	let tmpdir = Path::new(&game.root).join("simod_out");
	fs::create_dir(&tmpdir)
		.with_context(|| format!("cannot create temp directory {tmpdir:?}"))?;
	let orig_dir = std::env::current_dir()?;
	std::env::set_current_dir(tmpdir)?;
	let res = save_resources(&db);
	std::env::set_current_dir(orig_dir)?;
	pb.inc(1);
	res
}
/// Saves all modified game resources to game files.
///
/// This function saves in the current directory;
/// a chdir to the appropriate override directory needs to have been done
/// first.
fn save_resources(db: &Connection)->Result<()> {
	Item::save_all(db)?;
	Ok(())
}
// IV others: show etc.
fn command_show(db: Connection, target: impl AsRef<str>)->Result<()> {
	let target = target.as_ref();
	let (resref, resext) = match target.rfind('.') {
		None => return Ok(()),
		Some(i) => (&target[..i], &target[i+1..]),
	};
	match resext.to_lowercase().as_ref() {
		"itm" => Item::show_all(&db, resref),
		x => { warn!("unknown resource type: {x}"); Ok(()) },
	}
}
/// Restores all backed up files.
fn command_restore(game: &GameIndex)->Result<()> {
	for x in game.backup.read_dir()
		.with_context(|| format!("cannot read backup directory {:?}", game.backup))? {
		let path = x
			.with_context(|| format!("cannot read file in backup directory {:?}", game.backup))?
			.path();
		let ext = path.extension().unwrap().to_str().unwrap();
		match ext {
			"tlk" => {
				let mut lang = path.file_stem().unwrap().to_str().unwrap();
				let mut tail = String::new();
				if lang.as_bytes()[lang.len()-1] == b'F' {
					lang = &lang[..lang.len()-1];
					tail.push('F');
				}
				let dest = game.root.join("lang").join(lang)
					.join(format!("dialog{tail}.tlk"));
				fs::copy(&path, &dest)?;
				info!("restored {path:?} to {dest:?}");
			},
			_ => { // override case
				let dest = game.root.join("override").join(path.file_name().unwrap());
				fs::copy(&path, &dest)?;
				info!("restored {path:?} to {dest:?}");
			}
		};
	}
	Ok(())
}

/// The runtime options parser for `clap` crate.
#[derive(clap::Parser,Debug)]
#[command(version,about=r#"Exposes Infinity Engine game data as a SQLite database."#)]
struct RuntimeOptions {
	#[arg(short='G',long,help="sets the game directory (containing chitin.key)", default_value=".")]
	gamedir: PathBuf,
	#[arg(short='F',long,help="sets the sqlite file")]
	database: Option<PathBuf>,
	#[arg(short='O',long,help="sets log output",default_value="simod.log")]
	log_output: String,
	#[arg(short='L',long,help="sets log level",default_value_t=0)]
	log_level: i32,
	#[command(subcommand)]
	command: Command,
}
/// Subcommands passed to the executable.
#[derive(clap::Subcommand,Debug)]
enum Command {
	Init,
	Save,
	Restore,
	Add {
		target: String,
	},
	Remove,
	Select,
	Show {
		target: String,
	},
}

fn main() -> Result<()> {
	let mut options = RuntimeOptions::parse();
	if options.database.is_none() {
		options.database = Some(Path::new("game.sqlite").into());
	}
	let game = GameIndex::open(&options.gamedir)
		.with_context(|| format!("cannot initialize game from directory {:?}",
		options.gamedir))?;
	let loglevel = match options.log_level {
		0 => simplelog::LevelFilter::Off,
		1 => simplelog::LevelFilter::Error,
		2 => simplelog::LevelFilter::Warn,
		3 => simplelog::LevelFilter::Info,
		4 => simplelog::LevelFilter::Debug,
		_ => simplelog::LevelFilter::Trace,
	};
	if options.log_output == "-" {
		simplelog::TermLogger::init(loglevel,
			simplelog::Config::default(),
			simplelog::TerminalMode::Stderr,
			simplelog::ColorChoice::Auto)
	} else {
			simplelog::WriteLogger::init(loglevel,
			simplelog::Config::default(),
			fs::File::create(&options.log_output)
				.with_context(|| format!("cannot open log file {}", options.log_output))?)
	}.context("cannot set up logging")?;
	let db_file = options.database.unwrap();
	match options.command {
		Command::Init => load(create_db(&db_file, &game)?, &game)?,
		Command::Save => command_save(&game, database::open(db_file)?)?,
		Command::Restore => command_restore(&game)?,
		Command::Show{ target, .. } =>
			command_show(database::open(db_file)?, target)?,
		Command::Add{ target, .. } =>
			lua_api::command_add(database::open(db_file)?, &target)?,
		_ => todo!(),
	};
	info!("execution terminated with flying colors");
	Ok(())
}
