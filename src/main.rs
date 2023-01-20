#![allow(
	unused_imports,
// 	unreachable_code,
// 	unused_macros,
	unused_variables,
// 	dead_code,
// 	unused_must_use,
// 	unused_mut,
)]

mod gameindex {
/// This mod contains code related to the KEY/BIF side of the database.
/// Main interface:
///  - `Pack` trait;
///  - `Resref`, `Strref` basic types;
///  - `Gameindex` type and iterator.
use macros::Pack;
use std::cmp::min;
use std::fmt::{self,Display,Debug,Formatter};
use std::fs::{self, File};
use std::io::{self, Read, Write, Seek, SeekFrom, BufReader, Cursor};
use std::path::{Path, PathBuf};
use anyhow::{Result, Context};

#[derive(Clone,Copy)]
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
		let n = min(s.len(), N-1);
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
impl<const N: usize> Display for StaticString<N> {
	fn fmt(&self, f:&mut Formatter) -> fmt::Result {
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
impl<const N: usize> Debug for StaticString<N> {
	fn fmt(&self, f:&mut fmt::Formatter) -> fmt::Result { Display::fmt(self, f) }
}
impl<const N: usize> Pack for StaticString<N> {
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

pub trait Pack: Sized {
	fn unpack(f: &mut impl Read)->io::Result<Self>;
	fn pack(self, _f: &mut impl io::Write)->io::Result<()> { unimplemented!() }

	// associated functions:
	fn read_bytes(f: &mut impl Read, n: usize)->io::Result<Vec<u8>> {
		let mut buf = vec![0u8; n];
// 		let mut buf = Vec::<u8>::with_capacity(n);
// 		unsafe { buf.set_len(n); }
		f.read_exact(&mut buf)?; Ok(buf)
	}
	fn unpack_header(f: &mut impl Read, hdr: &str)->io::Result<()> {
		let buf = Self::read_bytes(f, hdr.len())?;
		assert_eq!(&buf[..], hdr.as_bytes());
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
#[derive(Clone,Copy)] pub struct Resref { pub name: StaticString::<8>, }
impl Debug for Resref {
	fn fmt(&self, f:&mut Formatter)->fmt::Result { Debug::fmt(&self.name, f) }
}
impl Display for Resref {
	fn fmt(&self, f:&mut Formatter)->fmt::Result {Display::fmt(&self.name, f)}
}
impl Pack for Resref {
	fn unpack(f: &mut impl Read)->io::Result<Self> {
		let mut name = StaticString::<8>::unpack(f)?;
		name.bytes.make_ascii_lowercase();
		Ok(Self { name, })
	}
}
#[derive(Debug,Pack,Clone,Copy)] pub struct Strref { pub value: i32, }
#[derive(Debug,Pack,Clone,Copy,PartialEq,Eq)] pub struct Restype { pub value: u16, }
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
	pub gamedir: &'a str,
	pub bifnames: Vec<String>,
	_bifsizes: Vec<u32>,
	pub resources: Vec<KeyRes>,
}

impl<'a> GameIndex<'a> {
	pub fn open(gamedir: &'a str)->Result<Self> {
		let indexfile = Path::new(&gamedir).join("chitin.key");
		let mut f = File::open(&indexfile)
			.with_context(|| format!("cannot open game index: {}",
				indexfile.to_str().unwrap()))?;
		let hdr = KeyHdr::unpack(&mut f)
			.with_context(|| format!("bad KEY header in file: {}",
				indexfile.to_str().unwrap()))?;
		let bifentries = KeyBif::vecunpack(&mut f, hdr.nbif as usize)
			.with_context(|| format!("could not read {} BIF entries in file: {}",
				hdr.nbif, indexfile.to_str().unwrap()))?;
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
			.with_context(|| format!("could not read {} BIF resources in file: {}",
				hdr.nres, indexfile.to_str().unwrap()))?;
		Ok(GameIndex{ gamedir, bifnames, resources, _bifsizes })
	}
}
#[derive(Debug)] pub struct GameIndexItr<'a> {
	index: &'a GameIndex<'a>,
	state: usize,
}
#[derive(Debug)] pub struct BifView<'a> { // element of GameIndex
	pub path: PathBuf,
	pub sourcefile: usize,
	pub resources: &'a Vec<KeyRes>,
	pub file: Option::<BufReader<File>>,
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

pub struct ResHandle<'a> {
	bif: &'a mut Option<BifFile>,
	path: &'a PathBuf,
	location: BifIndex,
	restype: Restype,
	pub resref: Resref,
}
impl<'a> ResHandle<'a> {
	pub fn open(&mut self)->Result<io::Cursor<Vec<u8>>> {
		biffile(self.bif, self.path)?;
		let biffile = self.bif.as_mut().unwrap();
		let j = self.location.resourceindex();
		let BifResource{ offset, size, restype, .. } = &biffile.resources[j];
		assert_eq!(restype, &self.restype);
		biffile.buf.seek(SeekFrom::Start(*offset as u64))?;
		Ok(Cursor::new(BifHdr::read_bytes(&mut biffile.buf, *size as usize)?))
	}
}

impl<'a> GameIndex<'a> {
	pub fn for_each<F>(&self, mut f: F)->Result<()>
	where F: (FnMut(Restype, ResHandle)->Result<()>) {
		for (sourcefile, filename) in self.bifnames.iter().enumerate() {
			let path = Path::new(self.gamedir).join(filename);
			let mut bif = Option::<BifFile>::default();
			for res in self.resources.iter() {
				let rread = ResHandle{ bif: &mut bif, path: &path,
					location: res.location, restype: res.restype, resref: res.resref, };
				if res.location.sourcefile() != sourcefile { continue }
				f(res.restype, rread)?
			}
		}
		Ok(())
	}
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
fn bifresources(file: impl AsRef<Path>+Debug)->io::Result<Vec<BifResource>> {
	let mut f = File::open(file)?;
	let hdr = BifHdr::unpack(&mut f)?;
	f.seek(SeekFrom::Start(hdr.offset as u64))?;
	BifResource::vecunpack(&mut f, hdr.nres as usize)
}
struct BifFile {
	buf: BufReader<File>,
	resources: Vec<BifResource>,
}
fn biffile<'a>(o: &'a mut Option<BifFile>, path: &'a (impl AsRef<Path>+Debug))->io::Result<&'a BifFile> {
	if o.is_none() {
		let resources = bifresources(path)?;
		let buf = BufReader::new(File::open(path)?);
		*o = Some(BifFile{buf, resources});
	}
	Ok(o.as_ref().unwrap())
}
} // mod gameindex
mod database {
/// This mod groups all code connected with the SQL side of the database.
/// Main exports are:
///  - `Schema` type: description of a particular SQL table;
///  - `Table` trait: connect a structure to a SQL row;
use std::marker::PhantomData;
use std::fs::{self};
use std::path::{self,Path,PathBuf};
use rusqlite::{Connection, Statement, Row, ToSql};
use rusqlite::types::{FromSql, ValueRef};
use crate::{Resref,Strref,gameindex::StaticString};
use extend::ext;
use anyhow::{Context,Result};
pub trait ConnectionExt {
	fn exec(&self, s: impl AsRef<str>)->Result<()>;
}
impl ConnectionExt for Connection {
	fn exec(&self, s: impl AsRef<str>)->Result<()> {
		self.execute(s.as_ref(), ())?; Ok(())
	}
}
pub trait RowExt {
	fn dump(&self);
}
impl<'a> RowExt for Row<'a> {
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

pub trait SqlType { const SQL_TYPE: FieldType; }
impl<T> SqlType for Option<T> where T: SqlType {
	const SQL_TYPE: FieldType = T::SQL_TYPE;
}
macro_rules! sqltype_int {
	($($T:ty),*) => { $(impl SqlType for $T {
		const SQL_TYPE: FieldType = FieldType::Integer;
	})* }
}
sqltype_int!{i8,i16,i32,i64,u8,u16,u32,u64}
// impl<T> SqlType for T where T: num::Integer {
// 	const SQL_TYPE: FieldType = FieldType::Integer;
// }
impl SqlType for Strref {
	const SQL_TYPE: FieldType = FieldType::Strref;
}
impl ToSql for Strref {
	fn to_sql(&self)->rusqlite::Result<rusqlite::types::ToSqlOutput> { self.value.to_sql() }
}
impl FromSql for Strref {
	fn column_result(v: ValueRef)->rusqlite::types::FromSqlResult<Self> {
		i32::column_result(v).map(|x| Strref { value: x })
	}
}
impl SqlType for Resref {
	const SQL_TYPE: FieldType = FieldType::Resref;
}
impl ToSql for Resref {
	fn to_sql(&self)->rusqlite::Result<rusqlite::types::ToSqlOutput> { self.name.as_ref().to_sql() }
}
impl FromSql for Resref {
	fn column_result(v: ValueRef)->rusqlite::types::FromSqlResult<Self> {
		match v {
			ValueRef::Text(s) => Ok(Resref { name: s.into() }),
			ValueRef::Null => Ok(Resref { name: "".into() }),
			_ => Err(rusqlite::types::FromSqlError::InvalidType)
		}
	}
}

#[derive(Debug,Clone,Copy)]
pub enum FieldType { Integer, Text, Resref, Strref, Other }
impl FieldType {
	pub const fn affinity(self)->&'static str {
		match self {
			FieldType::Integer | FieldType::Strref => "integer",
			FieldType::Text | FieldType::Resref => "text",
			FieldType::Other => "null",
		}
	}
}
#[derive(Debug)] pub struct Column<'a> {
	pub fieldname: &'a str,
	pub fieldtype: FieldType,
	pub extra: &'a str,
}
#[derive(Debug)] pub struct Schema<'a> {
	/// The main struct performing per-resource SQL operations.
	/// This contains all relevant information to fully define a resource
	/// on the SQL side.
	/// In practice there exists exactly one `Schema` instance per
	/// resource, and it is compiled by the `Table` derive macro.
	pub table_name: &'a str,
	pub primary_key: &'a str,
	pub parent_key: &'a str,
	pub parent_table: &'a str,
	pub fields: &'a[Column<'a>], // fat pointer
}
impl<'schema> Schema<'schema> {
	// Step 0: a few useful functions
	fn write_columns_pre(&self, s: &mut String, prefix: &str) {
		let mut isfirst = true;
		use std::fmt::Write;
		for Column { fieldname, .. } in self.fields.iter() {
			if isfirst { isfirst = false; } else { s.push(','); }
			write!(s, r#" {prefix}"{fieldname}" "#).unwrap();
		}
	}
	fn write_columns(&self, s: &mut String) {
		self.write_columns_pre(s, "")
	}
	// Step 1: creating tables
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
		let mut cols = String::new(); self.write_columns(&mut cols);
		{ // create main view
		let mut view = format!(r#"create view "{name}" as
	with "u" as (select {cols} from "res_{name}" union select {cols} from "add_{name}") select "#);
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
			FieldType::Strref => format!(
	r#"insert or ignore into "strref_dict" values (new."{fieldname}", null);"#),
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
			((select "component" from "current"), new."{key}", '{fieldname}',
			new."{fieldname}");
	end"#);
			db.exec(trig)?;
		}
		let mut newcols = String::new();
		self.write_columns_pre(&mut newcols, "new.");
		let trig = format!(
	r#"create trigger "insert_{name}"
	instead of insert on "{name}"
	begin
		{trans_edit}
		insert into "add_{name}" ({cols}) values ({newcols});
		insert or ignore into "{dirtytable}" values (new."{parent_key}");
	end"#);
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
		Ok (())
	}
	// Step 2: populating tables
	pub fn insert_query(&self)->String {
		let table_name = &self.table_name;
		let mut s = format!("insert into \"res_{table_name}\" (");
		self.write_columns(&mut s);
		s.push_str(") values (");
		for c in 0..self.fields.len() {
			if c > 0 { s.push(','); }
			s.push('?');
		}
		s.push(')');
		return s
	}
// 	pub fn insert<'b>(&'b self, db: &'b Connection, name: &str)->Result<Statement> {
// 		db.prepare(self.insert_query(name).as_ref())
// 		.map_err(|e| e.into())
// 	}
	// Step 3: retrieving data
// 	fn select_query(&self, name: &str)->String {
// 		use std::fmt::Write;
// 		let mut s = String::from("select ");
// 		self.write_columns(&mut s);
// 		write!(&mut s, "\nfrom \"{name}\" ").unwrap();
// 		s
// 	}
	pub fn compile_query(&self)->String {
		use std::fmt::Write;
		let name = &self.table_name;
		let Self { parent_table, parent_key, fields, .. } = self;
		let mut select = String::from("select\n  ");
		let mut source = String::from("\nfrom (select\n  ");
		let mut isfirst = true;
		#[inline] fn trans(dest: &mut String, what: &str, f: &str) {
			write!(dest, r#""{f}", (select "{what}" from "{what}_dict" as "o"
			where "o"."key" = "s"."{f}") as "trans_{f}""#).unwrap();
		}
		for Column {fieldname: f,fieldtype,..} in fields.iter() {
			if !isfirst {
				write!(&mut select, ",\n  ").unwrap();
				write!(&mut source, ",\n  ").unwrap();
			} else { isfirst = false; }
			match fieldtype {
		FieldType::Resref => {
			trans(&mut source, "resref", f);
// 			"{f}" as "a_{f}", "trans_{f}",
			write!(&mut select, r#"
			case when exists (select 1 from "resref_orig" as "o" where "o"."resref" = "t"."{f}") then "{f}" else "trans_{f}" end as "{f}""#).unwrap();
		},
		FieldType::Strref => {
			trans(&mut source, "strref", f);
 //"{f}" as "a_{f}", "trans_{f}",
			write!(&mut select, r#"
			case when typeof("{f}") == 'integer' then "{f}" else ifnull("trans_{f}", "{f}") end as "{f}""#).unwrap();
		},
		_ => {
			write!(&mut source, r#""{f}""#).unwrap();
			write!(&mut select, r#""{f}""#).unwrap();
		},
			}
		}
		let mut condition = String::new();
		if parent_table == name {
			write!(&mut condition, r#"where exists (select 1 from "dirty_{parent_table}" where "name" = "s"."{parent_key}")"#).unwrap();
		} else {
			write!(&mut condition, r#"where "{parent_key}"=?"#).unwrap();
		}
		write!(&mut select, r#"{source} from "{name}" as "s" {condition}) as "t""#).unwrap();
		select
	}
}

pub trait Table: Sized {
	type KeyIn;
	type KeyOut;
	type Res; // always == anyhow::Result<(Self, Self::KeyOut)>;
	// TODO: Res is more useful than KeyOut, replace KeyOut by Res
	const SCHEMA: Schema<'static>;
	fn ins(&self, s: &mut Statement, key: &Self::KeyIn)->rusqlite::Result<()>;
	fn sel(r: &Row)->rusqlite::Result<(Self, Self::KeyOut)>;
// 	fn find_field<T>(s: &AllResources<T>)->&T;
// 	fn find_field_mut<T>(s: &mut AllResources<T>)->&mut T;
	fn select_statement(db: &Connection)->rusqlite::Result<TypedStatement<'_, Self>> {
		let s = Self::SCHEMA.compile_query();
		Ok(TypedStatement(db.prepare(&s)?, PhantomData::<Self>))
	}
}
// We need a few types parametrized by a Table:
//  - `TypedStatement`: this gets saved as a (mut) local variable;
//  - `TypedRows`: the iterator producing Table objects from the query.
pub struct TypedStatement<'stmt, T: Table> (Statement<'stmt>, PhantomData<T>);
impl<'stmt, T: Table> TypedStatement<'stmt, T> {
	pub fn as_table<P: rusqlite::Params>(&mut self, params: P)->rusqlite::Result<TypedRows<T>> {
		let rows = self.0.query(params)?;
		Ok(TypedRows { rows, _marker: PhantomData::<T>, index: 0 })
	}
}
pub struct TypedRows<'stmt,T: Table> {
	/// An enriched version of `rusqlite::Rows`, retaining information about
	/// the columns of the query, as well as current row index.
	/// This also behaves as an `Iterator` (throwing when the underlying
	/// `Row` iterator fails).
	rows: rusqlite::Rows<'stmt>,
	_marker: PhantomData<T>,
	index: usize,
}
impl<'stmt,T: Table> Iterator for TypedRows<'stmt,T> {
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

pub use rusqlite;
mod gametypes;
mod constants;

use database::{Table, ConnectionExt, TypedStatement};
use gameindex::{GameIndex, Pack, Strref, Resref, ResHandle};
use gametypes::*;

fn type_of<T>(_:&T)->&'static str { std::any::type_name::<T>() }
use std::fmt::{self,Display,Debug};
use std::io::{Seek, SeekFrom};
use std::path::{Path, PathBuf};
use std::fs;
use rusqlite::{Connection,Statement};
use extend::ext;
pub(crate) use anyhow::{Context, Result};

// I. create DB
use crate::gametypes::*;
pub fn create_db(db_file: &str)->Result<Connection> {
	println!("creating file {}", db_file);
	if Path::new(db_file).exists() {
		fs::remove_file(db_file)
		.with_context(|| format!("cannot remove file: {}", db_file))?;
	}
	let db = Connection::open(db_file)?;
	println!("creating global tables.. ");
	db.execute_batch(r#"
create table "current" ("component" text);
create table "resref_orig" ("resref" text primary key);
create table "resref_dict" ("key" text not null primary key on conflict ignore, "resref" text);
create table "strref_dict" ("key" text not null primary key on conflict ignore, "strref" integer);
	"#)?;
	println!("creating per-resource tables.. ");
	RESOURCES.map(|schema,_| {
		println!("  creating for resource {}", schema.table_name);
		schema.create_tables_and_views(&db)?; Result::<()>::Ok(())
	})?;
	Ok(db)
}

// II. Game -> SQL
struct DbInserter<'a> {
	db: &'a Connection,
	add_resref: Statement<'a>,
	tables: AllResources<Statement<'a>>,
}

fn populate(db: &Connection, game: &GameIndex)->Result<()> {
	db.exec("begin transaction")?;
	let mut base = DbInserter { db,
		tables: RESOURCES.map(|schema, _| db.prepare(&schema.insert_query()) )?,
		add_resref: db.prepare(r#"insert or ignore into "resref_orig" values (?)"#)?
	};
// 	let mut insert = TypedInserts::from(db)?;
	game.for_each(|restype, mut handle| {
		match restype {
		constants::RESTYPE_ITM =>
			populate_item(&mut base, &mut handle),
		_ => Ok(())
		}
	})?;
	db.exec("commit")?;
	Ok(())
}
fn populate_item(db: &mut DbInserter, handle: &mut ResHandle)->Result<()> {
	let mut cursor = handle.open()?;
	let resref = handle.resref;
	let item = Item::unpack(&mut cursor)?;
	item.ins(&mut db.tables.items, &(resref,))?;
	db.add_resref.execute((&resref,))?;

	cursor.seek(SeekFrom::Start(item.abilities_offset as u64))?;
	// TODO zip those vectors
	let mut ab_n = Vec::<u16>::with_capacity(item.abilities_count as usize);
	let mut ab_i = Vec::<i64>::with_capacity(item.abilities_count as usize);
	for _ in 0..item.abilities_count {
		let ab = ItemAbility::unpack(&mut cursor)?;
		ab.ins(&mut db.tables.item_abilities, &(Some(resref),))?;
		ab_n.push(ab.effect_count);
		ab_i.push(db.db.last_insert_rowid());
	}
	println!("inserting item {resref}: {ab_n:?} {ab_i:?}");
	cursor.seek(SeekFrom::Start(item.effect_offset as u64))?;
	for _ in 0..item.equip_effect_count { // on-equip effects
		let eff = ItemEffect::unpack(&mut cursor)?;
		// TODO swap with Some above!!
		eff.ins(&mut db.tables.item_effects, &(resref, -1i64))?;
	}
	for (i, n) in ab_n.iter().enumerate() {
		for _ in 0..*n {
			let eff = ItemEffect::unpack(&mut cursor)?;
			eff.ins(&mut db.tables.item_effects, &(resref, ab_i[i]))?;
		}
	}
	Ok(())
}

// III. SQL -> Game
trait DbTypeCheck {
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

fn save(db: &Connection, game: &GameIndex)->Result<()> {
	let tmpdir = Path::new(&game.gamedir).join("sim_out");
// 	fs::create_dir(&tmpdir)
// 		.with_context(|| format!("cannot create temp directory {:?}", tmpdir))?;
	// TODO: generate strings & resrefs
	//
	let mut sel_item = Item::select_statement(db)?;
	let mut sel_item_ab = ItemAbility::select_statement(db)?;
	let mut sel_item_eff = ItemEffect::select_statement(db)?;
	for x in sel_item.as_table(())? {
		save_item(x, &mut sel_item_ab, &mut sel_item_eff, &tmpdir)?;
	}
	Ok(())
}
fn save_item(x: <Item as Table>::Res, sel_item_ab: &mut TypedStatement<'_,ItemAbility>, sel_item_eff: &mut TypedStatement<'_,ItemEffect>, tmpdir: &Path)->Result<()>{
	// ignore malformed db input (but report it on stdout)
	if x.is_db_malformed() { return Ok(()) }
	let (mut item, (itemref,)) = x?;
	println!("got an item! {itemref}");
	// abilities = (ability, abref)
	// effect = (effect, ability index)
	const NO_ABILITY: usize = usize::MAX;
	let mut abilities = Vec::<(ItemAbility, i64)>::new();
	let mut effects = Vec::<(ItemEffect, usize)>::new();
	for x in sel_item_ab.as_table((&itemref,))? {
		if x.is_db_malformed() { return Ok(()) }
		let (ab, (_, abref)) = x?;
		abilities.push((ab, abref));
	}
	for x in sel_item_eff.as_table((&itemref,))? {
		if x.is_db_malformed() { return Ok(()) }
		let (eff, (_, parent)) = x?;
		let ab_id = match abilities.iter().position(|&(_, abref)| abref == parent) {
			Some(i) => { abilities[i].0.effect_count+= 1; i },
			None    => { item.equip_effect_count+= 1; NO_ABILITY},
		};
		effects.push((eff, ab_id));
	}
	let mut current_effect_idx = item.equip_effect_count;
	for (a, _) in abilities.iter_mut() {
		a.effect_index = current_effect_idx;
		current_effect_idx+= a.effect_count;
	}
	println!("item has {} abilities and {} effects", abilities.len(),
		effects.len());
	for (i, (e, a)) in effects.iter().enumerate() {
		println!("  effect {i} (opcode {} parameters {},{}) belongs to ability {a}",
			e.opcode, e.parameter1, e.parameter2);
	}
	println!("item has {} global effects", item.equip_effect_count);
	for (i, (a, _)) in abilities.iter().enumerate() {
		println!("  ability {i} has {} effects, starting at {}",
			a.effect_count, a.effect_index);
	}
	item.abilities_offset = 114;
	item.effect_offset = 114 + 56*(item.abilities_count as u32);
// 	pack(item)
// 	pack(item abilities)
// 	pack(item effects grouped by associated ability)
	Ok(())
}

const DB_FILE: &str = "game.sqlite";

fn main() -> Result<()> {
	let gamedir = "/home/jerome/jeux/bg/game";
	let game = GameIndex::open(gamedir)
		.with_context(|| format!("could not initialize game from directory {}",
		gamedir))?;
// 	println!("{:?}", RESOURCES.items.schema); return Ok(());
	let db = create_db(DB_FILE)?;
	populate(&db, &game)?;
	db.execute_batch(r#"
update "items" set price=5 where itemref='sw1h34';

insert into "resref_dict" values
('New Item!', 'NEWITEM');

insert into "items" ("itemref", "name", "replacement", "ground_icon") values
('New Item!', 'New Item name!', 'Replacement', 'new icon');

update 'resref_dict'
set "resref" = 'REPLAC' where "key" = 'Replacement';

insert into "strref_dict" values
('New Item name!', 35001);

insert into "resref_orig" values
('isw1h01'), ('gsw1h01'), ('csw1h01');
	"#)?;
// 	let db = Connection::open(DB_FILE)?;
	save(&db, &game)?;
	Ok(())
}
