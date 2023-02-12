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
	single_use_lifetimes,
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

pub(crate) mod prelude {
//! The set of symbols we want accessible from everywhere in the crate.
#![allow(unused_imports)]
pub(crate) use anyhow::{Result,Context};
pub(crate) use log::{trace,debug,info,warn,error};
pub(crate) use rusqlite::{self,Connection,Statement,Rows};
pub(crate) use extend::ext;
pub(crate) use lazy_format::prelude::*;

pub(crate) use core::marker::PhantomData;
pub(crate) use std::ops::{Deref};
pub(crate) use std::fmt::{self,Display,Debug,Formatter,Write as FWrite};
pub(crate) use std::fs::{self,File};
pub(crate) use std::io::{self,Cursor,Read,Write,Seek,SeekFrom};
pub(crate) use std::path::{Path, PathBuf};

pub(crate) use macros::{Pack,Resource0,SqlRow,Newtype};
pub(crate) use crate::gamefiles::{Resref,Strref};
pub(crate) use crate::database::{GameDB,DbInterface};
pub(crate) use crate::progress::{Progress,scope_trace,NullDisplay};

pub(crate) fn any_ok<T>(x: T)->Result<T> { Ok(x) }

#[derive(Debug)]
pub(crate) enum Error{
	UnknownTable(String),
	BadArgumentNumber { function: &'static str, expected: usize, found: usize },
	BadArgumentType { expected: &'static str },
// 	BadType(String),
// 	BadArgumentNumber(usize, &'static str),
	CallbackMissingArgument,
	Any(String),
}
impl Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>)->fmt::Result {
		match self {
			Self::UnknownTable(s) => write!(f, "Unknown table: '{s}'"),
			Self::BadArgumentNumber { function, expected, found } =>
				write!(f, "Bad number of arguments for function '{function}': found {found}, expected {expected}"),
			Self::BadArgumentType { expected } =>
				write!(f, "Bad argument type: expected {expected}"),
			_ => write!(f, "Error: &{self:?}"),
		}
	}
}
impl std::error::Error for Error { }

/// Just like `write!` except that it is unwrapped.
macro_rules! uwrite { ($($a:tt)*) => { write!($($a)*).unwrap() } }
macro_rules! uwriteln { ($($a:tt)*) => { writeln!($($a)*).unwrap() } }
pub(crate) use {uwrite,uwriteln};

}
pub(crate) mod progress {
//! Generic useful functions for user interaction.
//!
//! This contains among other:
//!  - [`Progress`], a utility wrapper for a progress bar stack;
use crate::prelude::*;
use std::cell::{RefCell};
use indicatif::{ProgressBar,ProgressStyle,MultiProgress};

/// An empty struct with a trivial [`Display`] implementation.
#[derive(Debug,Clone,Copy)] pub struct NullDisplay();
impl Display for NullDisplay {
	fn fmt(&self, _: &mut Formatter<'_>)->fmt::Result { Ok(()) }
}

thread_local! {
	static COUNT: RefCell<usize> = RefCell::new(0);
	static MULTI: RefCell<MultiProgress> = RefCell::new(MultiProgress::new());
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
		debug!(r#"finished task "{name}" in {elapsed:?} »»"#,
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
		debug!(r#"start task "{name}" ««"#);
		Self { pb, name, }
	}
	/// Advances the progress bar by the given amount.
	pub fn inc(&self, n: u64) { self.as_ref().inc(n) }
}

/// A struct producing balanced, foldable log messages.
///
/// This produces a log message with a `««` marker when created and the
/// corresponding `»»` marker when dropped. This enables using folding in
/// the editor when viewing log messages.
pub struct ScopeLogger(pub log::Level);
impl Drop for ScopeLogger {
	fn drop(&mut self) { log::log!(self.0, "»»") }
}
macro_rules! scope_trace {
	($msg: literal $($arg:tt)*) => {
		let _log_scope = crate::progress::ScopeLogger(log::Level::Trace);
		log::trace!(concat!($msg, "««") $($arg)*);
	}
}
pub(crate) use scope_trace;
} // mod progress
pub(crate) mod pack {
use crate::prelude::*;
/// A type which may be packed to binary.
///
/// This trait is implemented by the derive macro.
/// The macro may be applied to both named and unnamed structs,
/// as well as (in some limited cases) for parametrized structs
/// (in which case it will require that all fields implement `Pack`).
pub trait Pack: Sized {
	/// Reads this object from a binary source.
	fn unpack(f: &mut impl Read)->io::Result<Self>;
	/// Writes this objects to a binary sink.
	fn pack(&self, _f: &mut impl Write)->io::Result<()> {
		error!("Missing Pack for type: {}", crate::type_of(&self));
		unimplemented!() }
	// associated functions:
	/// Reads a vector of objects (of known size) from a binary source.
	fn vecunpack(mut f: &mut impl Read, n: usize)->io::Result<Vec<Self>> {
		(0..n).map(|_| { Self::unpack(&mut f) }).collect()
	}
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
/// A newtype which is pass-through for SQL and blocking for [`Pack`].
#[derive(Newtype)]
pub struct NotPacked<T>(T);
impl<T: Default> Pack for NotPacked<T> {
	fn unpack(_r: &mut impl Read)->io::Result<Self> { Ok(Self(T::default())) }
	fn pack(&self, _w: &mut impl Write)->io::Result<()> { Ok(()) }
}

}
pub(crate) mod struct_io {
//! Basic types for interaction with SQL and binary files.
//! The main entry points for this module are the traits [`Pack`]
//! and [`SqlRow`].
use crate::prelude::*;
use rusqlite::{ToSql};
use rusqlite::types::{ToSqlOutput,FromSql};
use crate::schemas::{Field,ColumnWriter};
use crate::pack::{Pack,NotPacked};

pub trait RowExt {
	fn dump(&self);
}
impl RowExt for rusqlite::Row<'_> {
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
/// An utility enum defining SQL behaviour for a given resource field type.
///
/// This is somewhat different from [`rusqlite::types::Type`]:
///  - not all SQLite types exist here;
///  - the `Strref` variant can accept either a string or an integer,
/// etc.
#[derive(Debug,Clone,Copy,PartialEq)]
#[non_exhaustive]
pub enum FieldType {
	/// Plain integer.
	Integer,
	/// Plain text.
	Text,
	/// A resource reference: a string translated using `resref_dict`.
	Resref,
	/// A string reference: either an integer or a string translated using
	/// `strref_dict`.
	Strref,
	Rowid,
}
impl FieldType {
	pub const fn affinity(self)->&'static str {
		match self {
			Self::Rowid |
			Self::Integer | Self::Strref => r#"integer default 0"#,
			Self::Text | Self::Resref => r#"text default """#,
		}
	}
	pub const fn to_lua(self)->&'static str {
		match self {
			FieldType::Integer => "integer",
			FieldType::Text => "text",
			FieldType::Resref => "resref",
			FieldType::Strref => "strref",
			FieldType::Rowid => "auto",
		}
	}
}
/// A leaf SQL type: this can be converted from, to SQL and knows its own
/// affinity.
pub trait SqlLeaf: FromSql + ToSql {
	const FIELD_TYPE: FieldType;
}
impl SqlLeaf for Resref { const FIELD_TYPE: FieldType = FieldType::Resref; }
impl SqlLeaf for Strref { const FIELD_TYPE: FieldType = FieldType::Strref; }
// impl SqlLeaf for &str { const FIELD_TYPE: FieldType = FieldType::Text; }
impl SqlLeaf for String { const FIELD_TYPE: FieldType = FieldType::Text; }
macro_rules! sqlleaf_int { ($($T:ty),*) => { $(
	impl SqlLeaf for $T { const FIELD_TYPE: FieldType = FieldType::Integer; }
)* } }
sqlleaf_int!{i8,i16,i32,i64,u8,u16,u32,u64,usize}
impl<T> SqlLeaf for Option<T> where T: SqlLeaf {
	const FIELD_TYPE: FieldType = T::FIELD_TYPE;
}
/// Converting an object to something implementing [`rusqlite::Params`].
///
/// Since we cannot implement the [`rusqlite::Params`] trait ourselves,
/// this is the closest we can do.
pub trait AsParams {
	type Elt<'a>: ToSql where Self: 'a;
	type Iter<'a>: Iterator<Item=Self::Elt<'a>> where Self: 'a;
	fn params_iter(&self)->Self::Iter<'_>;
	fn as_params(&self)->rusqlite::ParamsFromIter<Self::Iter<'_>> {
		rusqlite::params_from_iter(self.params_iter())
	}
}
pub trait AsParams2: {
	type Iter<'a>: Iterator where Self: 'a;
	type Target<'a>: ToSql where Self: 'a;
	fn transform<'a>(a: <Self::Iter<'a> as Iterator>::Item)->Self::Target<'a>
		where Self: 'a;
	fn params_iter(&self)->Self::Iter<'_>;
	//The following associated type default is unstable:
// 	type Item<'a> = <Self::Iter<'a> as Iterator>::Item;
	// so we are forced to allow this:
	#[allow(clippy::type_complexity)]
	fn as_params2<'a>(&'a self)->rusqlite::ParamsFromIter<std::iter::Map<Self::Iter<'a>,fn(<Self::Iter<'a> as Iterator>::Item)->Self::Target<'a>>> {
		rusqlite::params_from_iter(self.params_iter().map(Self::transform))
	}
}
/// Converting an object to something implementing [`rusqlite::Params`].
///
/// Since we cannot implement the [`rusqlite::Params`] trait ourselves,
/// this is the closest we can do.
pub trait AsParams3 {
	type Elt<'a>: ToSql where Self: 'a;
	type Iter<'a>: Iterator<Item=Self::Elt<'a>> where Self: 'a;
	fn params_iter(&self)->Self::Iter<'_>;
	fn as_params(&self)->rusqlite::ParamsFromIter<Self::Iter<'_>> {
		rusqlite::params_from_iter(self.params_iter())
	}
}
/// A struct mapped to a SQL row.
pub trait SqlRow: Sized {
	/// The number of fields in the SQL row.
	const WIDTH: usize;
	/// Returns the `i`-th field of the resource,
	/// wrapped in a [`rusqlite::types::ToSqlOutput`]
	/// for trivial insertion into a SQL statement.
	/// **NOTE**: rusqlite does not allow pushing `Result<ToSqlOutput>`
	/// values back into a [`rusqlite::ParamsFromIter`] iterator,
	/// so we need to unwrap those values.
	/// Since we do so from a controlled struct, this should not panic;
	/// however, it would be better to be able to propagate errors.
	fn get_field(&self, i: usize)->ToSqlOutput<'_>;
	/// Reads a whole [`rusqlite::Row`] into a struct,
	/// or raises a conversion error.
	fn from_row_at(row: &rusqlite::Row<'_>, offset: usize)->Result<Self>;
	/// Returns the type of the `i`-th field.
	fn fieldtype(i: usize)->FieldType;
	/// Builds the name of the `i`-th field from context.
	fn fieldname_ctx(ctx: &'static str, i: usize)->&'static str;
	/// Any supplemental material introduced at table creation
	/// (constraints, etc.)
	fn field_create_text(_i: usize)->&'static str { "" }
	fn primary_index()->Option<usize>;
	/// Reads a whole [`rusqlite::Row`] into a struct.
	fn from_row(row: &rusqlite::Row<'_>,)->Result<Self> {
		Self::from_row_at(row, 0)
	}
	/// Returns the name of the `i`-th field, as a string.
	fn fieldname(i: usize)->&'static str { Self::fieldname_ctx("", i) }
	/// Return the `i`-th element of schema
	fn field(i: usize)->Field {
		Field { fname: Self::fieldname(i),
			ftype: Self::fieldtype(i), extra: Self::field_create_text(i) }
	}
	/// Returns the set of fields, as a vector.
	fn fields()->Vec<Field> {
		(0..Self::WIDTH).map(Self::field).collect()
	}
	/// The select Statement for an object.
	///
	/// The ID field (if present) is not selected. (This is used only for
	/// building game files, where this field is not used anyway).
	///
	/// The name of the table is `{n1}{n2}`; since many tables are built in
	/// two parts (e.g. `load_{name}`, `strings_{lang}`),
	/// this avoids calling `format!` on the
	/// caller side and slightly simplifies the API.
	///
	/// This returns a [`Result<TypedStatement>`]: it is possible to iterate
	/// over the result and recover structures of the original type.
	fn select_from_typed(db: &impl DbInterface, n: impl Display, cond: impl Display)->Result<TypedStatement<'_,Self>> {
		let t = Self::fields().cols().select_sql(n, cond);
		Ok(TypedStatement::new(db.prepare(&t)?))
	}
}
impl<T: SqlLeaf> SqlRow for T {
	const WIDTH: usize = 1;
	fn from_row_at(row: &rusqlite::Row<'_>, offset: usize)->Result<Self> {
		row.get::<_,T>(offset)
			.with_context(|| format!("cannot read SQL value as {}",
				std::any::type_name::<T>()))
	}
	fn get_field(&self, i: usize)->ToSqlOutput<'_> {
		assert_eq!(i, 0);
		self.to_sql()
			.expect("cannot convert to SQL")
	}
	fn fieldtype(_i: usize)->FieldType { Self::FIELD_TYPE }
	fn fieldname_ctx(ctx: &'static str, _i: usize)->&'static str { ctx }
	fn primary_index()->Option<usize> { None }
}
impl<T: SqlRow> AsParams for T {
	type Elt<'a> = ToSqlOutput<'a> where Self: 'a;
	type Iter<'a> = ContentIterator<'a,T> where Self: 'a;
	fn params_iter(&self)->Self::Iter<'_> { ContentIterator(0, self) }
}

/// An iterator over all content fields of a [`SqlRow`].
///
/// This iterates over the content of a struct.
pub struct ContentIterator<'a,T: SqlRow> (usize, &'a T);
impl<'a,T: SqlRow> Iterator for ContentIterator<'a,T> {
	type Item = ToSqlOutput<'a>;
	fn next(&mut self)->Option<Self::Item> {
		if self.0 >= T::WIDTH { return None }
		let f = self.1.get_field(self.0);
		self.0+= 1;
		Some(f)
	}
}
impl<T: SqlRow> ExactSizeIterator for ContentIterator<'_,T> {
	fn len(&self)->usize { T::WIDTH }
}

impl<T: SqlRow> SqlRow for NotPacked<T> {
	const WIDTH: usize = T::WIDTH;
	fn from_row_at(row: &rusqlite::Row<'_>, offset: usize)->Result<Self> {
		T::from_row_at(row, offset).map(Self::from)
	}
	fn get_field(&self, i: usize)->ToSqlOutput<'_> { self.as_ref().get_field(i) }
	fn fieldname_ctx(ctx: &'static str, i: usize)->&'static str {
		T::fieldname_ctx(ctx, i)
	}
	fn fieldtype(i: usize)->FieldType { T::fieldtype(i) }
	fn primary_index()->Option<usize> { T::primary_index() }
}
/// A wrapper which is pass-through for [`Pack`] and blocking for [`SqlRow`].
#[derive(Newtype,Pack)]
pub struct NoSql<T>(T);
impl<T: Default> SqlRow for NoSql<T> {
	const WIDTH: usize = 0;
	fn from_row_at(_row: &rusqlite::Row<'_>, _offset: usize)->Result<Self> {
		Ok(Self(T::default()))
	}
	fn get_field(&self, _i: usize)->ToSqlOutput<'_> {
		panic!("attempted to insert a NoSql value")
	}
	fn fieldtype(_i: usize)->FieldType {
		panic!("attempted to read field type of a NoSql value")
	}
	fn fieldname_ctx(_ctx: &'static str, _i: usize)->&'static str {
		panic!("attempted to read field name of a NoSql value")
	}
	fn primary_index()->Option<usize> {
		panic!("attempted to compute primary index of a NoSql value")
	}
}
impl<T,I> std::ops::AddAssign<I> for NoSql<T>
	where T: std::ops::AddAssign<I> {
	fn add_assign(&mut self, rhs: I) { self.0.add_assign(rhs) }
}

/// A statement aware of the associated table type.
///
/// We need a few types parametrized by a Resource0:
///  - `TypedStatement`: this gets saved as a (mut) local variable;
///  - `TypedRows`: the iterator producing Resource0 objects from the query.
#[derive(Debug)]
pub struct TypedStatement<'s, T: SqlRow> (Statement<'s>, PhantomData<T>);
impl<'stmt,T: SqlRow> TypedStatement<'stmt, T> {
	pub fn new(stmt: Statement<'stmt>)->Self { Self(stmt, PhantomData) }
	/// Iterates over this statement, returning (possibly) Rust structs of
	/// the type associated with this table.
	pub fn iter<P: rusqlite::Params>(&mut self, params: P) ->rusqlite::Result<TypedRows<'_,T>> {
		let rows = self.0.query(params)?;
		Ok(TypedRows { rows, _marker: PhantomData, index: 0 })
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
pub struct TypedRows<'stmt,T: SqlRow> {
	rows: Rows<'stmt>,
	_marker: PhantomData<T>,
	index: usize,
}
impl<T: SqlRow> Iterator for TypedRows<'_,T> {
	type Item = Result<T>;
	fn next(&mut self)->Option<Self::Item> {
		loop {
			let row = match self.rows.next() {
				Ok(Some(row)) => row,
				Ok(None) => break,
				Err(e) => return Some(Err(e.into()))
			};
			self.index+= 1;
			let t = T::from_row(row);
			match t {
			Err(e) => {
				println!("cannot read a {} from row {}: {e:?}",
				std::any::type_name::<T>(), self.index);
				row.dump();
				continue },
			_ => return Some(t)
			}
		}
		None
	}
}
} // mod struct_io
pub(crate) mod staticstrings {
//! Fixed-length string used as resource identifier.
//! 
//! This is *slightly* different from (TODO) standard implementations in
//! that (a) no ending zero is necessary (although one can be present,
//! thus shortening the string), and (b) these strings (used for indexing
//! game resources) are case-insensitive. (Since the game uses mostly
//! uppercase, we convert on purpose to lowercase: this facilitates
//! spotting bugs).
use std::fmt::{self,Debug,Display,Formatter};
use std::io::{self,Read,Write};
use std::cmp::min;
use crate::pack::Pack;

/// A fixed-length string.
#[derive(Clone,Copy)]
pub struct StaticString<const N: usize>([u8; N]);
impl<const N: usize> PartialEq<&str> for StaticString<N> {
	fn eq(&self, other: &&str) -> bool {
		for (i, c) in other.bytes().enumerate() {
			if i >= N { return false }
			if !c.is_ascii() { return false }
			if self.0[i] != c { return false }
			if c == 0u8 { return true }
		}
		true
	}
}
impl<const N: usize> PartialEq<StaticString<N>> for StaticString<N> {
	fn eq(&self, other: &StaticString<N>)->bool { self.0 == other.0 }
}
impl<const N: usize> From<&str> for StaticString<N> {
	fn from(s: &str) -> Self { Self::from(s.as_bytes()) }
}
impl<const N: usize> From<&[u8]> for StaticString<N> {
	fn from(s: &[u8])->Self {
		let mut bytes = [0u8; N];
		let n = min(s.len(), N);
		bytes[..n].copy_from_slice(&s[..n]);
		Self(bytes)
	}
}
impl<const N: usize> AsRef<str> for StaticString<N> {
	fn as_ref(&self)->&str {
		let r = self.0.iter().enumerate().find_map(|(i, c)| {
			if *c == 0 { Some(i) } else { None } });
		let n = r.unwrap_or(N);
		std::str::from_utf8(&self.0[..n]).unwrap()
	}
}
impl<const N: usize> Debug for StaticString<N> {
	fn fmt(&self, f:&mut Formatter<'_>) -> fmt::Result {
		// since we write individual chars to `f`,
		// we need to bring into scope its `Write` implem.:
		use fmt::Write;
		f.write_char('"')?;
		for c in &self.0 {
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
/// This Pack implementation is manual because (1) the derive macro has
/// some difficulties with const parameters, and (2) \[u8;N\] is not `Pack`
/// anyway.
impl<const N: usize> Pack for StaticString<N> {
	fn unpack(f: &mut impl Read)->io::Result<Self> {
		let mut x = [0u8; N];
		f.read_exact(&mut x)?;
		Ok(Self(x))
	}
	fn pack(&self, f: &mut impl Write)->io::Result<()> {
		f.write_all(&self.0)
	}
}
impl<const N: usize> Default for StaticString<N> {
	fn default()->Self { Self([0u8; N]) }
}
impl<const N: usize> StaticString<N> {
	pub fn make_ascii_lowercase(&mut self) { self.0.make_ascii_lowercase() }
}
/// A struct enumerating fixed-length strings of the following form:
/// x, x0, x1, .., x00, x01, .., x99, x000, x001, ...
///
/// we write additional numbers of l digits in the positions \[n-l:n-1\]
/// (where n ∈ \[0,7\] and l ∈ \[0,7\]):
#[derive(Debug)]
pub struct Generator<const N: usize> {
	buf: StaticString<N>,
	n: usize,
	l: usize,
	j: usize,
}
impl<const N: usize> Generator<N> {
	/// Initializes the generator from an input string.
	///
	/// The string is shortened to no more than N bytes, keeping only a
	/// small subset of characters guaranteed to be valid in resrefs.
	pub fn new(source: &str)->Self {
		let mut buf = StaticString::<N>::default();
		let mut n = 0;
		for c in source.as_bytes() {
			if c.is_ascii_alphanumeric() || "!#@-_".as_bytes().contains(c) {
				buf.0[n] = c.to_ascii_lowercase();
				n+= 1;
				if n >= 8 { break }
			}
		}
		Self { buf, n, l: 0, j: 0, }
	}
	/// Advances the generator, producing the next candidate string.
	pub fn next(&mut self)->&StaticString<N> {
// 		trace!("resref::fresh({source}), used {n} letters");
		self.j+= 1;
		if self.j > 111_111_111 {
			// This panics after all 111_111_111 possibilities have been
			// exhausted. Unlikely to happen irl (and then we cannot do much
			// useful either).
			// The last number written at any length is always (9*);
			// we detect this to increase the length.
			panic!("iteration exhausted");
		}
		let mut s = self.j+888_888_888;
			// we start inserting right-to-left from position n-1
			// (note: the decrement at the *beginning* of the loop prevents an
			// usize underflow)
		let mut i = self.n;
		let mut is_nines = true;
		for _ in 0..self.l {
			i-= 1;
			let c: u8 = (s % 10) as u8;
			s/= 10;
			is_nines = is_nines && (c == 9);
			self.buf.0[i] = 48u8 + c;
		}
		if is_nines {
			if self.n < 7 { self.n+= 1; }
			self.l+= 1;
		}
		&self.buf
	}
}
}// mod staticstrings
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
//!
//! Visibility barrier: this mod is the only place using the internals of
//! key/bif game files.
use crate::prelude::*;
use crate::pack::{Pack};
use rusqlite::types::{FromSql,ToSql, ValueRef};

use io::BufReader;

use crate::progress::{Progress};
use crate::staticstrings::{StaticString};

// I. Basic types: StaticString, Resref, Strref etc.
/// Binary I/O for game structures; implemented by derive macro.
///
/// We have a default implementation for fixed-width
/// integer types (little-endian) and for strings (they are ignored on
/// output and empty on input). For composite types, the implementation
/// is produces by the `[Pack]` derive macro.
/// A resource reference as used by the game: case-insensitive 8-bytes
/// ascii string.
///
/// (Note that, since `Pack` does not know how to work with
/// unnamed structs, we use a named struct here).
#[derive(Clone,Copy,Default)]
pub struct Resref { pub name: StaticString::<8>, }
impl Debug for Resref {
	fn fmt(&self, f:&mut Formatter<'_>)->fmt::Result { Debug::fmt(&self.name, f) }
}
impl Display for Resref {
	fn fmt(&self, f:&mut Formatter<'_>)->fmt::Result {Display::fmt(&self.name, f)}
}
impl Pack for Resref {
	fn unpack(f: &mut impl Read)->io::Result<Self> {
		let mut name = StaticString::<8>::unpack(f)?;
		name.make_ascii_lowercase();
		Ok(Self { name, })
	}
	fn pack(&self, f: &mut impl Write)->io::Result<()> { self.name.pack(f) }
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
	/// iterates until `test` returns `false`.
	pub fn fresh(source: &str, mut is_used: impl FnMut(&Resref)->rusqlite::Result<bool> )->rusqlite::Result<Self> {
		let mut gen = crate::staticstrings::Generator::<8>::new(source);
		loop {
			let resref = Resref { name: *gen.next() };
			if !is_used(&resref)? {
				return Ok(resref)
			}
		}
	}
}
/// A string reference as used by the game: 32-bit integer.
#[derive(Debug,Pack,Clone,Copy)]
pub struct Strref { value: i32, }
impl Display for Strref {
	fn fmt(&self, mut f: &mut Formatter<'_>)->std::result::Result<(),fmt::Error>{
		write!(&mut f, "@{}", self.value)
	}
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
/// Abstract, higher-level representation of a BIF file.
#[derive(Debug)]
pub struct BifFile {
	contents: Option<(BufReader<File>,Vec<BifResource>)>,
	path: PathBuf,
}
impl BifFile {
	/// Opens a BIF file (to cache) if not already open.
	///
	/// This converts a `None` option to a `Some()` where the BIF file is
	/// loaded to memory. This avoids opening several times the same file,
	/// or opening it when we have nothing to read in it.
	fn init(&mut self)->Result<()> {
		if self.contents.is_none() {
			let file = File::open(&self.path)
				.with_context(|| format!("cannot open BIF file: {:?}", self.path))?;
			let mut buf = BufReader::new(file);
			let hdr = BifHdr::unpack(&mut buf)
				.context("cannot open BIF header")?;
			buf.seek(SeekFrom::Start(hdr.offset as u64))?;
			let resources = BifResource::vecunpack(&mut buf, hdr.nres as usize)
				.with_context(|| format!("cannot read BIF resources from {:?}",
					self.path))?;
			self.contents = Some((buf, resources));
		}
		Ok(())
	}
	/// Reads a resource from the file.
	pub fn read(&mut self, resource_index: usize, restype1: Restype)->Result<Cursor<Vec<u8>>> {
		self.init()?;
		let (buf, resources) = self.contents.as_mut().unwrap();
		let BifResource { offset, size, restype, .. } = resources[resource_index];
		assert_eq!(restype1, restype);
		buf.seek(SeekFrom::Start(offset as u64))?;
		Ok(Cursor::new(BifHdr::read_bytes(buf, size as usize)?))
	}
	pub fn new(path: PathBuf)->Self { Self { contents: None, path } }
}
/// A (lazy) accessor to a game resource.
///
/// This encapsulates both the case of an override game resource and a
/// BIF game resource. In the second case, access to the BIF file is
/// lazy: this file is loaded only when [`ResHandle::open`] is called.
#[derive(Debug)]
pub enum ResHandle<'a> {
	Bif(&'a mut BifFile, BifIndex, Restype),
	Override(&'a Path),
}
impl ResHandle<'_> {
	pub fn is_override(&self)->bool { matches!(self, Self::Override(_)) }
	pub fn open(&mut self)->Result<Cursor<Vec<u8>>> {
		match self {
		Self::Bif(bif, location, restype) =>
			bif.read(location.resourceindex(), *restype),
		Self::Override(path) => Ok(Cursor::new(fs::read(&path)
				.with_context(|| format!("cannot open override file: {path:?}"))?)),
		}
	}
}
/// The main structure accessing game files.
#[derive(Debug)]
pub struct GameIndex {
	/// The root directory (containing "chitin.key").
	pub root: PathBuf,
	/// The names of the BIF files, as found in chitin.key.
	bifnames: Vec<String>,
	_bifsizes: Vec<u32>,
	resources: Vec<KeyRes>,
	/// The set of languages in the file, as a list of
	/// (5-letter language code, path to `dialog.tlk`).
	pub languages: Vec<(String,PathBuf)>,
	/// The backup directory. This is a constant value (always
	/// `$root/simod/backup`); we store it here for caching purposes.
	pub backup_dir: PathBuf,
}
/// Creates the directory unless it exists
pub fn create_dir(path: impl AsRef<Path>)->io::Result<()> {
	let path = path.as_ref();
	match fs::create_dir(path) {
		Ok(_) => { info!("creating directory {path:?}"); Ok(()) }
		Err(e) => match e.kind() {
			io::ErrorKind::AlreadyExists =>
				{ info!("not creating directory {path:?}: it already exists"); Ok(()) },
			_ => Err(e) } }
}
pub fn with_dir<T>(dir: impl AsRef<Path>, f: impl FnOnce()->Result<T>)->Result<T> {
	let orig_dir = std::env::current_dir()?;
	std::env::set_current_dir(dir)?;
	let res = f();
	std::env::set_current_dir(orig_dir)?;
	res
}
impl GameIndex {
	/// Initializes the structure from the path containing "chitin.key".
	///
	/// This mostly sets a few internal variables from the contents of the
	/// directory. It also creates the `simod` directory structure if it
	/// does not exist.
	pub fn new(gamedir: impl AsRef<Path>)->Result<Self> {
		let gamedir = gamedir.as_ref().to_path_buf();
		let indexfile = Path::new(&gamedir).join("chitin.key");
		let mut f = File::open(&indexfile)
			.with_context(|| format!("cannot open game index: {indexfile:?}"))?;
		let hdr = KeyHdr::unpack(&mut f)
			.with_context(|| format!("bad KEY header in file: {indexfile:?}"))?;
		println!("{hdr:?}");
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
		let simod_dir = gamedir.join("simod");
		create_dir(&simod_dir)
			.with_context(|| format!("cannot create simod directory {simod_dir:?}"))?;
		let backup_dir = simod_dir.join("backup");
		Ok(GameIndex{ root: gamedir, bifnames, resources, _bifsizes, languages,
			backup_dir })
	}
	/// Creates a temporary directory in an appropriate place in the game
	/// structure.
	pub fn tempdir(&self)->Result<tempfile::TempDir> {
		let dir = tempfile::tempdir_in(self.root.join("simod"))
			.context("failed to create temporary directory")?;
		debug!("created temporary directory: {dir:?}");
		Ok(dir)
	}
	/// Initializes the set of languages used in the game.
	///
	/// A language is stored as a 5-byte value (eg `"en_US"`), possibly
	/// followed by the letter `'F'` (eg `"fr_FRF"`). This is lossless and
	/// thus allows reconstructing the full path to `dialog[F].tlk` for
	/// backup and restoration.
	fn languages(gamedir: &impl AsRef<Path>)->Result<Vec<(String,PathBuf)>> {
		let langdir = gamedir.as_ref().join("lang");
		let mut r = Vec::<(String,PathBuf)>::new();
		for x in fs::read_dir(&langdir)
			.with_context(|| format!("cannot read lang directory: {langdir:?}"))? {
			let entry = x?;
			let lang = &entry.file_name().into_string().unwrap()[0..5];
			// TMP: this is to speed up execution (a bit) during test runs.
			if lang != "en_US" /* && lang != "fr_FR" && lang != "frF" */ { continue }
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
	///
	/// Resources are read first from override files, then from BIF files.
	/// This (and the use of a well-placed `INSERT OR IGNORE` SQL
	/// statement) allows ignoring of BIF resources masked by an override
	/// file.
	pub fn for_each<F>(&self, mut f: F)->Result<()>
	where F: (FnMut(Restype, Resref, ResHandle<'_>)->Result<()>)
	{
		let pb = Progress::new(self.resources.len(), "resources");
		let over_dir = self.root.join("override"); // "override" is a reserved kw
		if over_dir.is_dir() {
			scope_trace!("iterating over game resources from override: {:?}",
				over_dir);
			for e in over_dir.read_dir()
				.with_context(|| format!("cannot read directory {over_dir:?}"))? {
				let entry = e?;
				let name = match entry.file_name().into_string() { Err(_) => continue,
					Ok(s) => s };
				let pos = match name.find('.') { None => continue, Some(p) => p };
				if pos > 8 { continue }
				let resref = Resref { name: StaticString::<8>::from(&name[..pos]), };
				let ext = &name[pos+1..];
				let restype = crate::resources::restype_from_extension(ext);
				trace!("reading override file: {name}; restype={restype:?}");
				f(restype, resref, ResHandle::Override(&entry.path()))?
			}
		}
		scope_trace!("iterating over game resources from BIF");
		for (sourcefile, filename) in self.bifnames.iter().enumerate() {
			let path = self.root.join(filename);
			let mut bif = BifFile::new(path);
			let pb1 = Progress::new(self.resources.len(), filename);
			for res in &self.resources {
				if res.location.sourcefile() != sourcefile { continue }
				let rread = ResHandle::Bif(&mut bif, res.location, res.restype,);
				pb.inc(1);
				pb1.inc(1);
				f(res.restype, res.resref, rread)?
			}
		}
		Ok(())
	}
	/// Backups all game files.
	///
	/// If the backup already exists then an error is raised,
	/// unless `no_fail` is true.
	pub fn maybe_backup(&self, dest: impl AsRef<Path>, no_fail: bool)->Result<()> {
		let dest = dest.as_ref();
		self.backup(dest).or_else(|e| {
			if no_fail {
				warn!("could not backup game to {dest:?}: {e:?}"); Ok(())
			} else {
				Err(e)
				.with_context(|| format!("could not backup to {dest:?}"))
			}
		})
	}
	/// Completely backs up the game state to the given path.
	///
	/// This copies the content of the `override` directory, as well as all
	/// strings file, to the given destination. Such a backup may be
	/// reinstalled by the [`Self::restore`] function.
	///
	/// This fails if the destination directory already exists.
	pub fn backup(&self, dest: impl AsRef<Path>)->Result<()> {
		let dest = dest.as_ref();
		debug!("backup game files to {dest:?}");
// 		fs::remove_dir_all(dest) {
// 			Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(()),
// 			x => x
// 		}?;
		fs::create_dir(dest)?;
		let override_dir = self.root.join("override");
		for (lang, dialog_tlk) in &self.languages {
			fs::copy(dialog_tlk, dest.join(&format!("{lang}.tlk")))?;
		}
		if override_dir.is_dir() {
			for e in override_dir.read_dir()
					.with_context(|| format!("cannot read override directory: {override_dir:?}"))? {
				let entry = e?;
				fs::copy(entry.path(), dest.join(entry.file_name()))?;
			}
		}
		Ok(())
	}
	/// Completely sets game state from the one saved in a directory.
	///
	/// This sets the contents of all `.tlk` files from those files found
	/// in this directory, **erases** all previous contents of override
	/// directory, and moves all non-tlk files to override directory.
	///
	/// This is used both for restoring a backup game state and for
	/// quasi-atomic installation of a new game state.
	pub fn restore(&self, source: impl AsRef<Path>)->Result<()> {
		let source = source.as_ref();
		debug!("restoring files from {source:?}");
		let override_dir = self.root.join("override");
		let out_dir = self.tempdir()?;
		// prepare the move: list all `.tlk` files (these will be installed
		// to separate locations).
		let mut tlk_files = Vec::<(PathBuf,PathBuf)>::
			with_capacity(self.languages.len());
		for e in source.read_dir()
			.with_context(|| format!("cannot read source directory: {source:?}"))? {
			let entry = e?;
			let name = match entry.file_name().into_string() { Err(_) => continue,
				Ok(s) => s };
			let pos = match name.find('.') { None => continue, Some(p) => p };
			let ext = &name[pos+1..];
			trace!(r#"found file "{name}" with extension "{ext}""#);
			if ext.eq_ignore_ascii_case("tlk") {
				let mut lang = &name[..pos];
				let mut filename = String::from("dialog");
				// change "fr_FRF" to "fr_FR" + "dialogF.tlk":
				if pos == 6 && lang.as_bytes()[5] == b'F' {
					lang = &lang[..5];
					filename.push('F');
				}
				filename.push_str(".tlk");
				trace!(r#"this is a tlk file, pushing {name} -> {filename}"#);
				tlk_files.push((override_dir.join(&name),
					self.root.join("lang").join(lang).join(filename)));
				continue;
			}
		}
		// now do the move
		// it is not possible to do it atomically since we are moving to
		// several directories, so we group all move operations together
		// instead:
		fs::rename(&override_dir, out_dir.path())
			.with_context(|| format!("cannot displace old override directory {override_dir:?} to {out_dir:?}"))?;
		fs::rename(source, &override_dir)
			.with_context(|| format!("cannot displace old override directory {override_dir:?} to {out_dir:?}"))?;
		// once the “dangerous” step of overwriting override is completed,
		// we can move the tlk files from the new override:
		for (source, dest) in tlk_files {
			fs::rename(&source, &dest)
				.with_context(|| format!("cannot install saved language file {source:?} to {dest:?}"))?;
		}
		// TODO: in case of failure of either rename operation, use (slower)
		// file-by-file copy instead
		Ok(())
	}
}
} // mod gamefiles
pub(crate) mod schemas {
//! Inner description of SQL table.
//!
//! This mod groups everything which has access to the content of the
//! [`Schema0`] struct.
use crate::prelude::*;
use crate::gamefiles::Restype;
use crate::struct_io::{FieldType};

/// Description of a field in a game resource.
#[derive(Debug,Clone)]
pub struct Field {
	/// Name of this column.
	pub fname: &'static str,
	/// Content type of this column.
	pub ftype: FieldType,
	/// Any extra information given to SQLite when creating the table.
	pub extra: &'static str,
}
/// Displays the quoted field name.
/// This is a useful help for writing SQL statements.
impl Display for Field {
	fn fmt(&self, f: &mut Formatter<'_>)->fmt::Result {
		write!(f, r#""{n}""#, n = self.fname)
	}
}

pub trait ColumnWriter {
	type Elt: Display;
	type Iter: ExactSizeIterator<Item=Self::Elt>;
	fn as_iter(&self)->Self::Iter;
	fn prefixed<P: Display>(self, prefix: P)->ColumnPrefixed<Self, P>
	where Self: Sized { ColumnPrefixed(self, prefix) }
	fn cols(self)->ColumnPrefixed<Self, NullDisplay> where Self: Sized {
		ColumnPrefixed(self, NullDisplay())
	}
	fn len(&self)->usize { self.as_iter().len() }
}
impl<'a> ColumnWriter for &'a[Field] {
	type Elt = &'a Field;
	type Iter = std::slice::Iter<'a,Field>;
	fn as_iter(&self)->Self::Iter { self.iter() }
}
/// An utility type to write column headers
/// joined by commas and with a possible prefix (used for "new.").
pub struct ColumnPrefixed<W,P>(W,P);
impl<W: ColumnWriter, P: Display> Display for ColumnPrefixed<W, P> {
	fn fmt(&self, f: &mut Formatter<'_>)->fmt::Result {
		let mut isfirst = true;
		for element in self.0.as_iter() {
			if isfirst { isfirst = false; } else { write!(f, ",")?; }
			write!(f, r#" {prefix}{element}"#, prefix = self.1)?;
		}
		Ok(())
	}
}
impl<W: ColumnWriter, T: Display> ColumnPrefixed<W,T> {
	pub fn len(&self)->usize { self.0.len() }
	/// Returns the SQL select statement for these columns, as a String.
	pub fn select_sql(self, n: impl Display, condition: impl Display)->String {
		format!(r#"select {self} from "{n}" {condition}"#)
	}
	/// Returns the SQL insert statement for these columns, as a String.
	pub fn insert_sql(self, or: impl Display, n1: impl Display, n2: impl Display)->String {
		let mut s = format!("insert {or} into \"{n1}{n2}\" ({self}) values (");
		for c in 0..self.len()  {
			if c > 0 { s.push(','); }
			s.push('?');
		}
		s.push(')');
		s
	}
}

/// Description of several fields.
///
/// This convenience type does several things for us, all related to the
/// fields of a schema:
///  - it implements [`IntoIterator`] so that we may iterate over schema
///  columns,
///  - it implements [`Display`] (joined with commas) so that we may
///  easily build SQL statements.
///
/// The methods from [`Schema0`] might return various distinct instances
/// from this (e.g. payload, full columns, or context columns).
#[derive(Debug)]
pub struct Columns<'a,T>(pub &'a [Field], pub T);
impl<'a,T> IntoIterator for Columns<'a,T> {
	type Item = &'a Field;
	type IntoIter = std::slice::Iter<'a,Field>;
	fn into_iter(self)->Self::IntoIter { self.0.iter() }
}
impl<T: Display> Display for Columns<'_,T> {
	fn fmt(&self, f: &mut Formatter<'_>)->fmt::Result {
		let mut isfirst = true;
		for Field { fname, .. } in self.0.iter() {
			if isfirst { isfirst = false; } else { write!(f, ",")?; }
			write!(f, r#" {prefix}"{fname}""#, prefix = self.1)?;
		}
		Ok(())
	}
}
impl<T> Columns<'_, T> {
	/// The length of this particular column set.
	/// We need this since it is in general different from schema length.
	pub fn len(&self)->usize { self.0.len() }
}

/// The identifier of the table for a schema.
#[derive(Debug,Clone)]
pub enum SchemaResource {
#[allow(dead_code)]
	Top { extension: &'static str, restype: Restype, },
#[allow(dead_code)]
	Sub { parent: &'static str, resref_index: usize, },
}

/// The full database description of a game resource.
///
/// This contains all relevant information to fully define a resource
/// on the SQL side.
///
/// In practice there exists exactly one [`Schema`] instance per
/// resource, and it is compiled by the [`Resource`] derive macro.
///
/// The structure of the schema is always as follows:
/// 1. **Payload fields.** These are the fields present in game files.
/// 2. **Context fields.** These fields identify the position of the
///    resource in game fields.
/// In turn, the context fields are always:
///  - for top-level resources: only the associated resref.
///  - for sub-resources: resref is the first field and the last field is
///    `rowid` alias. (In particular there are always several context
///    fields).
/// In all cases, the primary key is guaranteed to be the last field,
/// while context starts at the resref fields.
/// This structure allows handling context as a (contiguous) slice of fields;
/// see [`Schema::iter`], [`Schema::columns`].
///
/// A few fields in this struct (`extension`, etc.)
/// are used only for top-level resources.
/// Doing this saves us a bit of code (separating top-level resources as
/// their own type + writing ad-hoc macros etc.). The most harm it does
/// is (a) storing a few bytes of useless memory in the executable,
/// and (b) possibly inserting a few always-empty tables in the database
/// (we try to avoid this however).
#[derive(Debug,Clone)]
pub struct Schema {
	pub name: &'static str,
	pub resource: SchemaResource,
	pub fields: Vec<Field>,
}
impl Display for Schema {
	fn fmt(&self, f: &mut Formatter<'_>)->fmt::Result { f.write_str(self.name) }
}
impl Schema {
	/// Returns the index of the resref field.
	fn resref_idx(&self)->usize {
		match self.resource {
			SchemaResource::Top { .. } => self.fields.len()-1,
			SchemaResource::Sub { resref_index, .. } => resref_index,
		}
	}
	/// Returns the name of the table containing the parent resref.
	pub fn parent_table(&self)->&'static str {
		match self.resource {
			SchemaResource::Top { .. } => self.name,
			SchemaResource::Sub { parent, .. } => parent,
		}
	}
	/// Returns the name of the field containing the resref for this
	/// resource.
	pub fn resref(&self)->&'static str { self.fields[self.resref_idx()].fname }
	/// Returns the name of the primary key.
	pub fn primary(&self)->&'static str { self.fields[self.fields.len()-1].fname }
	/// Returns the SQL statement creating a given table with this schema.
	///
	/// The parameters are the table name (not necessarily matching the
	/// name of the schema) and the string defining extra columns, if any.
	pub fn create_table(&self, name: impl Display, more: impl Display)->String {
		let mut s = format!("create table \"{name}\" (");
		let mut isfirst = true;
		for Field { fname, ftype, extra, .. } in &self.fields {
			if isfirst { isfirst = false; }
			else { s.push(','); }
			uwrite!(&mut s, "\n \"{fname}\" {} {extra}", ftype.affinity());
		}
		uwrite!(&mut s, "{more})");
		s
	}
	/// Returns the SQL statement creating the main view for this schema.
	fn create_main_view(&self)->String {
		let Schema { name, .. } = self;
		let primary = self.primary();
		let mut view = format!(r#"create view "{name}" as
	with "u" as (select {f} from "load_{name}" union select {f} from "add_{name}") select "#, f = self.fields.cols());
		for Field {fname, .. } in &self.fields[..self.fields.len()-1] {
			uwrite!(&mut view, r#"ifnull((select "value" from "edit_{name}" where "resource"="{primary}" and "field"='{fname}' order by rowid desc limit 1), "{fname}") as "{fname}", "#);
		}
		uwrite!(&mut view, r#""{primary}" from "u""#);
		view
	}
	/// Returns the SQL for creating the output view of a resource.
	fn create_output_view(&self)->String {
		let parent_key = self.resref();
		let mut select = format!(r#"create view "save_{self}" as select"#);
		let mut source = format!(r#"
from "{self}" as "a""#);
		for Field { fname: f, ftype, .. } in &self.fields {
			match ftype {
				FieldType::Resref => {
					let a = format!(r#""a"."{f}""#);
					let b = format!(r#""b_{f}""#);
					uwrite!(&mut select, r#"
case when {a} in (select "resref" from "resref_orig") then {a} else ifnull({b}."resref", '') end as "{f}","#);
					uwrite!(&mut source, r#"
left join "resref_dict" as {b} on {a} = {b}."key""#);
				},
				FieldType::Strref => {
					let a = format!(r#""a"."{f}""#);
					let b = format!(r#""b_{f}""#);
					uwrite!(&mut select, r#"
case when typeof({a}) == 'integer' then {a} else ifnull({b}."strref", 0) end as "{f}","#);
					uwrite!(&mut source, r#"
left join "strref_dict" as {b} on {a} = {b}."native""#);
				},
				_ => uwrite!(&mut select, r#""a"."{f}","#),
			}
		}
		uwrite!(&mut select, r#""a"."{parent_key}" as "key"{source}"#);
		select
	}
	/// Creates all tables and views in the database associated with a
	/// given resource.
	///
	/// The tables are as follows:
	/// - `load_{name}`: data read from game files.
	/// - `add_{name}`: new data inserted by mods.
	/// - `edit_{name}`: data modified by mods.
	/// The views are:
	/// - `{name}`: the main view on which edits are done by mods (and
	/// propagated to `add_{name}` or `edit_{name}` by triggers as needed).
	/// - `save_{name}`: the view used to save new or modified values to
	/// game files.
	pub fn create_tables_and_views<T>(&self, f: impl Fn(String)->Result<T>)->Result<()> {
		let name = self.name;
		let parent_key = self.resref();
		let parent_table = self.parent_table();
		let primary = self.primary();

		// read-only table of initial resources:
		f(self.create_table(&format!("load_{name}"), ""))?;
		// table of resources inserted by mods:
		f(self.create_table(&format!("add_{name}"), r#", "source" text"#))?;
		// table of fields edited by mods:
		f(format!(r#"create table "edit_{name}" ("source" text, "resource" text, "field" text, "value")"#))?;
		f(self.create_main_view())?;
		f(self.create_output_view())?;
		let dirtytable = format!("dirty_{parent_table}");
		if let SchemaResource::Top { .. } = self.resource {
			// only for top-level resource: create the dirty and orphan tables
			f(format!(r#"create table "{dirtytable}" ("name" text primary key on conflict ignore)"#))?;
			f(format!(r#"create table "orphan_{name}" ("name" text primary key on conflict ignore)"#))?;
			f(format!(r#"create trigger "orphan_{name}" after delete on "add_{name}"
begin
	insert into "orphan_{name}" values (old."{primary}");
end"#))?;
		}
		// populate resref_dict or strref_dict as needed:
		// There are two triggers which edit "resref_dict"; we build both
		// of them from resref fields at once, by storing the text in
		// `trans` (for update triggers) and `trans_insert` (for insert
		// trigger).
		let mut trans_insert = String::new();
		for Field {fname, ftype, ..} in &self.fields {
			let trans = match ftype {
			FieldType::Resref => format!(
	r#"insert or ignore into "resref_dict" values (new."{fname}", null);"#),
// 			FieldType::Strref => format!(
// 	r#"insert or ignore into "strref_dict" values (new."{fname}", null);"#),
			_ => String::new()
			};
			trans_insert.push_str(&trans);
			f(format!(
r#"create trigger "update_{name}_{fname}"
instead of update of "{fname}" on "{name}"
begin
	{trans}
	insert or ignore into "{dirtytable}" values (new."{parent_key}");
	insert into "edit_{name}" ("source", "resource", "field", "value") values
		((select "component" from "global"), new."{primary}", '{fname}',
		new."{fname}");
end"#))?;
		}
		f(format!(
r#"create trigger "insert_{name}"
instead of insert on "{name}"
begin
	{trans_insert}
	insert into "add_{name}" ({cols}) values ({newcols});
	insert or ignore into "{dirtytable}" values (new."{parent_key}");
end"#, cols = self.fields.cols(), newcols = self.fields.prefixed("new.")))?;
		f(format!(
r#"create trigger "delete_{name}"
instead of delete on "{name}"
begin
	insert or ignore into "{dirtytable}" values (old."{parent_key}");
	delete from "add_{name}" where "{primary}" = old."{primary}";
end"#))?;
		f(format!(
r#"create trigger "unedit_{name}"
after delete on "edit_{name}"
begin
	insert or ignore into "{dirtytable}" values (old."resource");
end"#))?;
		Ok (())
	}
	/// The SQL code for inserting into a table.
	///
	/// This is used for both initial populating of the database from game
	/// files and for `simod.insert`.
	///
	/// The `or` parameter allows calling SQL `INSERT OR IGNORE`;
	/// this is used when initializing the database to ignore resources
	/// which are superseded by override files.
	pub fn insert_sql(&self, or: impl Display, prefix: impl Display)->String {
		self.fields.cols().insert_sql(or, prefix, self.name)
	}
	/// Helper function for generating `new_strings` view.
	pub fn append_new_strings_schema(&self, w: &mut impl fmt::Write, is_first: &mut bool) {
		for Field { fname, ftype, .. } in &self.fields {
			if *ftype != FieldType::Strref { continue }
			if *is_first { *is_first = false; }
			else { uwrite!(w, "\n\tunion "); }
			// the '1' column is a placeholder for flags:
			uwriteln!(w, r#"select "{fname}" as "native", 1 as "flags" from "add_{self}"
	union select "value" as "native", 1 as "flags" from "edit_{self}" where "field"='{fname}'"#);
		}
	}
	/// Helper function for generating `new_strings` triggers.
	pub fn append_new_strings_trigger(&self, w: &mut impl fmt::Write) {
		let resref = self.resref();
		let parent = self.parent_table();
		let mut is_first_field = true;
		for Field { fname, ftype, .. } in &self.fields {
			if *ftype != FieldType::Strref { continue }
			if is_first_field {
				uwrite!(w, r#"
	insert into "dirty_{parent}" select "{resref}" from "{self}" where "#);
				is_first_field = false;
			}
			else { uwrite!(w, " or "); }
			uwrite!(w, r#""{fname}"=new."native""#);
		}
		if !is_first_field {
			uwriteln!(w, ";");
		}
	}
}

/// The full database description of a game resource.
///
/// This contains all relevant information to fully define a resource
/// on the SQL side.
///
/// In practice there exists exactly one [`Schema0`] instance per
/// resource, and it is compiled by the [`Resource0`] derive macro.
///
/// The structure of the schema is always as follows:
/// 1. **Payload fields.** These are the fields present in game files.
/// 2. **Context fields.** These fields identify the position of the
///    resource in game fields.
/// In turn, the context fields are always:
///  - for top-level resources: only the associated resref.
///  - for sub-resources: resref is the first field and the last field is
///    `rowid` alias. (In particular there are always several context
///    fields).
/// In all cases, the primary key is guaranteed to be the last field,
/// while context starts at the resref fields.
/// This structure allows handling context as a (contiguous) slice of fields;
/// see [`Schema0::iter`], [`Schema0::columns`].
///
/// A few fields in this struct (`extension`, etc.)
/// are used only for top-level resources.
/// Doing this saves us a bit of code (separating top-level resources as
/// their own type + writing ad-hoc macros etc.). The most harm it does
/// is (a) storing a few bytes of useless memory in the executable,
/// and (b) possibly inserting a few always-empty tables in the database
/// (we try to avoid this however).
#[derive(Debug)]
pub struct Schema0<'a> {
	/// Descriptions for all the fields of this struct.
	pub fields: &'a[Field],
	/// The stem for the SQL table name associated with this structure.
	pub name: &'a str,
	/// The index of the field pointing to top-level resource resref.
	///
	/// This is also (always) the first context column.
	/// Top-level resources have this field as their primary key;
	/// subresources have a dedicated "id" column at the end.
	pub resref_key: usize,
	/// The name of the table holding the top-level resource.
	pub parent_table: &'a str,
	/// The extension for this file type (if top-level), or "".
	pub extension: &'a str,
	/// The restype identifier for this resource type (if top-level), or 0.
	pub restype: Restype,
}
impl<'schema> Schema0<'schema> {
	// Step 0: a few useful functions
	/// Is this a subresource?
	pub fn is_subresource(&self)->bool {
		self.resref_key < self.fields.len() - 1
	}
	/// Returns the index before the id column (if present).
	fn before_id(&self)->usize {
		self.fields.len() - (self.is_subresource() as usize)
	}
	/// Columns of payload + context; no id.
	///
	/// In other words: these are the columns which can be inserted (id is
	/// automatically computed by sqlite).
	///
	/// Returns an object implementing [`std::fmt::Display`] to write the
	/// column headers for this schema in a SQL query.
	pub fn columns(&'schema self)->Columns<'schema, NullDisplay> {
		Columns(&self.fields[..self.before_id()], NullDisplay())
	}
	/// Context columns only.
	pub fn context(&'schema self)->Columns<'schema, NullDisplay> {
		Columns(&self.fields[self.resref_key..self.before_id()], NullDisplay())
	}
	/// The primary key for this schema (as a string).
	///
	/// Note that the [`Schema0`] structure imposes that this is always the
	/// last field.
	pub fn primary(&'schema self)->&str {
		self.fields[self.fields.len()-1].fname
	}
	// Step 1: creating tables
	/// The SQL code for inserting into a table.
	///
	/// This is used for both initial populating of the database from game
	/// files and for `simod.insert`.
	///
	/// The `or` parameter allows calling SQL `INSERT OR IGNORE`;
	/// this is used when initializing the database to ignore resources
	/// which are superseded by override files.
	pub fn insert_statement(&self, or: impl Display, prefix: impl Display)->String {
		let name = &self.name;
		let cols = self.columns();
		let mut s = format!("insert {or} into \"{prefix}{name}\" ({cols}) values (");
		for c in 0..cols.len()  {
			if c > 0 { s.push(','); }
			s.push('?');
		}
		s.push(')');
		s
	}
	/// Collects parameters for a SQL statement from the fields of the
	/// schema.
	///
	/// Utility wrapper for [`rusqlite::params_from_iter`].
	/// Usage: `schema.params_from(|fname, ftype| { Ok(parameter...) })?`
	pub fn params_from<T,F>(&self, f: F)->Result<rusqlite::ParamsFromIter<Vec<T>>>
	where T: rusqlite::ToSql + Debug,
		F: Fn(&str, FieldType)->Result<T>
	{
		let params = self.columns().into_iter()
			.map(|Field{fname, ftype, ..}| f(fname, *ftype))
			.collect::<Result<Vec<T>>>()?;
		trace!("collected parameters: {params:?}");
		Ok(rusqlite::params_from_iter(params))
	}
} // impl Schema0
/// Convenience implem.: prints schema name.
impl Display for Schema0<'_> {
	fn fmt(&self, f: &mut Formatter<'_>)->fmt::Result { f.write_str(self.name) }
}
} // mod schemas
pub(crate) mod gamestrings {
//! Access to game strings in database.
//!
//! This is the only mod knowing the internals of [`GameString`] struct.
use crate::prelude::*;
use crate::progress::{Progress};
use crate::database::{self,DbInterface};
use crate::gamefiles::{GameIndex};
use crate::pack::{Pack,NotPacked};
use crate::struct_io::{NoSql,SqlRow};
use macros::{Pack,SqlRow};

#[derive(Debug,Pack)]
struct TlkHeader {
	#[header("TLK V1  ")]
	lang: u16,
	nstr: u32,
	offset: u32,
}
/// A game string as present in a .tlk file.
#[derive(Debug,Default,Clone,Pack,SqlRow)]
pub struct GameString {
#[column("primary key")]
	strref: NotPacked::<i32>,
	flags: u16,
	sound: Resref,
	volume: i32,
	pitch: i32,
	delta: NoSql::<i32>,
	strlen: NoSql::<i32>,
	string: String,
}
/// The iterator used for accessing game strings.
#[derive(Debug)]
pub struct GameStringsIterator<'a> {
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

		let start = self.offset + (s.delta.unwrap() as usize);
		let end = start + (s.strlen.unwrap() as usize);
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
	let path = path.as_ref();
	let mut file = fs::File::create(path)
		.with_context(|| format!("cannot create TLK file: {path:?}"))?;
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
			.with_context(|| format!("cannot write strings to TLK file:{path:?}"))?;
	}
	Ok(())
}

/// Fills all game strings tables from game dialog files.
pub fn load_languages(db: &impl DbInterface, game: &GameIndex)->Result<()> {
	let pb = Progress::new(game.languages.len(), "languages");
	for (langname, dialog) in game.languages.iter() {
		pb.inc(1);
		load_language(db, langname, &dialog).with_context(||
				format!("cannot load language '{langname}' from '{dialog:?}'"))?;
	}
	Ok(())
}
/// Fills the database table for a single language.
fn load_language(db: &impl DbInterface, langname: &str, path: &(impl AsRef<Path> + Debug))->Result<()> {
	let bytes = fs::read(path)
		.with_context(|| format!("cannot open strings file: {path:?}"))?;
	let mut q = db.prepare(&format!(r#"insert into "load_strings_{langname}" ("strref", "flags", "sound", "volume", "pitch", "string") values (?,?,?,?,?,?)"#))?;
	let itr = GameStringsIterator::try_from(bytes.as_ref())?;
	let pb = Progress::new(itr.len(), langname);
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
/// For all game languages, saves database strings to "{lang}.tlk" in
/// current directory.
///
/// This also backups those files if needed (i.e. if no backup exists
/// yet).
pub fn save(db: &impl DbInterface, game: &GameIndex)->Result<()> {
	use database::DbTypeCheck;
	let pb = Progress::new(game.languages.len(), "save translations");
	for (lang, _) in &game.languages {
		pb.inc(1);
		let count = 1+db.query_row(&format!(r#"select max("strref") from "strings_{lang}""#),
			(), |row| row.get::<_,usize>(0))?;
		let mut vec = vec![GameString::default(); count];
		let mut sel_str = GameString::select_from_typed(db,
			lazy_format!("strings_{lang}"), NullDisplay())?;
		for row in sel_str.iter(())? {
			if row.is_db_malformed() { continue }
			let gamestring = row?;
			let strref = gamestring.strref.unwrap() as usize;
			vec[strref] = gamestring;
		}
		let target = format!("{lang}.tlk");
		save_language(&vec, &target)
		.with_context(|| format!("could not save strings for language '{lang}'"))?;
		info!("updated strings in {target:?}; file now contains {count} entries");
	}
	Ok(())
}
} // mod gamestrings
pub(crate) mod database {
//! Access to the SQL side of the database.
//!
//! Main exports are:
//!  - [`Schema0`] type: description of a particular SQL table;
//!  - [`Resource0`] trait: connect a Rust structure to a SQL row.
use crate::prelude::*;
use rusqlite::{Row};
use crate::resources::*;
use crate::gamefiles::GameIndex;

// I. Low-level stuff: basic types and extensions for `rusqlite` traits.
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

// III. Structure accessing directly SQL data
/// A trivial wrapper on [`rusqlite::Connection`];
/// mainly used for standardizing log messages.
#[derive(Debug)]
pub struct GameDB(Connection);
impl Deref for GameDB {
	type Target = Connection;
	fn deref(&self)->&Self::Target { &self.0 }
}
#[derive(Debug)]
pub struct GameTransaction<'a>(rusqlite::Transaction<'a>);
impl Deref for GameTransaction<'_> {
	type Target = Connection;
	fn deref(&self)->&Self::Target { self.0.deref() }
}
/// A trait for trivial wrappers of [`rusqlite::Connection`].
pub trait DbInterface {
	// Required methods
	/// Executes a statement with arguments (and logging).
	fn execute(&self, s: impl AsRef<str>, params: impl rusqlite::Params+Debug)->Result<usize>;
	/// Prepares a statement (and logs it).
	fn prepare(&self, s: impl AsRef<str>)->Result<Statement<'_>>;
	fn last_insert_rowid(&self)->i64;
	// Provided low-level methods
	/// Executes a statement without arguments (but with logging).
	fn exec(&self, s: impl AsRef<str>)->Result<usize> { self.execute(s, ()) }
	/// Executes a batch of statements (with logging).
	fn batch(&self, s: impl AsRef<str>)->Result<()>;
	/// Prepares and executes a statement returning a single row.
	fn query_row<T>(&self, s: impl AsRef<str>, params: impl rusqlite::Params+Debug, f: impl FnOnce(&Row<'_>)->rusqlite::Result<T>)->Result<T> {
		self.prepare(s)?.query_row(params, f).map_err(|e| e.into())
	}
	fn query_row_and_then<T>(&self, s: impl AsRef<str>, params: impl rusqlite::Params+Debug, f: impl FnOnce(&Row<'_>)->Result<T>) -> Result<T> {
		let mut stmt = self.prepare(s)?;
		let mut rows = stmt.query(params)?;
		let row = match rows.next()? {
			None => return Err(rusqlite::Error::QueryReturnedNoRows.into()),
			Some(row) => row };
		f(row)
	}
	// Higher-level methods
	/// Before saving: fills the strref translation tables.
	fn translate_strrefs(&self)->Result<()> {
		let new_strings = self.query_row(
			r#"select count(1) from "new_strings""#, (),
			|row| { row.get::<_,usize>(0) })?;
		let base_strings = self.query_row(
			r#"select "strref_count" from "global""#, (),
			|row| { row.get::<_,usize>(0) })?;
		// The new strrefs will be allocated up to `bound`-1.
		// Thus all entries which currently have a higher strref will need to
		// be modified. The simplest way is to delete them from the dictionary.
		let bound = base_strings + new_strings;
		debug!("reallocating strrefs up to {bound}");
		// we update `new_strings` with a dummy value (-1);
		// thanks to the trigger, SQLite replaces this for us by
		// the lowest available strref:
		self.batch(format!(r#"
delete from "strref_dict"
where
	"native" not in (select "native" from "new_strings")
or
	"strref" >= {bound};
update "new_strings" set "strref"=-1 where "strref" is null"#))
			.context("cannot generate new strrefs")?;
		info!("translated strrefs");
		Ok(())
	}
	/// Before saving: fills the resref translation table.
	fn translate_resrefs(&self)->Result<()> {
		let mut enum_st = self.prepare(r#"select key from "resref_dict" where "resref" is null"#)?;
		let mut enum_rows = enum_st.query(())?;
		let mut find = self.prepare(r#"select 1 from "resref_orig" where "resref"=?1 union select 1 from "resref_dict" where "resref"=?1"#)?;
		let mut update = self.prepare(r#"update "resref_dict" set "resref"=?2 where "key"=?1"#)?;
		let mut n_resrefs = 0;
		while let Some(row) = enum_rows.next()? {
			let longref = row.get::<_,String>(0)?;
			// TODO: remove spaces etc.
			trace!("generate fresh resref from '{longref}'");
			let resref = Resref::fresh(&longref, |r| find.exists((r,)))?;
			update.execute((&longref, &resref))?;
			n_resrefs+= 1;
		}
		info!("translated {n_resrefs} resrefs");
		Ok(())
	}
	/// Deletes from current directory all resources marked as 'orphan' in
	/// the database. This also clears the list of orphan resources.
	fn clear_orphan_resources(&self)->Result<()> {
		all_schemas().map(|schema| {
			let extension = match schema.resource {
				crate::schemas::SchemaResource::Top { extension, .. } => extension,
				_ => return any_ok(())
			};
			let mut stmt = self.prepare(
				format!(r#"select "name" from "orphan_{schema}""#))?;
			let mut rows = stmt.query(())?;
			while let Some(row) = rows.next() ? {
				let mut file = row.get::<_,String>(0)
					.with_context(|| format!(r#"bad entry in "orphan_{schema}""#))?;
				file.push('.');
				file.push_str(extension);
				fs::remove_file(&file).or_else(|e| match e.kind() {
					io::ErrorKind::NotFound => {
						warn!(r#"should have removed orphan file "{file}""#);
						Ok(())
					},
					_ => Err(e)
				}).with_context(|| format!(r#"cannot remove file "{file}""#))?;
			}
			any_ok(())
		}).context("cannot clear orphan resources")?;
		Ok(())
	}
	/// Cleans the dirty bit from all resources after saving.
	fn unmark_dirty_resources(&self)->Result<()> {
		all_schemas().map(|schema| {
			if matches!(schema.resource, crate::schemas::SchemaResource::Top { .. }) {
				self.exec(format!(r#"delete from "dirty_{schema}""#))?;
				self.exec(format!(r#"delete from "orphan_{schema}""#))?;
			};
			any_ok(())
		})?;
		any_ok(())
	}
}
impl<T: Deref<Target=Connection>>  DbInterface for T {
	fn execute(&self, s: impl AsRef<str>, params: impl rusqlite::Params+Debug)->Result<usize> {
		let s = s.as_ref();
		debug!("executing SQL statement: {s} with parameters {params:?}");
		self.deref().execute(s, params)
			.with_context(|| format!("failed SQL statement:\n {s}"))
	}
	fn prepare(&self, s: impl AsRef<str>)->Result<Statement<'_>> {
		let s = s.as_ref();
		debug!("preparing SQL statement: {s}");
		self.deref().prepare(s)
			.with_context(|| format!("failed to prepare SQL statement:\n {s}"))
	}
	fn last_insert_rowid(&self)->i64 { self.deref().last_insert_rowid() }
	/// Executes a batch of statements (with logging).
	fn batch(&self, s: impl AsRef<str>)->Result<()> {
		let s = s.as_ref();
		debug!("executing a batch of SQL statements: {s}");
		self.deref().execute_batch(s)
			.with_context(|| format!("failed batched SQL statements:\n {s}"))
	}
}
impl GameDB {
	/// Wraps a closure inside a game transaction.
	///
	/// The transaction aborts if the closure returns an `Err` variant, and
	/// commits if it returns an `Ok` variant.
	pub fn transaction<T>(&mut self, mut f: impl FnMut(&GameTransaction<'_>)->Result<T>)->Result<T> {
		let t = GameTransaction(self.0.transaction()
			.context("create new transaction")?);
		let r = f(&t)?; // automatic rollback if Err
		t.0.commit()?;
		Ok(r)
	}
	/// Opens an existing game database.
	///
	/// (Just like [`Connection::open`] but with logging).
	pub fn open(p: impl AsRef<Path>)->Result<Self> {
		let p = p.as_ref();
		debug!("opening database from {p:?}");
		let r = Self(Connection::open(p)
			.with_context(|| format!("failed to open SQL database:\n {p:?}"))?);
		info!("opened database: {p:?}");
		Ok(r)
	}
	/// Creates and initializes the game database.
	///
	/// This creates all relevant tables and views in the database
	/// (but does not fill them).
	///
	/// There are global tables, per-resource tables, and per-language tables.
	/// The per-resource tables are described in the
	/// [`Schema0::create_tables_and_views`] function.
	/// The per-language tables are described in [`Self::create_language_tables`].
	/// The global tables are the following:
	///
	/// - `global`: a single-row table holding the global variables:
	///   - `component`: current mod component being installed;
	///   - `strref_count`: number of strings in `"dialog.tlk"` (34000);
	/// - `resref_orig`: list of all original game resrefs — this is used for
	///    de-namespacing (all resrefs in this list translate to themselves);
	/// - `resref_dict`: translations of resrefs (from namespaced to 8 bytes);
	/// - `strref_dict`: translations of strrefs (from string key to 4-bytes int);
	/// - `new_strings`: (built in [`crate::schemas::create_new_strings`])
	///    a view of all strrefs currently introduced by mods.
	///    This gets compiled into `strref_dict` as part of `save`.
	pub fn create(db_file: impl AsRef<Path>, game: &GameIndex)->Result<Self> {
		let db_file = db_file.as_ref();
		info!("creating file {db_file:?}");
		if Path::new(&db_file).exists() {
			fs::remove_file(db_file)
			.with_context(|| format!("cannot remove DB file: {db_file:?}"))?;
		}
		let mut db = Self::open(db_file)
			.with_context(|| format!("cannot open DB file: {db_file:?}"))?;
		db.transaction(|db| {
			db.batch(r#"
create table "global" ("component" text, "strref_count" integer);
insert into "global"("component") values (null);
create table "override" ("resref" text, "ext" text);
create table "resref_orig" ("resref" text primary key on conflict ignore);
create table "resref_dict" ("key" text not null primary key on conflict ignore, "resref" text);
create table "strref_dict" ("native" text not null primary key, "strref" integer unique);
create index "strref_dict_reverse" on "strref_dict" ("strref");
			"#).context("create strings tables")?;
			all_schemas().map(|schema| {
				debug!("  creating tables for resource '{schema}'");
				schema.create_tables_and_views(|s| db.exec(s))
			}).context("cannot create main tables and views")?;
			all_schemas().create_new_strings(|s| db.exec(s))?;
			Ok(())
		}).context("creating global tables")?;
		db.create_language_tables(game).context("cannot create language tables")?;
		Ok(db)
	}
	/// Creates (but does not fill) all per-language tables.
	///
	/// The per-language tables are as follows:
	/// - `translations_XX`: user-filled table containing string translations
	/// - `strings_XX`: output to fill game strings
	///   (last column is a bit indicating whether this is untranslated, in
	///   which case we need to remove all {?..} marks)
	fn create_language_tables(&mut self, game: &GameIndex)->Result<()> {
		self.transaction(|db| {
// 		const SCHEMA: &str = r#"("strref" integer primary key, "string" text, "flags" integer, "sound" text, "volume" integer, "pitch" integer)"#;
// 		const SCHEMA1: &str = r#"("native" string primary key, "string" text, "flags" integer, "sound" text, "volume" integer, "pitch" integer)"#;
		for (lang, _) in &game.languages {
			db.batch(&format!(r#"
	create table "load_strings_{lang}"("strref" integer primary key, "string" text, "flags" integer, "sound" text, "volume" integer, "pitch" integer);
	create table "translations_{lang}"("native" string primary key, "string" text, "sound" text, "volume" integer, "pitch" integer);
	create view "strings_{lang}" as
	select "strref", "string",  "flags", "sound", "volume", "pitch", 0 as "raw" from "load_strings_{lang}" union
	select "strref", ifnull("string", "native"), "flags", ifnull("sound",''), ifnull("volume",0), ifnull("pitch",0), ("string" is null)
from "new_strings"
	left join "translations_{lang}" using ("native");
	"#))?;
		}
		Ok(())
		})
	}
	/// Fills the database from game files.
	///
	/// The database must have been already created and initialized (with
	/// empty tables).
	pub fn load(&mut self, game: &GameIndex)->Result<()> {
		let pb = Progress::new(3, "Fill database"); pb.inc(1);
		self.transaction(|db| {
			crate::gamestrings::load_languages(db, game).with_context(||
			format!("cannot read game strings from game directory {:?}", game.root))
		})?;
		pb.inc(1);
		debug!("loading game resources");
		self.transaction(|db| {
			let mut base = DbInserter::new(db)?;
			game.for_each(|restype, resref, handle| {
				trace!("found resource {}.{:#04x}", resref, restype.value);
				base.register(&resref)
					.with_context(|| format!("could not register resref:{resref}"))?;
				#[allow(clippy::single_match)]
				match restype {
				Item::RESTYPE => Item::load_from_handle(&mut base, resref, handle)?,
				_ => (),
				};
				Ok(())
			})?;
			pb.inc(1);
			Ok(())
		})
	}
}
/// A structure holding insertion statements for all resource types.
///
/// This structure does the main work for initially filling the database.
/// It also contains a statement for storing original resrefs.
#[derive(Debug)]
pub struct DbInserter<'a> {
// 	db: &'a Connection,
	add_resref: Statement<'a>,
	pub tables: AllResources<Statement<'a>>,
	pub add_override: Statement<'a>,
	resource_count: AllResources<(&'static str, usize)>,
}
impl<'a> DbInserter<'a> {
	/// Creates a new `DbInserter` from a database and the list of all
	/// resources.
	pub fn new(db: &'a impl DbInterface)->Result<Self> {
		Ok(Self { // db,
		tables: all_schemas().map(|schema|
			db.prepare(&schema.insert_sql("or ignore", "load_"))
				.with_context(|| format!("insert statement for table '{schema}'"))
		)?,
		resource_count: all_schemas().map(|schema| any_ok((schema.name, 0)))?,
		add_resref: db.prepare(
			r#"insert or ignore into "resref_orig" values (?)"#)?,
		add_override: db.prepare(
			r#"insert or ignore into "override" values (?,?)"#)?,
	}) }
	/// Adds a new [`Resref`] to the table of original resrefs.
	pub fn register(&mut self, resref: &Resref)->rusqlite::Result<usize> {
		self.add_resref.execute((resref,))
	}
}
impl Drop for DbInserter<'_> {
	fn drop(&mut self) {
		self.resource_count.map(|(name, n)|
			any_ok(info!(r#"loaded {n} entries in table "{name}""#))).unwrap();
	}
}
} // mod resources
pub(crate) mod lua_api {
//! Loads mod-supplied Lua files.
//!
//! This module supplies the [`command_add`] function, which runs
//! user-supplied Lua code on the database.
//!
//! The Lua interpreter is first equipped with a `simod` table containing
//! a basic API into the database. **This API is not stabilized yet**.
//! It currently contains the following functions:
//!
//!  - `simod.list(table, [parent])`: returns a list of primary keys in
//!    the named table (string). If `parent` is given, then only those
//!    primary keys for rows attached to the named toplevel resource are
//!    returned.
//!  - `simod.select(table, key)`: returns a table representing one
//!    single row in the named table (string); the table keys are the
//!    column headers.
//!  - `simod.update(table, key, field, value)`: updates a single field
//!    in the table.
//!  - `simod.insert(table, row, context)`: inserts a full row in the
//!    table. The columns are read from the merge of both tables `row`
//!    and `context`.
//!
//! Any error occurring during execution of one of those functions
//! (including SQL errors) is passed back to Lua in the form of a
//! callback error.
use mlua::{Lua,ExternalResult};

use crate::prelude::*;
use crate::resources::{AllResources,all_schemas,RESOURCES};
use crate::schemas::Schema;
use crate::struct_io::{FieldType,AsParams};
/// Runtime errors during interface between SQL and Lua.
/// A simple wrapper allowing `mlua::Value` to be converted to SQL.
#[derive(Debug)]
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
/// A newtype around [`&mlua::Value`], allowing us to implement various
/// extra traits: [`rusqlite::ToSql`], custom [`Debug`] etc.
///
/// This would be more properly written with *two* lifetime parameters
/// `'a` and `'lua`, but we have no use for this right now, so for the
/// sake of simplicity we keep only the shortest lifetime `'a`.
pub struct LuaValueRef<'a>(&'a mlua::Value<'a>);
impl<'a> From<&'a mlua::Value<'a>> for LuaValueRef<'a> {
	fn from(source: &'a mlua::Value<'a>)->Self { Self(source) }
}
impl Debug for LuaValueRef<'_> {
	fn fmt(&self, f: &mut Formatter<'_>)->fmt::Result {
		match self.0 {
			mlua::Value::String(s) => f.write_str(&s.to_string_lossy()),
			x => write!(f, "{x:?}"),
		}
	}
}
impl rusqlite::ToSql for LuaValueRef<'_> {
	fn to_sql(&self)->rusqlite::Result<rusqlite::types::ToSqlOutput<'_>> {
		use rusqlite::types::{ToSqlOutput::Owned,Value as SqlValue};
		match self.0 {
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
/// Value conversion in the Sql->Lua direction.
///
/// Note that this function needs access to the `[mlua::Lua]` instance so
/// that it may allocate strings.
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

/// Helper function for reading arguments passed to Lua callbacks.
///
/// Reads an argument as the given `FromLua` type and returns it,
/// wrapped in a `Result`.
fn pop_arg_as<'lua, T: mlua::FromLua<'lua>>(args: &mut mlua::MultiValue<'lua>, lua:&'lua Lua)->Result<T> {
	let arg0 = args.pop_front().ok_or(Error::CallbackMissingArgument)?;
	let r = T::from_lua(arg0, lua).with_context(||
		format!("cannot convert argument to type {}", std::any::type_name::<T>()))?;
	Ok(r)
}
/// Small simplification for [`mlua::MultiValue`] impl of [`AsParams`].
type MluaMultiIter<'a,'lua> = std::iter::Rev<std::slice::Iter<'a,mlua::Value<'lua>>>;
impl<'lua> AsParams for mlua::MultiValue<'lua> {
	type Elt<'a> = LuaValueRef<'a> where Self: 'a;
	type Iter<'a> = std::iter::Map<MluaMultiIter<'a,'lua>,fn(&'a mlua::Value<'lua>)->LuaValueRef<'a>> where Self: 'a;
	fn params_iter(&self)->Self::Iter<'_> {
		self.into_iter().map(LuaValueRef::from)
	}
}
impl<'lua> crate::struct_io::AsParams2 for mlua::MultiValue<'lua> {
	type Iter<'a> = MluaMultiIter<'a,'lua> where 'lua: 'a;
	type Target<'a> = LuaValueRef<'a> where 'lua: 'a;
	fn transform<'a>(a: &'a mlua::Value<'lua>)->Self::Target<'a> where 'lua: 'a { LuaValueRef::from(a) }
	fn params_iter(&self)->Self::Iter<'_> { self.into_iter() }
}

macro_rules! fail {
	($($a:tt)+) => { return Err(Error::$($a)+.into()) }
}
trait Callback<'a>: Debug+Sized {
	const NAME: &'static str;
	/// Builds the data for the callback from the table schema.
	fn prepare(db: &'a impl DbInterface, schema: &'a Schema)->Result<Self>;
	/// Runs the callback (from the selected table, etc.) and builds the
	/// resulting Lua value.
	fn execute<'lua>(&mut self, lua: &'lua Lua, args: mlua::MultiValue<'lua>)->Result<mlua::Value<'lua>>;
	/// Utility function to check that arguments match statement.
	fn expect_arguments(args: &mlua::MultiValue<'_>, expected: usize)->Result<()> {
		// We assume that one argument was discarded (table name).
		let found = args.len() + 1;
		if found != expected {
			Err(Error::BadArgumentNumber { function: Self::NAME, expected, found }.into())
		} else {
			Ok(())
		}
	}
}
/// Implementation of `simod.list`.
#[derive(Debug)]
struct ListKeys<'a>(Statement<'a>);
impl<'a> Callback<'a> for ListKeys<'a> {
	const NAME: &'static str = "list";
	fn prepare(db: &'a impl DbInterface, schema: &'a Schema)->Result<Self> {
		let mut s = format!(r#"select "{primary}" from "{schema}""#,
			primary = schema.primary());
		if matches!(schema.resource, crate::schemas::SchemaResource::Sub { .. }) {
			uwrite!(&mut s, r#" where "{resref}"=? order by "index""#,
				resref = schema.resref());
		}
		db.prepare(s).map(Self)
	}
	/// Implementation of `simod.list`.
	///
	/// This takes as parameters either
	/// 1. the name of a top-level resource table, or
	/// 2. the name of a sub-resource + a resref for the corresponding
	///    top-level resource,
	/// and returns in a Lua table the list of all primary keys matching
	/// this condition.
	fn execute<'lua>(&mut self, lua: &'lua Lua, args: mlua::MultiValue<'lua>)->Result<mlua::Value<'lua>> {
		Self::expect_arguments(&args, self.0.parameter_count()+1)?;
		let mut rows = self.0.query(args.as_params())?;
		let ret = lua.create_table()?;
		while let Some(row) = rows.next()? {
			let v_sql = row.get_ref(0)?;
			let v_lua = sql_to_lua(v_sql, lua)?;
			ret.push(v_lua)?;
		}
		Ok(mlua::Value::Table(ret))
	}
}
/// Implementation of the `simod.select` callback.
#[derive(Debug)]
struct SelectRow<'a>(Statement<'a>);
impl<'a> Callback<'a> for SelectRow<'a> {
	const NAME: &'static str = "select";
	fn prepare(db: &'a impl DbInterface, schema: &'a Schema)->Result<Self> {
		use crate::schemas::ColumnWriter;
		let s = schema.fields.cols().select_sql(schema,
			lazy_format!(r#"where "{primary}"=?"#, primary=schema.primary()));
		db.prepare(s).map(Self)
	}
	/// Implementation of `simod.list`.
	///
	/// This takes as parameters either
	/// 1. the name of a top-level resource table, or
	/// 2. the name of a sub-resource + a resref for the corresponding
	///    top-level resource,
	/// and returns in a Lua table the list of all primary keys matching
	/// this condition.
	fn execute<'lua>(&mut self, lua: &'lua Lua, args: mlua::MultiValue<'lua>)->Result<mlua::Value<'lua>> {
		Self::expect_arguments(&args, self.0.parameter_count()+1)?;
		let mut rows = self.0.query(args.as_params())?;
		match rows.next()? {
			Some(row) => {
				let ret = lua.create_table()?;
				for i in 0..row.as_ref().column_count() {
					let val = row.get_ref(i)
						.with_context(|| format!("cannot read field {i} in row"))?;
					ret.set(row.as_ref().column_name(i)?, sql_to_lua(val, lua)?)?;
				}
				Ok(mlua::Value::Table(ret))
			},
			None => Ok(mlua::Value::Nil),
		}
	}
}
/// Implementation of `simod.insert`.
#[derive(Debug)]
struct InsertRow<'a>(Statement<'a>,&'a Schema);
impl<'a> Callback<'a> for InsertRow<'a> {
	const NAME: &'static str = "insert";
	fn prepare(db: &'a impl DbInterface, schema: &'a Schema)->Result<Self> {
		let s = schema.insert_sql(NullDisplay(), NullDisplay());
		Ok(Self(db.prepare(s)?, schema))
	}
	fn execute<'lua>(&mut self, lua: &'lua Lua, mut args: mlua::MultiValue<'lua>)->Result<mlua::Value<'lua>> {
		Self::expect_arguments(&args, 2)?;
		let table = match args.pop_front().unwrap() {
			mlua::Value::Table(t) => t,
			_ => fail!(BadArgumentType { expected: "table" }),
		};
		// TODO: write an iterator over the schema fields,
		for pair in table.pairs() {
			let (k, value): (mlua::Value<'_>, mlua::Value<'_>) = pair?;
			let key = match k {
				mlua::Value::String(ref s) => s.to_str()?,
				_ => fail!(Any("bad table key".into())),
			};
			println!("got a table key: {key}; {value:?}", value = LuaValueRef(&value));
		}
		todo!()
	}
}


/// This extension trait allows factoring the code for selecting the
/// appropriate table for a Lua callback.
/// Once this is done, the individual [`Callback`] instance for this
/// table is run.
#[ext]
impl<'a, T: Callback<'a>> AllResources<T> {
	fn prepare(db: &'a impl DbInterface, schemas: &'a AllResources<Schema>)->Result<Self> where Self: Sized {
		schemas.map(|s| T::prepare(db, s))
	}
	/// Selects the appropriate individual callback from the first argument
	/// (table name) and runs it.
	fn execute<'lua>(&mut self, lua: &'lua Lua, mut args: mlua::MultiValue<'lua>)->Result<mlua::Value<'lua>> {
		let function = T::NAME;
		let table = pop_arg_as::<String>(&mut args, lua)
			.with_context(|| format!(
				"first argument to '{function}' callback must be a string"))?;
		self.by_name_mut(&table)?.execute(lua, args)
			.with_context(|| format!(
				"converting from SQL to Lua in '{function}' callback"))
	}
	/// The wrapper installing the callback function in a table.
	fn install_callback<'scope>(&'scope mut self, table: &mlua::Table<'scope>, scope: &mlua::Scope<'_,'scope>)->mlua::Result<()> {
// **NOTE**:
// The lifetime parameters were determined after **a lot** of trial
// and error....
// So this code is **precious** and we keep it for future reference.
// 	fn callback<'scope,'closure>(&'scope mut self, scope: &'closure mlua::Scope<'_,'scope>)->mlua::Result<mlua::Function<'closure>> {
// 		scope.create_function_mut(|lua, args|
// 			self.prepare(lua, args).to_lua_err())
// 	}
		table.set(T::NAME, scope.create_function_mut(|lua, args|
			self.execute(lua, args).to_lua_err())?)
	}
}

/// A collection of all the prepared SQL statements used for implementing
/// the Lua API.
///
/// Since we need to split this struct when passing `&mut Statement` values
/// to various `FnMut` closures, the fields are not typed as
/// `AllResources<Statement<'a>>` but as newtype wrappers;
/// each newtype runs only the appropriate conversion between the
/// prepared SQL statement and Lua values.
#[derive(Debug)]
struct LuaStatements<'a> {
// 	/// We keep an owned copy of the original table schemas.
// 	schemas: AllResources<Schema>,
	/// Prepared statements for `simod.list`.
	list_keys: AllResources<ListKeys<'a>>,
	select_row: AllResources<SelectRow<'a>>,
// 	select_row: SelectRow<'a>,
	// Prepared statements for `simod.select`.
}
impl<'a> LuaStatements<'a> {
	pub fn new(db: &'a impl DbInterface, schemas: &'a AllResources<Schema>)->Result<Self> {
		Ok(Self {
			list_keys: AllResources::<_>::prepare(db, schemas)?,
			select_row: AllResources::<_>::prepare(db, schemas)?,
// 			schemas,
		})
	}
}

/// Implementation of `simod.update`.
///
/// Updates a single field in one of the resource tables.
/// The type of value is not checked (TODO: do this either here or as a SQL
/// constraint when creating the tables?).
fn update(db: &impl DbInterface, (table, key, field, value): (String, String, String, mlua::Value<'_>))->Result<()> {
	scope_trace!("callback 'simod.update' invoked with: '{}' '{}' '{} '{:?}'",
		table, key, field, LuaValueRef(&value));
	let s = format!(
		r#"update "{table}" set "{field}"=? where "{primary}"='{key}'"#,
		primary = RESOURCES.table_schema(&table)?.primary());
	trace!("executing sql: {s} {:?}", LuaValueRef(&value));
	db.execute(&s, (LuaToSql(value),))?;
	Ok(())
}
/// Implementation of `simod.insert`.
fn insert(db: &impl DbInterface, (table, vals, context): (String, mlua::Table<'_>, mlua::Table<'_>))->Result<()> {
	let schema = RESOURCES.table_schema(&table)?;
	let mut stmt = db.prepare(&schema.insert_statement("", ""))?;
	let params = schema.params_from(|fname, ftype| {
		if ftype == FieldType::Rowid { Ok(LuaToSql(mlua::Value::Nil)) }
		else {
			match vals.get::<_,LuaToSql<'_>>(fname)? {
				LuaToSql(mlua::Value::Nil) => Ok(context.get(fname)?),
				x => Ok(x)
			}
		}
	})?;
	stmt.execute(params)?;
	// fix the primary key if needed
	if schema.is_subresource() {
		let rowid = db.last_insert_rowid();
		trace!("read subresource {table} with rowid {rowid}; assigning it to field {primary}", primary = schema.primary());
		context.set(schema.primary(), rowid)?;
	}
	Ok(())
}
/// Implementation of `simod.delete`.
fn delete(db: &impl DbInterface, (table, key): (String, mlua::Value<'_>))->Result<()> {
	let schema = RESOURCES.table_schema(&table)?;
	let primary = schema.primary();
	db.execute(format!(r#"delete from "{schema}" where "{primary}"=?"#),
		(LuaToSql(key),))?;
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
pub fn command_add(db: impl DbInterface, _target: &str)->Result<()> {
	use crate::struct_io::RowExt;
	let lua = Lua::new();
	let lua_file = Path::new("/home/jerome/src/infinity_compiler/init.lua");
	let schemas = all_schemas();
	let mut statements = LuaStatements::new(&db, &schemas)?;


	// We need to wrap the rusqlite calls in a Lua scope to preserve  the
	// lifetimes of the references therein:
	lua.scope(|scope| {
		let simod = lua.create_table()?;
		let lua_schema = lua.create_table()?;
		RESOURCES.map_mut(|schema, _| {
			let fields = lua.create_table()?;
			let context = lua.create_table()?;
			for col in schema.columns() {
				fields.set(col.fname, col.ftype.to_lua())?;
			}
			for col in schema.context() {
				context.push(col.fname)?;
			}
			let res_schema = lua.create_table()?;
			res_schema.set("fields", fields)?;
			res_schema.set("context", context)?;
			res_schema.set("primary", schema.primary())?;
			lua_schema.set(schema.to_string(), res_schema)?;
			Ok::<_,mlua::Error>(())
		})?;
		simod.set("schema", lua_schema)?;
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
		statements.list_keys.install_callback(&simod, scope)?;
		statements.select_row.install_callback(&simod, scope)?;
		simod.set("update", scope.create_function(
			|_lua, args| update(&db, args).to_lua_err())?)?;
		simod.set("insert", scope.create_function(
			|_lua, args| insert(&db, args).to_lua_err())?)?;
		simod.set("delete", scope.create_function(
			|_lua, args| delete(&db, args).to_lua_err())?)?;
		lua.globals().set("simod", simod)?;

		info!("loading file {lua_file:?}");
		lua.load(lua_file).exec()?;
		Ok(())
	})?;
	Ok(())
}
} // mod lua_api

pub(crate) mod resources;

use crate::prelude::*;
use clap::Parser;

fn type_of<T>(_:&T)->&'static str { std::any::type_name::<T>() }

use gamefiles::{GameIndex};
use progress::{Progress};
use resources::*;

/// Saves all modified game strings and resources to current directory.
///
/// This function saves in the current directory;
/// a chdir to the appropriate override directory needs to have been done
/// first.
fn save_resources(db: &impl DbInterface, game: &GameIndex)->Result<()> {
	let pb = Progress::new(2, "save all"); pb.as_ref().tick();
	gamestrings::save(db, game)?;
	pb.inc(1);
	Item::save_all(db)?;
	pb.inc(1);
	Ok(())
}
/// Saves game resources to filesystem.
///
/// This wraps [`save_resources`] so that any
/// exception throws do not prevent changing back to the previous
/// directory:
fn command_save_full(game: &GameIndex, mut db: GameDB)->Result<()> {
	// prepare database by translating resrefs and strrefs
	db.transaction(|db| {
		db.translate_resrefs()?;
		db.translate_strrefs()?;

		// save in a temp directory before moving everything to override
		let tmpdir = game.tempdir()?;
		gamefiles::with_dir(&tmpdir, || {
			debug!("saving to temporary directory {:?}", tmpdir);
			save_resources(db, game)
		})?;
		debug!("installing resources saved in temporary directory {:?}", tmpdir);
		game.restore(tmpdir)?;
		db.clear_orphan_resources()?;
		db.unmark_dirty_resources()?;
		Ok(())
	})
}
fn command_save_diff(game: &GameIndex, mut db: GameDB)->Result<()> {
	db.transaction(|db| {
		db.translate_resrefs()?;
		db.translate_strrefs()?;
		let override_dir = game.root.join("override");
		gamefiles::with_dir(&override_dir, || {
			debug!("differential save to {override_dir:?}");
			// clear orphan resources **before** installing new resources —
			// a resource might legitimately be both orphan and still existing
			// (if it was removed and then added again in the DB), in which case we
			// want to save it to override:
			db.clear_orphan_resources()?;
			save_resources(db, game)
		})?;
		debug!("resources saved!");
		db.unmark_dirty_resources()?;
		Ok(())
	})
}
fn command_show(db: &GameDB, target: impl AsRef<str>)->Result<()> {
	let target = target.as_ref();
	let (resref, resext) = match target.rfind('.') {
		None => return Ok(()),
		Some(i) => (&target[..i], &target[i+1..]),
	};
	match resext.to_lowercase().as_ref() {
		"itm" => Item::show_all(db, resref),
		x => { warn!("unknown resource type: {x}"); Ok(()) },
	}
}
/// Restores all backed up files.
fn command_restore(game: &GameIndex)->Result<()> {
	game.restore(&game.backup_dir)
		.with_context(|| format!("failed to restore game files from {:?}",
			game.backup_dir))
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
	/// Initializes the database from the game installation.
	Init {
#[arg(short='B',default_value_t=false,help="Don't abort even if backup fails.")]
		ignore_backup_fail: bool,
	},
	/// Saves the database to the game installation (differential save).
	Save,
	FullSave,
	Restore,
	/// Installs a mod component by running the associated Lua script.
	Add {
#[arg(help="The name of the mod script to install.")]
		target: String,
	},
	Remove,
	Select,
	Show {
		target: String,
	},
	/// Displays the schema for a given game resource.
	///
	/// If no name is given then the list of game resources is displayed
	/// instead.
	Schema {
#[arg(help="The name of the SQL table to display.")]
		table: Option<String>,
	},
}

fn main() -> Result<()> {
	let mut options = RuntimeOptions::parse();
	if options.database.is_none() {
		options.database = Some(Path::new("game.sqlite").into());
	}
	let game = GameIndex::new(&options.gamedir)
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
		Command::Init{ ignore_backup_fail, } => {
			game.maybe_backup(game.root.join("simod").join("backup"), ignore_backup_fail)?;
			GameDB::create(&db_file,&game)?.load(&game)?
		},
		Command::FullSave => command_save_full(&game, GameDB::open(db_file)?)?,
		Command::Save => command_save_diff(&game, GameDB::open(db_file)?)?,
		Command::Restore => command_restore(&game)?,
		Command::Show{ target, .. } =>
			command_show(&GameDB::open(db_file)?, target)?,
		Command::Add{ target, .. } =>
			lua_api::command_add(GameDB::open(db_file)?, &target)?,
		Command::Schema{ table, .. } => match table {
			None => {
				all_schemas().map(|schema| any_ok(println!("{}", schema.name)))?;
			},
			Some(s) => {
				let schema = all_schemas().by_name(&s)?;
				for crate::schemas::Field { fname, ftype, .. } in &schema.fields {
					println!("{fname:30?}{}", ftype.to_lua());
				}
			},
			},
		_ => todo!(),
	};
	info!("execution terminated with flying colors");
	Ok(())
}
