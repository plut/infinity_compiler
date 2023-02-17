//! Compiler between IE game files and a SQLite database.
#![allow(
	unused_attributes,
// 	unused_imports,
// 	dead_code,
// 	unreachable_code,
	unused_macros,
	unused_variables,
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
// 	missing_docs,
)]
// #![feature(trace_macros)]
// trace_macros!(false);

pub(crate) mod prelude {
//! The set of symbols we want accessible from everywhere in the crate.
#![allow(unused_imports)]
pub(crate) use anyhow::{Result,Context};
pub(crate) use log::{trace,debug,info,warn,error};
pub(crate) use rusqlite::{self,Connection,Statement,Row};
pub(crate) use extend::ext;
pub(crate) use lazy_format::prelude::*;
pub(crate) use once_cell::sync::{Lazy,OnceCell};

pub(crate) use core::marker::PhantomData;
pub(crate) use std::ops::{Deref,DerefMut};
pub(crate) use std::fmt::{self,Display,Debug,Formatter,Write as FWrite};
pub(crate) use std::fs::{self,File};
pub(crate) use std::io::{self,Cursor,Read,Write,Seek,SeekFrom};
pub(crate) use std::path::{Path, PathBuf};
pub(crate) use std::convert::{Infallible};

pub(crate) use macros::{Pack,SqlRow};
pub(crate) use crate::gamefiles::{Resref,Strref};
pub(crate) use crate::database::{GameDB,DbInterface};
pub(crate) use crate::toolbox::{Progress,scope_trace,NullDisplay};

pub(crate) fn infallible<T>(x: T)->std::result::Result<T,Infallible>{ Ok(x) }
pub(crate) fn any_ok<T>(x: T)->Result<T> { Ok(x) }
/// Alias for `()`; makes some code easier to read:
#[allow(non_upper_case_globals)]
pub(crate) const nothing: () = ();
/// Alias for `((),())` (pair of empty tuples); makes some code easier to read:
#[allow(non_upper_case_globals)]
pub(crate) const nothing2: ((),()) = (nothing, nothing);


/// Our own error type. Mainly used for errors from Lua callbacks.
#[derive(Debug)]
pub enum Error{
	UnknownTable(String),
	BadArgumentNumber { expected: usize, found: usize },
	BadArgumentType { position: usize, expected: &'static str, found: String },
	BadParameterCount { expected: usize, found: usize },
	CallbackMissingArgument,
	UnknownField { field: String },
}
impl Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>)->fmt::Result {
		match self {
			Self::UnknownTable(s) => write!(f, "Unknown table: '{s}'"),
			Self::BadArgumentNumber { expected, found } =>
				write!(f, "Bad number of arguments: found {found}, expected {expected}"),
			Self::BadArgumentType { position, expected, found } =>
				write!(f, "Bad argument type at position {position}: expected {expected}, found {found}"),
			Self::BadParameterCount { expected, found } =>
				write!(f, "Bad parameter count for SQL statement: found {found}, expected {expected}"),
			Self::UnknownField { field } => write!(f, r#"Unknown field "{field}""#),
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
pub mod toolbox {
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
#[derive(Debug)]
pub struct ScopeLogger(pub log::Level);
impl Drop for ScopeLogger {
	fn drop(&mut self) { log::log!(self.0, "»»") }
}
macro_rules! scope_trace {
	($msg: literal $($arg:tt)*) => {
		let _log_scope = crate::toolbox::ScopeLogger(log::Level::Trace);
		log::trace!(concat!($msg, "««") $($arg)*);
	}
}
pub(crate) use scope_trace;
} // mod toolbox
pub mod pack {
//! I/O to binary format. See the [`Pack`] trait.
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
/// Convenience trait used for packing vectors in one call.
pub trait PackAll {
	/// Packs all elements of a vector in one pass.
	fn pack_all(self, f: &mut impl Write)->io::Result<()>;
}
impl<'a, T: Pack+'a, I: IntoIterator<Item=&'a T>> PackAll for I {
	fn pack_all(self, mut f: &mut impl Write)->io::Result<()> {
		for x in self {
			x.pack(&mut f)?;
		}
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
/// A trait which provides a dummy pack implementation.
pub trait NoPack: Default { }
impl<T: NoPack> Pack for T {
	fn unpack(_f: &mut impl Read)->io::Result<Self> { Ok(Self::default()) }
	fn pack(&self, _f: &mut impl Write)->io::Result<()> { Ok(()) }
}
impl NoPack for String { }
impl<T> NoPack for Vec<T> { }

}
pub mod staticstrings {
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
	/// Converts in-place to lowercase.
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
	pub fn generate(&mut self)->&StaticString<N> {
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
pub mod sql_rows {
//! Basic types for interaction with SQL and binary files.
//! The main entry points for this module are the traits [`Pack`]
//! and [`SqlRow`].
use crate::prelude::*;
use rusqlite::{ToSql};
use rusqlite::types::{FromSql,ValueRef};

/// An extension trait allowing to dump a row from SQL.
pub trait RowExt {
	/// dumps the content of this row to stdout.
	fn dump(&self);
}
impl RowExt for Row<'_> {
	/// Dumps all values found in a single row to stdout.
	fn dump(&self) {
		for (i, c) in self.as_ref().column_names().iter().enumerate() {
			match self.get_ref(i) {
				Ok(ValueRef::Null) => println!("  [{i:3}] {c} = \x1b[31mNull\x1b[m"),
				Ok(ValueRef::Text(s)) => println!("  [{i:3}] {c} = Text(\"{}\")",
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
}
impl FieldType {
	/// Describes this type to SQLite.
	pub const fn affinity(self)->&'static str {
		match self {
			Self::Integer | Self::Strref => r#"integer default 0"#,
			Self::Text | Self::Resref => r#"text default """#,
		}
	}
	/// Describes this type as a string.
	pub const fn description(self)->&'static str {
		match self {
			FieldType::Integer => "integer",
			FieldType::Text => "text",
			FieldType::Resref => "resref",
			FieldType::Strref => "strref",
		}
	}
}
/// A leaf SQL type: this can be converted from, to SQL and knows its own
/// affinity.
pub trait SqlLeaf: FromSql + ToSql {
	/// How this field is handled database-side.
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

/// Converting an object to something implementing [`rusqlite::Params`].
///
/// Since we cannot implement the [`rusqlite::Params`] trait ourselves,
/// this is the closest we can do.
pub trait AsParams {
	/// Element type of the iterator we use to map to SQL.
	type Elt<'a>: ToSql where Self: 'a;
	/// Iterator we use to map to SQL.
	type Iter<'a>: Iterator<Item=Self::Elt<'a>> where Self: 'a;
	/// Converts this to an iterator.
	fn params_iter(&self)->Self::Iter<'_>;
	/// Converts this to some object implementing [`rusqlite::Params`].
	fn as_params(&self)->rusqlite::ParamsFromIter<Self::Iter<'_>> {
		rusqlite::params_from_iter(self.params_iter())
	}
}

/// Data statically attached to the schema of a table.
#[derive(Debug,Clone,Copy)]
pub struct SqlRowData {
	/// Columns of this table
	pub fields: crate::schemas::Fields,
	/// Extension for a top-resource, or empty string
	pub ext: &'static str,
}
/// Structure which can be read from (part of) a SQL row,
/// or bound to (part of) a SQL statement.
pub trait SqlRow: Sized {
	const FIELDS: SqlRowData;
	/// The description of the columns that this binds to.
	const FIELDS9: crate::schemas::Fields;
	/// Binds to columns [offset, ...] of statement.
	fn bind_at(&self, s: &mut Statement<'_>, offset: usize)->Result<()>;
	/// Reads from columns [offset, ... ] of row.
	fn collect_at(s: &Row<'_>, offset: usize)->Result<Self>;
	/// Checks that the number of columns is good.
	fn check_parameter_count(s: &mut Statement<'_>, extra: usize)->Result<()> {
		let found = Self::FIELDS.fields.len() + extra;
		let expected = s.parameter_count();
		if found == expected {
			Ok(())
		} else {
			Err(rusqlite::Error::InvalidParameterCount(found, expected).into())
		}
	}
	/// Binds this object with a header of a single parameter, and executes
	/// the statement.
	fn bind_execute1<P: ToSql>(&self, stmt: &mut Statement<'_>, ctx: P)->Result<usize> {
		Self::check_parameter_count(stmt, 1)?;
		stmt.raw_bind_parameter(1, ctx)?;
		self.bind_at(stmt, 1)?;
		stmt.raw_execute().context("raw_execute")
	}
	/// Binds this object with a header of two parameters, and executes the
	/// statement.
	fn bind_execute2<P1: ToSql, P2: ToSql>(&self, stmt: &mut Statement<'_>, c1: P1, c2: P2)->Result<usize> {
		Self::check_parameter_count(stmt, 2)?;
		stmt.raw_bind_parameter(1, c1)?;
		stmt.raw_bind_parameter(2, c2)?;
		self.bind_at(stmt, 2)?;
		stmt.raw_execute().context("raw_execute")
	}
}

/// A structure collectable from a SQL statement.
///
/// Functionally almost identical to `TryFrom<Row<'_>>`, except (1) it
/// avoids a useless lifetime parameter, and (2) we own this trait and
/// thus are allowed to implement it on e.g. (Strref, GameString).
pub trait FromSqlRow: Sized {
	/// Tries to collect a row into this structure.
	fn from_row(row: &Row<'_>)->Result<Self>;
	/// Converts a statement into an iterator of `Self` structures.
	fn iter<'a>(stmt: &'a mut Statement<'_>, params: impl rusqlite::Params)->rusqlite::Result<TypedRows<'a,Self>> {
		Ok(TypedRows { rows: stmt.query(params)?, index: 0, _marker: PhantomData })
	}
}
impl<P: FromSql, T: SqlRow> FromSqlRow for (P, T) {
	fn from_row(row: &Row<'_>)->Result<Self> {
		Ok((row.get(1)?, T::collect_at(row, 1)?))
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
pub struct TypedRows<'stmt, T:FromSqlRow> {
	rows: rusqlite::Rows<'stmt>,
	index: usize,
	_marker: PhantomData<T>,
}
impl<'a, T:FromSqlRow> From<rusqlite::Rows<'a>> for TypedRows<'a,T> {
	fn from(rows: rusqlite::Rows<'a>)->Self {
		Self { rows, index: 0, _marker: PhantomData }
	}
}
impl<T: FromSqlRow> Iterator for TypedRows<'_,T> {
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

} // mod sql_rows
pub mod gamefiles {
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
use rusqlite::types::{FromSql,ToSql, ValueRef,ToSqlOutput,FromSqlResult,FromSqlError};

use io::BufReader;

use crate::toolbox::{Progress};
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
pub struct Resref(StaticString<8>);
impl Debug for Resref {
	fn fmt(&self, f:&mut Formatter<'_>)->fmt::Result { Debug::fmt(&self.0, f) }
}
impl Display for Resref {
	fn fmt(&self, f:&mut Formatter<'_>)->fmt::Result {Display::fmt(&self.0, f)}
}
impl Pack for Resref {
	fn unpack(f: &mut impl Read)->io::Result<Self> {
		let mut name = StaticString::<8>::unpack(f)?;
		name.make_ascii_lowercase();
		Ok(Self(name))
	}
	fn pack(&self, f: &mut impl Write)->io::Result<()> { self.0.pack(f) }
}
impl ToSql for Resref {
	fn to_sql(&self)->rusqlite::Result<ToSqlOutput<'_>> {
		self.0.as_ref().to_sql()
	}
}
impl FromSql for Resref {
	fn column_result(v: ValueRef<'_>)->FromSqlResult<Self> {
		match v {
			ValueRef::Text(s) => Ok(Self(s.into())),
			ValueRef::Null => Ok(Self("".into())),
			_ => Err(FromSqlError::InvalidType)
		}
	}
}
impl Resref {
	/// iterates until `test` returns `false`.
	pub fn fresh(source: &str, mut is_used: impl FnMut(&Resref)->rusqlite::Result<bool> )->rusqlite::Result<Self> {
		let mut gen = crate::staticstrings::Generator::<8>::new(source);
		loop {
			let resref = Resref(*gen.generate());
			if !is_used(&resref)? { return Ok(resref) }
		}
	}
}
/// A string reference as used by the game: 32-bit integer.
#[derive(Debug,Pack,Clone,Copy)]
pub struct Strref(pub i32);
impl Display for Strref {
	fn fmt(&self, mut f: &mut Formatter<'_>)->std::result::Result<(),fmt::Error>{
		write!(&mut f, "@{}", self.0)
	}
}
impl ToSql for Strref {
	fn to_sql(&self)->rusqlite::Result<ToSqlOutput<'_>> { self.0.to_sql() }
}
impl FromSql for Strref {
	fn column_result(v: ValueRef<'_>)->FromSqlResult<Self> {
		i32::column_result(v).map(Self)
	}
}
/// A 16-bit value describing a resource type in the Key file.
#[derive(Debug,Pack,Clone,Copy,PartialEq,Eq)] pub struct Restype (pub u16);
/// A 32-bit vlaue describing the position of the resource in the BIF file.
#[derive(Debug,Pack,Clone,Copy)] pub struct BifIndex(u32);
/// A reference to a resource inside a BIF file as encoded in
/// `chitin.key`.
impl BifIndex {
	fn sourcefile(&self)->usize { (self.0 >> 20) as usize }
	fn resourceindex(&self)->usize { (self.0 & 0x3fff) as usize }
#[allow(dead_code)]
	fn tilesetindex(&self)->usize { ((self.0 >> 14) & 0x3f) as usize }
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
	/// Lazily opens a BIF file.
	pub fn new(path: PathBuf)->Self { Self { contents: None, path } }
}
/// A (lazy) file descriptor of a game reosurce.
///
/// This encapsulates both the case of an override game resource and a
/// BIF game resource. In the second case, access to the BIF file is
/// lazy: this file is loaded only when [`ResReader::open`] is called.
#[derive(Debug)]
pub enum ResReader<'a> {
	/// A resource stored at some position inside a BIF file.
	Bif(&'a mut BifFile, BifIndex, Restype),
	/// A resource stored in its own file.
	Override(&'a Path),
}
impl ResReader<'_> {
	/// Tests where the resource is stored.
	pub fn is_override(&self)->bool { matches!(self, Self::Override(_)) }
	/// Opens the file and returns a [`Cursor`] pointing to the data.
	pub fn open(&mut self)->Result<Cursor<Vec<u8>>> {
		match self {
		Self::Bif(bif, location, restype) =>
			bif.read(location.resourceindex(), *restype),
		Self::Override(path) => Ok(Cursor::new(fs::read(&path)
				.with_context(|| format!("cannot open override file: {path:?}"))?)),
		}
	}
}
/// A (lazy) accessor to a game resource.
#[derive(Debug)]
pub struct ResHandle<'a> {
	/// The name attached to this resource.
	pub resref: Resref,
	reader: ResReader<'a>,
}
impl ResHandle<'_> {
	pub fn open(&mut self)->Result<Cursor<Vec<u8>>> { self.reader.open() }
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
/// Executes a closure in a given directory and returns to the original
/// directory as soon as the closure returns.
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
	where F: (FnMut(Restype, ResHandle<'_>)->Result<()>)
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
				let resref = Resref(name[..pos].into());
				let ext = &name[pos+1..];
				let restype = Restype::from(ext);
				trace!("reading override file: {name}; restype={restype:?}");
				let path = entry.path();
				let handle = ResHandle { resref, reader: ResReader::Override(&path) };
				f(restype, handle)?
			}
		}
		scope_trace!("iterating over game resources from BIF");
		for (sourcefile, filename) in self.bifnames.iter().enumerate() {
			let path = self.root.join(filename);
			let mut bif = BifFile::new(path);
			let pb1 = Progress::new(self.resources.len(), filename);
			for res in &self.resources {
				if res.location.sourcefile() != sourcefile { continue }
				let handle = ResHandle { resref: res.resref,
					reader: ResReader::Bif(&mut bif, res.location, res.restype,) };
				pb.inc(1);
				pb1.inc(1);
				f(res.restype, handle)?
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
pub mod schemas {
//! Inner description of SQL table.
//!
//! This mod groups everything which has access to the content of the
//! [`Schema`] struct.
use crate::prelude::*;
use crate::sql_rows::{FieldType};

/// Description of a field in a game resource.
#[derive(Debug,Clone,Copy)]
pub struct Field {
	/// Name of this column.
	pub fname: &'static str,
	/// Content type of this column.
	pub ftype: FieldType,
	/// Any extra information given to SQLite when creating the table.
	pub create: &'static str,
}
/// Displays the quoted field name.
/// This is a useful help for writing SQL statements.
impl Display for Field {
	fn fmt(&self, f: &mut Formatter<'_>)->fmt::Result {
		write!(f, r#""{n}""#, n = self.fname)
	}
}

/// A newtype around a slice of [`Field`] values,
/// allowing us to simplify some SQL writing.
#[derive(Debug,Clone,Copy)]
pub struct Fields(pub &'static [Field]);
impl Fields {
	pub(crate) fn len(&self)->usize { self.0.len() }
	pub fn iter(&self)->std::slice::Iter<'_,Field> { self.0.iter() }
	fn with_prefix<T: Display>(self, prefix: T)->FieldsWithPrefix<T> {
		FieldsWithPrefix(self, prefix)
	}
	/// Returns the SQL insert statement for these columns, as a String.
	pub fn insert_sql(self, name: impl Display,
		more: impl Display, more_n: usize)->String {
		let mut sql = format!("insert into \"{name}\" ({more}{self})
	values(");
		for c in 0..self.len() + more_n  {
			if c > 0 { sql.push(','); }
			sql.push('?');
		}
		sql.push(')');
		sql
	}
}
impl Display for Fields {
	fn fmt(&self, f: &mut Formatter<'_>)->fmt::Result {
		Display::fmt(&self.with_prefix(NullDisplay()), f)
	}
}
/// A helper type to insert new."fields" inside a query.
#[derive(Debug)]
pub struct FieldsWithPrefix<T: Display>(Fields, T);
impl<T: Display> Display for FieldsWithPrefix<T> {
	fn fmt(&self, f: &mut Formatter<'_>)->fmt::Result {
		let mut isfirst = true;
		for element in self.0.iter() {
			if isfirst { isfirst = false; } else { write!(f, ",")?; }
			write!(f, r#" {prefix}{element}"#, prefix = self.1)?;
		}
		Ok(())
	}
}

/// Identifies whether this is a top resource or a sub-resource.
#[derive(Debug,Clone)]
pub enum TableType {
	/// A top-level resource (associated to a given file extension).
	Top { extension: &'static str, },
	/// A sub-resource
	Sub { parent: String, root: &'static str, },
}
/// The full database description of a game resource.
///
/// This contains all relevant information to fully define a resource
/// on the SQL side.
///
/// In practice there exists exactly one [`Schema`] instance per
/// resource, and it is compiled by the [`SqlRow`] derive macro.
#[derive(Debug)]
pub struct Schema {
	/// The nesting level for this resource (0 for top-level resources).
	pub level: usize,
	/// Name of the SQL table mapped to this data.
	pub name: String,
	/// Position of the schema in the arborescence.
	pub table_type: TableType,
	/// Description of the SQL table for this data.
	pub fields: Fields,
}
impl Display for Schema {
	fn fmt(&self, f: &mut Formatter<'_>)->fmt::Result { f.write_str(&self.name) }
}
impl Schema {
	/// `true` iff this is not a top-level resource.
	pub fn is_subresource(&self)->bool {
		matches!(self.table_type, TableType::Sub { .. })
	}
	/// Creates a table with this schema and an arbitrary name.
	pub fn create_table(&self, name: impl Display, more: impl Display)->String {
		let mut sql = format!("create table \"{name}\" (");
		if let TableType::Sub { parent, .. } = &self.table_type {
			uwrite!(&mut sql, r#"
	"id" integer primary key,
	"position" integer,
	"parent" references "{parent}"("id") on delete cascade"#);
		} else {
			// We put "on conflict ignore" so that earlier insertions are
			// prioritized — then we can detect (using number of changed rows)
			// that an insertion fails and skip inserting sub-resources
			uwrite!(&mut sql, r#""id" string primary key on conflict ignore"#);
		}
		for Field { fname, ftype, create, .. } in self.fields.iter() {
			uwrite!(&mut sql, ",\n \"{fname}\" {} {create}", ftype.affinity());
		}
		uwrite!(&mut sql, "{more})");
		sql
	}
	/// Returns the SQL code creating the main view (e.g. `"items"`).
	pub fn create_main_view(&self)->String {
		let mut sql = format!(r#"create view "{self}" as
	select "source"."id""#);
		let source_position: &'static str;
		if let TableType::Sub { parent, .. } = &self.table_type {
			source_position = r#""parent", "position", "#;
			uwrite!(&mut sql, r#",
	"{parent}"."root" as "root",
	"source"."parent" as "parent",
	"source"."position" as "position""#);
		} else {
			source_position = "";
			uwrite!(&mut sql, r#",
	"source"."id" as "root""#);
		}
		for Field { fname, .. } in self.fields.iter() {
			uwrite!(&mut sql, r#",
	ifnull((select "value" from "edit_{self}" where "line"="source"."id" and "field"='{fname}' order by rowid desc limit 1), "source"."{fname}") as "{fname}""#);
		}
		uwrite!(&mut sql, r#"
from
	(select "id", {source_position}{fields} from "load_{self}" union select "id", {source_position}{fields} from "add_{self}")
as "source""#, fields = self.fields);
		if let TableType::Sub{ parent, .. } = &self.table_type {
			uwrite!(&mut sql, r#"
	inner join "{parent}" on "source"."parent" = "{parent}"."id""#);
		}
		sql
	}
	/// Creates the `save_xxx` view.
	pub fn create_save_view(&self)->String {
		let mut select = format!(r#"create view "save_{self}" as select"#);
		let mut source = format!(r#"
from "{self}" as "a""#);
		for Field { fname: f, ftype, .. } in self.fields.iter() {
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
		uwrite!(&mut select, r#""a"."root" as "root"{source}"#);
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
		// read-only table of initial resources:
		f(self.create_table(&format!("load_{self}"), NullDisplay()))?;
		// table of resources inserted by mods:
		// (the extra field designates the mod)
		f(self.create_table(&format!("add_{self}"), r#", "source" text"#))?;
		// table of fields edited by mods:
		f(format!(r#"create table "edit_{self}" ("source" text, "line", "field" text, "value")"#))?;
		f(self.create_main_view())?;
		f(self.create_save_view())?;
		let root = match self.table_type {
			TableType::Sub { root, .. } => root,
			_ => {
			// only for top-level resource: create the dirty and orphan tables
			f(format!(r#"create table "dirty_{self}" ("name" text primary key on conflict ignore)"#))?;
			f(format!(r#"create table "orphan_{self}" ("name" text primary key on conflict ignore)"#))?;
			f(format!(r#"create trigger "orphan_{self}" after delete on "add_{self}"
begin
	insert into "orphan_{self}" values (old."id");
end"#))?;
			&self.name
		},
		};
		// populate resref_dict or strref_dict as needed:
		// There are two triggers which edit "resref_dict"; we build both
		// of them from resref fields at once, by storing the text in
		// `trans` (for update triggers) and `trans_insert` (for insert
		// trigger).
		let mut trans_insert = String::new();
		for Field {fname, ftype, ..} in self.fields.iter() {
			let trans = match ftype {
			FieldType::Resref => format!(
	r#"insert or ignore into "resref_dict" values (new."{fname}", null);"#),
// 			FieldType::Strref => format!(
// 	r#"insert or ignore into "strref_dict" values (new."{fname}", null);"#),
			_ => String::new()
			};
			trans_insert.push_str(&trans);
			f(format!(
r#"create trigger "update_{self}_{fname}"
instead of update of "{fname}" on "{self}"
begin
	{trans}
	insert or ignore into "dirty_{root}" values (new."root");
	insert into "edit_{self}" ("source", "line", "field", "value") values
		((select "component" from "global"), new."id", '{fname}', new."{fname}");
end"#))?;
		}
		f(format!(
r#"create trigger "insert_{self}"
instead of insert on "{self}"
begin
	{trans_insert}
	insert into "add_{self}" ({cols}) values ({newcols});
	insert or ignore into "dirty_{root}" values (new."root");
end"#, cols = self.fields, newcols = self.fields.with_prefix("new.")))?;
		f(format!(
r#"create trigger "delete_{self}"
instead of delete on "{self}"
begin
	insert or ignore into "dirty_{root}" values (old."root");
	delete from "add_{self}" where "id" = old."id";
end"#))?;
		f(format!(
r#"create trigger "undo_{self}"
before delete on "edit_{self}"
begin
	insert or ignore into "dirty_{root}"
	select "root" from "{self}" where "id" = old."line";
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
	pub fn insert_sql(&self, prefix: impl Display)->String {
		let (more, more_n) = match self.table_type {
			TableType::Top { .. } => (r#""id", "#, 1),
			TableType::Sub { .. } => (r#""parent", "position", "#, 2),
		};
		self.fields.insert_sql(lazy_format!("{prefix}{self}"), more, more_n)
	}
	/// Helper function for generating `new_strings` view.
	pub fn append_new_strings_schema(&self, w: &mut impl fmt::Write, is_first: &mut bool) {
		for Field { fname, ftype, .. } in self.fields.iter() {
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
		let mut is_first_field = true;
		for Field { fname, ftype, .. } in self.fields.iter() {
			let root = match self.table_type {
				TableType::Sub { root, .. }=> root,
				TableType::Top { .. }  => &self.name
			};
			if *ftype != FieldType::Strref { continue }
			if is_first_field {
				uwrite!(w, r#"
	insert into "dirty_{root}" select "root" from "{self}" where "#);
				is_first_field = false;
			}
			else { uwrite!(w, " or "); }
			uwrite!(w, r#""{fname}"=new."native""#);
		}
		if !is_first_field {
			uwriteln!(w, ";");
		}
	}
	/// Displays a full description of the schema on stdout.
	pub fn describe(&self) {
		println!("name={self}\nheader = {:?}", self.table_type);
		for (i, f) in self.fields.iter().enumerate() {
			println!("{i:2} {:<20} {}", f.fname, f.ftype.description());
		}
	}
	/// Returns the SQL statement for saving resources to filesystem.
	pub fn select_dirty_sql(&self)->String {
		match self.table_type {
			TableType::Top { .. } =>
			format!(r#"select "id", {cols} from "{self}" where "id" in "dirty_{self}""#, cols = self.fields),
			TableType::Sub { .. } =>
			format!(r#"select "id", {cols} from "{self}" where "parent"=? order by "position""#, cols = self.fields),
		}
	}
}

} // mod schemas
pub mod resources {
//! Infrastructure for recursive, tree-like reosurces and subresources.
//! Definition of specific resources goes to `restypes` mod.
use crate::prelude::*;
use rusqlite::{ToSql,types::{FromSql}};
use crate::sql_rows::{SqlRow,TypedRows};
use crate::schemas::{Schema,Fields};
use crate::gamefiles::Restype;
use crate::restypes::{RootForest};
pub trait ResourceTree: SqlRow {
	/// Always `Tree<FooForest<Fields>>`.
	type FieldsTree;
	/// SQL statements in the database, linked to all the resources tables.
	type StatementForest<'a>: 'a + Forest<In=Statement<'a>>;
	/// The tree holding the fields description for this resource and all
	/// sub-resources.
	const FIELDS_TREE: Self::FieldsTree;
	/// The type of the primary key for this resource:
	/// either Resref for top-level or i64 for sub-resources.
	type Primary: ToSql + FromSql + Display + Copy;
	/// Given an object inserted into the database,
	/// insert its sub-resources. Generated by macro.
	/// and mutually recursive with `(subresources)::insert_as_subresource`.
	fn insert_subresources(&self, db: &Connection, branches: &mut Self::StatementForest<'_>, primary: impl ToSql+Copy)->Result<()>;
	/// Complete a partially-initialized object from the db
	/// by reading all its sub-resource fields. Generated by macro.
	fn select_subresources(&mut self, branches: &mut Self::StatementForest<'_>,
		primary: Self::Primary)->Result<()>;
	/// Inserts a top-level resource.
	/// This differs from subresources in that the primary key (resref) is
	/// known.
	///
	/// This is the entry point called when populating the database;
	/// this function is not called recursively.
	fn insert_as_topresource(&self, db: &Connection, tree: &mut Tree<Self::StatementForest<'_>>, resref: Self::Primary)->Result<usize> {
		let n = self.bind_execute1(&mut tree.content, resref)?;
		if n == 0 {
			warn!("skipped inserting {resref}");
			return Ok(n)
		}
		self.insert_subresources(db, &mut tree.branches, resref)?;
		Ok(1)
	}
	/// Inserts a sub-resource.
	///
	/// This differs from a top-level resource in that the primary key
	/// (rowid) is not known yet; instead it is derived from
	/// `last_insert_rowid`.
	///
	/// This function is mutually recursive with
	/// `Self::insert_subresources` (derived from macro).
	fn insert_as_subresource(&self, db: &Connection, tree: &mut Tree<Self::StatementForest<'_>>, parent: impl ToSql, position: usize)->Result<()> {
		self.bind_execute2(&mut tree.content, parent, position)?;
		let primary = db.last_insert_rowid();
		self.insert_subresources(db, &mut tree.branches, primary)?;
		Ok(())
	}
	fn collect_all(node: &mut Tree<Self::StatementForest<'_>>, params: impl rusqlite::Params)->Result<Vec<Self>> {
		todo!()
	}
}
/// An iterator building full recursive resources from a statement tree.
#[allow(missing_debug_implementations)]
pub struct RecursiveRows<'stmt, T: ResourceTree> {
	rows: TypedRows<'stmt,(T::Primary,T)>,
	tree: &'stmt mut Tree<T::StatementForest<'stmt>>,
}
impl<'a,T: ResourceTree> RecursiveRows<'a,T> {
	fn new(tree: &'a mut Tree<T::StatementForest<'a>>, params: impl rusqlite::Params)->Result<Self> {
		todo!()
// 		let stmt = node.deref_mut();
// 		let rows = stmt.query(params)?;
// 		Ok(Self { rows: rows.into(), node })
	}
}
impl<T: ResourceTree> Iterator for RecursiveRows<'_,T> {
	type Item = Result<(T::Primary, T)>;
	fn next(&mut self)->Option<Self::Item> {
		match self.rows.next() {
			None => None,
			Some(Err(e)) => Some(Err(e)),
			Some(Ok((primary, mut resource))) =>
				match resource.select_subresources(&mut self.tree.branches, primary) {
					Err(e) => Some(Err(e)),
					_ => Some(Ok((primary, resource))),
			}
		}
	}
}
#[derive(Debug)]
pub struct Tree<T: Forest> {
	pub content: T::In,
	pub branches: T,
}
impl<X: Debug, T: Forest<In=X>> Tree<T> {
	pub fn by_name1<'a>(&'a self, s: &str)->Option<&'a X> {
		if s.is_empty() { return Some(&self.content) }
		if s.as_bytes()[0] != b'_' { return None }
		self.branches.by_name1(&s[1..])
	}
	pub fn by_name_mut1<'a>(&'a mut self, s: &str)->Option<&'a mut X> {
		if s.is_empty() { return Some(&mut self.content) }
		if s.as_bytes()[0] != b'_' { return None }
		self.branches.by_name_mut1(&s[1..])
	}
	pub fn by_name<'a>(&'a self, target: &str)->Result<&'a X> {
		self.by_name1(target).ok_or(Error::UnknownTable(target.into()).into())
	}
	/// Same, with mutable reference.
	pub fn by_name_mut<'a>(&'a mut self, target: &str)->Result<&'a mut X> {
		self.by_name_mut1(target).ok_or(Error::UnknownTable(target.into()).into())
	}
	/// Recursively traverse the tree from its root,
	/// applying the closure on each element. Used by `traverse` and `map`.
	///
	/// For each node in the tree, the closure `f` is invoked with:
	///  - the content of the node,
	///  - the name of this node (as a `&'static str`),
	///  - the state computed for the parent (as an `Option`),
	/// It should return a tuple of:
	///  - the new state value passed to children,
	///  - the value computed for this node.
	pub fn recurse<'a,'n,S,E,F,Y>(&'a self, f: F, name: &'n str, state: &S)
		->Result<Tree<T::To>,E>
	where X: 'a, Y: Debug, T: TreeRecurse<Y>,
		F: Fn(&'a X, &'n str, &S)->Result<(S,Y),E>
	{
		let (new_state, content) = f(&self.content, name, state)?;
		Ok(Tree { content, branches: self.branches.recurse(f, name, &new_state)? })
	}
	/// Same as `recurse`, but for a `FnMut`.
	pub fn recurse_mut<'a,'n,S,E,F,Y>(&'a self, mut f: F, name: &'n str, state: &S)
		->Result<Tree<T::To>,E>
	where X: 'a, Y: Debug, T: TreeRecurse<Y>,
		F: FnMut(&'a X, &'n str, &S)->Result<(S,Y),E>
	{
		let (new_state, content) = f(&self.content, name, state)?;
		Ok(Tree { content, branches: self.branches.recurse_mut(f, name, &new_state)? })
	}
	/// Apply a closure to each element of the tree; fallible version.
	pub fn try_map<'a,E,F,Y>(&'a self, f: F)->Result<Tree<T::To>,E>
	where X: 'a, Y: Debug, T: TreeRecurse<Y>, F: Fn(&'a X)->Result<Y,E> {
		self.recurse(|x, _, _| f(x).map(|x| ((),x)), "", &())
	}
	/// Apply a closure to each element of the tree; fallible version.
	pub fn try_map_mut<E,F,Y>(&self, mut f: F)->Result<Tree<T::To>,E>
	where Y: Debug, T: TreeRecurse<Y>, F: FnMut(&X)->Result<Y,E> {
		self.recurse_mut(|x, _, _| f(x).map(|x| ((),x)), "", &())
	}
	/// Apply a closure to each element of the tree; infallible version.
	pub fn map<F,Y>(&self, f: F)->Tree<T::To>
	where Y: Debug, T: TreeRecurse<Y>, F: Fn(&X)->Y {
		self.try_map(|x| infallible(f(x))).unwrap()
	}
	/// Apply a closure to each element of the tree; mut version
	pub fn map_mut<F,Y>(&self, mut f: F)->Tree<T::To>
	where Y: Debug, T: TreeRecurse<Y>, F: FnMut(&X)->Y {
		self.try_map_mut(|x| infallible(f(x))).unwrap()
	}
}
/// Impl of this trait is produced by macro
pub trait Forest {
	type In;
	fn by_name1<'a>(&'a self, target: &str)->Option<&'a Self::In>;
	fn by_name_mut1<'a>(&'a mut self, target: &str)->Option<&'a mut Self::In>;
	/// Runtime search in the tree.
	/// this would be a bit hard to do with `recurse` — the lifetimes are
	/// a mess, and we want to interrupt search as soon as we find *and*
	/// cut branches with a non-matching name:
	fn by_name<'a>(&'a self, target: &str)->Result<&'a Self::In> {
		self.by_name1(target).ok_or(Error::UnknownTable(target.into()).into())
	}
	/// Same, with mutable reference.
	fn by_name_mut<'a>(&'a mut self, target: &str)->Result<&'a mut Self::In> {
		self.by_name_mut1(target).ok_or(Error::UnknownTable(target.into()).into())
	}
}
/// Trait containing the basic recursion function for forests.
/// Implemented by the derive macro for forests deduced from resource
/// types.
pub trait TreeRecurse<Y>: Forest {
	type To: Forest<In=Y>;
	fn recurse<'a,'n,S,E,F>(&'a self, f: F, name: &'n str, state: &S)
		->Result<Self::To,E>
	where F: Fn(&'a Self::In, &'n str, &S)->Result<(S,Y),E>;
	fn recurse_mut<'a,'n,S,E,F>(&'a self, f: F, name: &'n str, state: &S)
		->Result<Self::To,E>
	where F: FnMut(&'a Self::In, &'n str, &S)->Result<(S,Y),E>;
	/// Apply a closure to each element of the tree; fallible version.
	fn try_map<'a,E,F>(&'a self, f: F)->Result<Self::To,E>
	where Y: 'a, F: Fn(&'a Self::In)->Result<Y,E> {
		self.recurse(|x, _, _| f(x).map(|x| ((),x)), "", &())
	}
	/// Apply a closure to each element of the tree; fallible version.
	fn try_map_mut<'a,E,F>(&self, mut f: F)->Result<Self::To,E>
	where Y: 'a, F: FnMut(&Self::In)->Result<Y,E> {
		self.recurse_mut(|x, _, _| f(x).map(|x| ((),x)), "", &())
	}
}
/// A helper type for building schemas for all in-game resource.
struct SchemaBuildState {
	level: usize,
	table_name: String,
	parent_name: &'static str,
	root: &'static str,
	table_type: crate::schemas::TableType,
}
impl SchemaBuildState {
	fn descend(state: Option<&Self>, ext: &'static str, name: &'static str)->Self {
		use crate::schemas::TableType;
		if let Some(parent) = state {
			Self {
				level: parent.level+1,
				table_name: format!("{}_{name}", parent.table_name),
				parent_name: name,
				root: parent.root,
				table_type: TableType::Sub {
					parent: parent.table_name.clone(),
					root: parent.root
				},
			}
		} else {
			Self {
				level: 0,
				table_name: name.to_owned(),
				parent_name: name,
				root: name,
				table_type: TableType::Top {
					extension: ext,
				},
			}
		}
	}
	fn schema(&self, fields: &Fields)->Schema {
		Schema {
			level: self.level,
			table_type: self.table_type.clone(),
			fields: *fields,
			name: self.table_name.clone(),
		}
	}
}
/// The definition of schemas for all in-game resources.
pub static ALL_SCHEMAS: Lazy<Tree<RootForest<Schema>>> = Lazy::new(|| {
	crate::restypes::Root::FIELDS_TREE.recurse(|row_data,name,state| {
		let new_state = SchemaBuildState::descend(state.as_ref(), row_data.ext, name);
		let schema = new_state.schema(&row_data.fields);
		infallible((Some(new_state), schema))
	}, "", &None).unwrap()
});

/// A trait containing resource I/O functions.
///
/// This is only available for top-level resources.
pub trait ResourceIO: ResourceTree {
	const EXTENSION: &'static str;
	const RESTYPE: Restype;
	// Required methods
	/// Loads a resource from filesystem.
	fn load(io: impl Read+Seek)->Result<Self>;
	/// Saves a resource to filesystem.
	fn save(&mut self, io: impl Write+Seek+Debug)->Result<()>;
	// Provided methods
}
impl<T: ResourceTree<Primary=Resref>+ResourceIO> crate::database::LoadResource for T {
	type InsertHandle<'db:'b,'b> = (&'db Connection, &'b mut Tree<T::StatementForest<'db>>);
	#[allow(single_use_lifetimes)] // clippy over-optimistic here
	fn load_and_insert<'db:'b,'b>((db, tree): Self::InsertHandle<'db,'b>, 
			mut handle: crate::gamefiles::ResHandle<'_>)->Result<()> {
		let resource = T::load(handle.open()?)?;
		resource.insert_as_topresource(db, tree, handle.resref)?;
		// TODO: mark resource as override
		// TODO: increase counter if possible
		Ok(())
	}

}

}
pub mod gamestrings {
//! Access to game strings in database.
//!
//! This is the only mod knowing the internals of [`GameString`] struct.
use crate::prelude::*;
use crate::toolbox::{Progress};
use crate::database::{DbInterface};
use crate::gamefiles::{GameIndex};
use crate::pack::{Pack,PackAll};
use crate::sql_rows::{SqlRow,FromSqlRow};
use macros::{Pack};

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
// #[column("primary key")]
// 	strref: NotPacked::<i32>,
	flags: u16,
	sound: Resref,
	volume: i32,
	pitch: i32,
#[sql(false)]
	delta: u32,
#[sql(false)]
	strlen: i32,
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

/// Fills the database table for a single language.
fn load_language(db: &impl DbInterface, langname: &str, path: &(impl AsRef<Path> + Debug))->Result<()> {
	let bytes = fs::read(path)
		.with_context(|| format!("cannot open strings file: {path:?}"))?;
	let mut q = db.prepare(GameString::FIELDS9.insert_sql(
		lazy_format!("load_strings_{langname}"), r#""strref","#, 1))?;
	let itr = GameStringsIterator::try_from(bytes.as_ref())?;
	let pb = Progress::new(itr.len(), langname);
	db.execute(r#"update "global" set "strref_count"=?"#, (itr.len(),))?;
	let mut n_strings = 0;
	for (strref, x) in itr.enumerate() {
		let s = x?;
		pb.inc(1);
		s.bind_execute1(&mut q, strref)?;
		n_strings+= 1;
	}
	info!("loaded {n_strings} strings for language \"{langname}\"");
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

/// Saves game strings in one language to the given file.
fn save_language(vec: &[GameString], path: &impl AsRef<Path>)->Result<()> {
	// FIXME: this could be done with a prepared statement and iterator
	// (saving the (rather big) memory for the whole vector of strings),
	// but rusqlite does not export `reset`...
	let path = path.as_ref();
	let mut file = fs::File::create(path)
		.with_context(|| format!("cannot create TLK file: {path:?}"))?;
	TlkHeader { lang: 0, nstr: vec.len() as u32,
		offset: (26*vec.len() + 18) as u32 }.pack(&mut file)?;
	vec.iter().pack_all(&mut file)?;
	for s in vec {
		file.write_all(s.string.as_bytes())
			.with_context(|| format!("cannot write strings to TLK file:{path:?}"))?;
	}
	Ok(())
}
/// For all game languages, saves database strings to "{lang}.tlk" in
/// current directory.
///
/// This also backups those files if needed (i.e. if no backup exists
/// yet).
pub fn save(db: &impl DbInterface, game: &GameIndex)->Result<()> {
	let pb = Progress::new(game.languages.len(), "save translations");
	for (lang, _) in &game.languages {
		pb.inc(1);
		let count = 1+db.query_row(&format!(r#"select max("strref") from "strings_{lang}""#),
			(), |row| row.get::<_,usize>(0))?;
		let mut vec = vec![GameString::default(); count];
		let mut delta = 0;
		let mut stmt = db.prepare(format!(r#"select "strref",{fields} from "strings_{lang}""#, fields = GameString::FIELDS9))?;
		for row in <(Strref,GameString)>::iter(&mut stmt, ())? {
			let (strref, mut gamestring) = row?;
	// first string in en.tlk has: (flags: u16=5, offset=0, strlen=9)
	// second string has (flags: u16=1, offset=9, strlen=63) etc.
	// (offset=72) etc.
			gamestring.delta = delta;
			delta+= gamestring.string.len() as u32;
			vec[strref.0 as usize] = gamestring;
		}
		let target = format!("{lang}.tlk");
		save_language(&vec, &target)
		.with_context(|| format!("could not save strings for language '{lang}'"))?;
		info!("updated strings in {target:?}; file now contains {count} entries");
	}
	Ok(())
}
} // mod gamestrings
pub mod database {
//! Access to the SQL side of the database.
use crate::prelude::*;
use crate::restypes::*;
use crate::gamefiles::GameIndex;
use crate::resources::{ALL_SCHEMAS,Tree,ResourceIO};
use crate::schemas::{Schema};

/// A trivial wrapper on [`rusqlite::Connection`];
/// mainly used for standardizing log messages.
#[derive(Debug)]
pub struct GameDB(Connection);
impl Deref for GameDB {
	type Target = Connection;
	fn deref(&self)->&Self::Target { &self.0 }
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
	/// [`crate::schemas::Schema::create_tables_and_views`] function.
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
	/// - `new_strings`: a view of all strrefs currently introduced by mods.
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
			ALL_SCHEMAS.try_map(|schema| {
				debug!("  creating tables for resource '{schema}'");
				schema.create_tables_and_views(|s| db.exec(s))
			}).context("cannot create main tables and views")?;
			ALL_SCHEMAS.create_new_strings(|s| db.exec(s))?;
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
			let mut tables = ALL_SCHEMAS.try_map(|schema|
				db.prepare(&schema.insert_sql("load_"))
					.with_context(|| format!("insert statement for table '{schema}'")))?;
			game.for_each(move |restype, handle| {
				trace!("found resource {}.{:#04x}", handle.resref, restype.0);
// 				base.register(&resref)
// 					.with_context(|| format!("could not register resref:{resref}"))?;
				#[allow(clippy::single_match)]
				match restype {
				Item::RESTYPE =>
					Item::load_and_insert((db, &mut tables.branches.items), handle)?,
				_ => (),
				};
				Ok(())
			})?;
			pb.inc(1);
			Ok(())
		})
	}
}

/// A newtype wrapper on [`rusqlite::Transaction`];
/// mainly used for standardizing log messages.
#[derive(Debug)]
pub struct GameTransaction<'a>(rusqlite::Transaction<'a>);
impl Deref for GameTransaction<'_> {
	type Target = Connection;
	fn deref(&self)->&Self::Target { self.0.deref() }
}

/// A trait for trivial wrappers of [`rusqlite::Connection`].
pub trait DbInterface {
	// Required methods
	/// Access to the actual database.
	fn db(&self)->&Connection;
	/// Executes a statement with arguments (and logging).
	fn execute(&self, s: impl AsRef<str>, params: impl rusqlite::Params+Debug)->Result<usize> {
		let s = s.as_ref();
		debug!("executing SQL statement: {s} with parameters {params:?}");
		self.db().execute(s, params)
			.with_context(|| format!("failed SQL statement:\n {s}"))
	}
	/// Prepares a statement (and logs it).
	fn prepare(&self, s: impl AsRef<str>)->Result<Statement<'_>> {
		let s = s.as_ref();
		debug!("preparing SQL statement: {s}");
		self.db().prepare(s)
			.with_context(|| format!("failed to prepare SQL statement:\n {s}"))
	}
	/// Same as `rusqlite::last_insert_rowid`.
	fn last_insert_rowid(&self)->i64 { self.db().last_insert_rowid() }
	// Provided low-level methods
	/// Executes a statement without arguments (but with logging).
	fn exec(&self, s: impl AsRef<str>)->Result<usize> { self.execute(s, ()) }
	/// Executes a batch of statements (with logging).
	fn batch(&self, s: impl AsRef<str>)->Result<()> {
		let s = s.as_ref();
		debug!("executing a batch of SQL statements: {s}");
		self.db().execute_batch(s)
			.with_context(|| format!("failed batched SQL statements:\n {s}"))
	}
	/// Prepares and executes a statement returning a single row.
	fn query_row<T>(&self, s: impl AsRef<str>, params: impl rusqlite::Params+Debug, f: impl FnOnce(&Row<'_>)->rusqlite::Result<T>)->Result<T> {
		self.prepare(s)?.query_row(params, f).map_err(|e| e.into())
	}
	/// Same as `rusqlite` function with same name,
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
		ALL_SCHEMAS.try_map(|schema| {
			let extension = match schema.table_type {
				crate::schemas::TableType::Top { extension, .. } => extension,
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
		ALL_SCHEMAS.try_map(|schema| {
			if matches!(schema.table_type, crate::schemas::TableType::Top { .. }) {
				self.exec(format!(r#"delete from "dirty_{schema}""#))?;
				self.exec(format!(r#"delete from "orphan_{schema}""#))?;
			};
			any_ok(())
		})?;
		any_ok(())
	}
}
impl<T: Deref<Target=Connection>>  DbInterface for T {
	fn db(&self)->&Connection { self.deref() }
}

impl Tree<RootForest<Schema>> {
	/// Builds the SQL statement creating the `new_strings` view.
	///
	/// This also builds a few triggers which mark resources as dirty
	/// whenever their strrefs are reassigned.
	pub fn create_new_strings<T>(&self, f: impl Fn(&str)->Result<T>)->Result<()> {
		let mut create = String::from(
r#"create view "new_strings"("native","strref","flags") as
select "a"."native", "strref", "flags" from ("#);
		let mut is_first = true;
		let mut trigger = String::from(r#"create trigger "update_strrefs"
after insert on "strref_dict"
begin"#);
		self.map_mut(|schema| {
			schema.append_new_strings_schema(&mut create, &mut is_first);
			schema.append_new_strings_trigger(&mut trigger);
		});
		write!(&mut create, r#") as "a"
	left join "strref_dict" as "b" on "a"."native" = "b"."native"
	where typeof("a"."native") = 'text'"#)?;
		write!(&mut trigger, "end")?;
		f(&create)?;
		f(&trigger)?;
		// This triggers makes any update of `strref` in `new_strings`
		// (with any dummy value; we use -1) produce the lowest still-available
		// strref; see
		// https://stackoverflow.com/questions/24587799/returning-the-lowest-integer-not-in-a-list-in-sql
		f(r#"create trigger "update_new_strings"
	instead of update of "strref" on "new_strings"
	begin
		insert into "strref_dict"("native","strref")
		values (new."native",
			ifnull((select min("strref")+1 from "strref_dict"
				where "strref"+1 not in (select "strref" from "strref_dict")),
				(select "strref_count" from "global")));
	end"#)?;
		Ok(())
	}
}

/// A trait describing how to insert a top-level resource into the
/// database.
pub trait LoadResource {
	type InsertHandle<'db:'b,'b>;
	/// Reads from a resource handle and writes to database.
	#[allow(single_use_lifetimes)] // clippy over-optimistic here
	fn load_and_insert<'db:'b,'b>(db_handle: Self::InsertHandle<'db,'b>,
		handle: crate::gamefiles::ResHandle<'_>)->Result<()>;
}

} // mod database
pub mod lua_api {
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
//!  - `simod.update(table, field, primary, value)`: updates a single field
//!    in the table.
//!  - `simod.insert(table, row, context)`: inserts a full row in the
//!    table. The columns are read from the merge of both tables `row`
//!    and `context`.
//!
//! Any error occurring during execution of one of those functions
//! (including SQL errors) is passed back to Lua in the form of a
//! callback error.
use mlua::{Lua,FromLua,ToLua,ExternalResult,Value,MultiValue};
use rusqlite::{ToSql, types::ToSqlOutput};

use std::collections::HashMap;

use crate::prelude::*;
use crate::resources::{ALL_SCHEMAS,Forest,TreeRecurse};
use crate::restypes::{RootForest};
use crate::schemas::{Schema,TableType};
use crate::sql_rows::{AsParams};
/// Small simplification for frequent use in callbacks.
macro_rules! fail {
	($($a:tt)+) => { return Err(Error::$($a)+.into()) }
}
fn mlua_ok<T>(a: T)->mlua::Result<T> { Ok::<_,mlua::Error>(a) }
#[ext]
impl<'lua> Value<'lua> {
	/// Conversion to SQL.
	/// We need this as (1) a tool for impl of [`ToLua`] for a newtype
	/// around [`Value`], and (2) to decouple the lifetime of the
	/// resulting `ToSqlOutput` from the incoming reference
	/// (since we are producing owned values here, we can use `'static`).
	///
	/// Note that this is almost costless: most values are integers anyway,
	/// and strings need copying from Lua to ensure UTF-8 validity.
	fn to_sql_owned(&self)->rusqlite::Result<ToSqlOutput<'static>> {
		use rusqlite::types::{ToSqlOutput::Owned,Value as SqlValue};
		let x =
		match self {
		Value::Nil => Ok(Owned(SqlValue::Null)),
		Value::Boolean(b) => Ok(Owned((*b).into())),
		Value::Integer(n) => Ok(Owned((*n).into())),
		Value::Number(x) => Ok(Owned((*x).into())),
		Value::String(ref s) =>
		// TODO: replace unwrap() by map_err()
			Ok(Owned(s.to_str().unwrap().to_owned().into())),
		// TODO: use proper error
		e => Err(rusqlite::Error::InvalidParameterName(format!("cannot convert {e:?} to a SQL type")))
		};
		x
	}
}
/// A newtype around [`mlua::Value`], allowing us to implement various
/// extra traits: [`rusqlite::ToSql`], custom [`Debug`] etc.
///
/// This would be more properly written with *two* lifetime parameters
/// `'a` and `'lua`, but we have no use for this right now, so for the
/// sake of simplicity we keep only the shortest lifetime `'a`.
pub struct LuaValueRef<'a>(&'a Value<'a>);
impl<'a> From<&'a Value<'a>> for LuaValueRef<'a> {
	fn from(source: &'a Value<'a>)->Self { Self(source) }
}
impl Debug for LuaValueRef<'_> {
	fn fmt(&self, f: &mut Formatter<'_>)->fmt::Result {
		match self.0 {
			Value::String(s) => f.write_str(&s.to_string_lossy()),
			x => write!(f, "{x:?}"),
		}
	}
}
impl ToSql for LuaValueRef<'_> {
	fn to_sql(&self)->rusqlite::Result<ToSqlOutput<'_>> { self.0.to_sql_owned() }
}
/// Value conversion in the Sql->Lua direction.
///
/// Note that this function needs access to the `[mlua::Lua]` instance so
/// that it may allocate strings.
fn sql_to_lua<'lua>(v: rusqlite::types::ValueRef<'_>, lua: &'lua Lua)->Result<Value<'lua>> {
	use rusqlite::types::{ValueRef::*};
	match v {
		Null => Ok(Value::Nil),
		Integer(n) => Ok(Value::Integer(n)),
		Real(x) => Ok(Value::Number(x)),
		Text(s) => Ok(Value::String(lua.create_string(&s)?)),
		_ => Err(rusqlite::types::FromSqlError::InvalidType.into()),
	}
}

/// Helper function for reading arguments passed to Lua callbacks.
///
/// Reads an argument as the given `FromLua` type and returns it,
/// wrapped in a `Result`.
fn pop_arg_as<'lua, T: FromLua<'lua>>(args: &mut MultiValue<'lua>, lua:&'lua Lua)->Result<T> {
	let arg0 = args.pop_front().ok_or(Error::CallbackMissingArgument)?;
	let r = T::from_lua(arg0, lua).with_context(||
		format!("cannot convert argument to type {}", std::any::type_name::<T>()))?;
	Ok(r)
}
/// Small simplification for [`MultiValue`] impl of [`AsParams`].
type MluaMultiIter<'a,'lua> = std::iter::Rev<std::slice::Iter<'a,Value<'lua>>>;
impl<'lua> AsParams for MultiValue<'lua> {
	type Elt<'a> = LuaValueRef<'a> where Self: 'a;
	type Iter<'a> = std::iter::Map<MluaMultiIter<'a,'lua>,fn(&'a Value<'lua>)->LuaValueRef<'a>> where Self: 'a;
	fn params_iter(&self)->Self::Iter<'_> {
		self.into_iter().map(LuaValueRef::from)
	}
}
/// A callback function, callable by Lua user code.
///
/// This traits is for structs wrapping some per-table data (e.g. some
/// prepared statements). This happens in two phases:
///  - `prepare` builds the struct from the schema for a table,
///  - when called from Lua, `execute` builds the returned Lua value.
/// The extension trait for `RootForest<Callback>` defined later takes
/// care of dispatching all callback invocations to the correct table.
pub trait Callback<'a>: Sized {
	/// Builds the data for the callback from the table schema.
	fn prepare(db: &'a impl DbInterface, schema: &'a Schema)->Result<Self>;
	/// Runs the callback (from the selected table, etc.) and builds the
	/// resulting Lua value.
	fn execute<'lua>(&mut self, lua: &'lua Lua, args: MultiValue<'lua>)->Result<Value<'lua>>;
	/// Utility function to check that arguments match statement.
	fn expect_arguments(args: &MultiValue<'_>, expected: usize)->Result<()> {
		// We assume that one argument was discarded (table name).
		let found = args.len() + 1;
		if found != expected {
			Err(Error::BadArgumentNumber { expected, found }.into())
		} else {
			Ok(())
		}
	}
}
/// Implementation of `simod.list`.
#[derive(Debug)]
struct ListKeys<'a>(Statement<'a>);
impl<'a> Callback<'a> for ListKeys<'a> {
	fn prepare(db: &'a impl DbInterface, schema: &'a Schema)->Result<Self> {
		let mut s = format!(r#"select "id" from "{schema}""#);
		if schema.is_subresource() {
			uwrite!(&mut s, r#" where "parent"=? order by "position""#);
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
	fn execute<'lua>(&mut self, lua: &'lua Lua, args: MultiValue<'lua>)->Result<Value<'lua>> {
		Self::expect_arguments(&args, self.0.parameter_count()+1)?;
		let mut rows = self.0.query(args.as_params())?;
		let ret = lua.create_table()?;
		while let Some(row) = rows.next()? {
			let v_sql = row.get_ref(0)?;
			let v_lua = sql_to_lua(v_sql, lua)?;
			ret.push(v_lua)?;
		}
		Ok(Value::Table(ret))
	}
}
/*
/// Implementation of the `simod.select` callback.
#[derive(Debug)]
struct SelectRow<'a>(Statement<'a>);
impl<'a> Callback<'a> for SelectRow<'a> {
	fn prepare(db: &'a impl DbInterface, schema: &'a Schema)->Result<Self> {
		let sql = format!(r#"select {cols} from "{schema}" where "id"=?"#,
			cols = schema.fields);
		db.prepare(sql).map(Self)
	}
	/// Implementation of `simod.list`.
	///
	/// This takes as parameters either
	/// 1. the name of a top-level resource table, or
	/// 2. the name of a sub-resource + a resref for the corresponding
	///    top-level resource,
	/// and returns in a Lua table the list of all primary keys matching
	/// this condition.
	fn execute<'lua>(&mut self, lua: &'lua Lua, args: MultiValue<'lua>)->Result<Value<'lua>> {
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
				Ok(Value::Table(ret))
			},
			None => Ok(Value::Nil),
		}
	}
}
*/
/*
/// Implementation of `simod.insert`.
#[derive(Debug)]
struct InsertRow<'a>(&'a Connection, Statement<'a>,&'a Schema);
impl<'a> Callback<'a> for InsertRow<'a> {
	fn prepare(db: &'a impl DbInterface, schema: &'a Schema)->Result<Self> {
		let s = schema.insert_sql(lazy_format!(""));
		// TODO: use a restricted form of insertion where primary is not
		// inserted
		Ok(Self(db.db(), db.prepare(s)?, schema))
	}
	fn execute<'lua>(&mut self, _lua: &'lua Lua, mut args: MultiValue<'lua>)->Result<Value<'lua>> {
		Self::expect_arguments(&args, 2)?;
		let table = match args.pop_front().unwrap() {
			Value::Table(t) => t,
			other => fail!(BadArgumentType { position: 2, expected: "table",
				found: format!("{other:?}")}),
		};
		let Self(db, stmt, schema) = self;
		// We could use two strategies here:
		// 1. build an iterator from schema.fields, then map to SQL values,
		//    then pass to [`rusqlite::params_from_iter`],
		// 2. use a loop and raw bind to the statement.
		// Since [`rusqlite::params_from_iter`] does not accept `Result`
		// values, taking option 2 will produce better failure messages.
		let offset: usize;
		let expected = stmt.parameter_count();
		let mut bind_field = |i, name| {
			let v_lua: Value<'_> = table.get(name)?;
			// Note that sqlite uses 1-based indexing.
			stmt.raw_bind_parameter(i+1, v_lua.to_sql_owned()?)?;
			any_ok(())
		};
		// first bind the header fields:
		if schema.is_subresource() {
			offset = 2;
			bind_field(0, "parent")?;
			bind_field(1, "position")?;
		} else {
			offset = 1;
			bind_field(0, "id")?;
		}
		let found = schema.fields.len() + offset;
		if found != expected {
			fail!(BadParameterCount { expected, found })
		}
		for (i, field) in schema.fields.iter().enumerate() {
			bind_field(i + offset, field.fname)?;
		}
		stmt.raw_execute()?;
		if schema.is_subresource() {
			table.set("id", db.last_insert_rowid())?;
		}
		Ok(Value::Table(table))
	}
}
*/
/*
/// Implementation of `simod.update`.
///
/// We collect all the per-field update statements in a hash map.
/// While this might lead to preparing useless statements in the case of
/// a small mod, this should save time for large mods on average,
/// and using prepared statements is safer anyway.
/// (Besides, the memory storage req for a prepared statement is small).
struct UpdateRow<'a>(HashMap<&'a str, Statement<'a>>);
impl<'a> Callback<'a> for UpdateRow<'a> {
	fn prepare(db: &'a impl DbInterface, schema: &'a Schema)->Result<Self> {
		let mut h = HashMap::<&str, Statement<'_>>::
			with_capacity(schema.fields.len());
		for field in schema.fields.iter() {
			h.insert(field.fname, db.prepare(&format!(
				r#"update "{schema}" set {field}=?2 where "id"=?1"#))?);
		}
		Ok(Self(h))
	}
	fn execute<'lua>(&mut self, lua: &'lua Lua, mut args: MultiValue<'lua>)->Result<Value<'lua>> {
		Self::expect_arguments(&args, 4)?;
		let arg0 = args.pop_front().unwrap(); // take ownership of first argument
		let fieldname = match arg0 {
			Value::String(ref s) => s.to_str()?,
			other => fail!(BadArgumentType { position: 2, expected: "string",
				found: format!("{other:?}")}),
		};
		let stmt = match self.0.get_mut(fieldname) {
			Some(s) => s,
			_ => fail!(UnknownField { field: fieldname.into(), }),
		};
		// Return the number of changed rows:
		let n = stmt.execute((LuaValueRef(args.get(0).unwrap()),
			LuaValueRef(args.get(1).unwrap())))?;
		Ok((n > 0).to_lua(lua)?)
	}
}
*/
/*
/// Implementation of `simod.delete`.
struct DeleteRow<'a>(Statement<'a>);
impl<'a> Callback<'a> for DeleteRow<'a> {
	fn prepare(db: &'a impl DbInterface, schema: &'a Schema)->Result<Self> {
		db.prepare(&format!(r#"delete from "{schema}" where "id"=?"#))
			.map(Self)
	}
	fn execute<'lua>(&mut self, lua: &'lua Lua, args: MultiValue<'lua>)->Result<Value<'lua>> {
		Self::expect_arguments(&args, self.0.parameter_count()+1)?;
		let n = self.0.execute(args.as_params())?;
		Ok((n > 0).to_lua(lua)?)
	}
}
/// This extension trait allows factoring the code for selecting the
/// appropriate table for a Lua callback.
/// Once this is done, the individual [`Callback`] instance for this
/// table is run.
#[ext]
*/
impl<'a, T: Callback<'a> + Debug+'a + Sized> RootForest<T> {
	fn prepare(db: &'a impl DbInterface)->Result<Self> {
		ALL_SCHEMAS.branches.try_map(|s| T::prepare(db, s))
	}
	/// Selects the appropriate individual callback from the first argument
	/// (table name) and runs it.
	fn execute<'lua>(&mut self, lua: &'lua Lua, mut args: MultiValue<'lua>)->Result<Value<'lua>> {
		let table = pop_arg_as::<String>(&mut args, lua)
			.context("first argument must be a string")?;
		self.by_name_mut(&table)?
			.execute(lua, args)
			.with_context(|| format!(r#"executing callback on table "{table}""#))
	}
	/// The wrapper installing the callback function in a table.
	fn install_callback<'scope>(&'scope mut self, scope: &mlua::Scope<'_,'scope>,
		table: &mlua::Table<'scope>, name: &'scope str)->mlua::Result<()> {
// **NOTE**:
// The lifetime parameters were determined after **a lot** of trial
// and error....
// So this code is **precious** and we keep it for future reference.
// 	fn callback<'scope,'closure>(&'scope mut self, scope: &'closure mlua::Scope<'_,'scope>)->mlua::Result<mlua::Function<'closure>> {
// 		scope.create_function_mut(|lua, args|
// 			self.prepare(lua, args).to_lua_err())
// 	}
		table.set(name, scope.create_function_mut(move |lua, args|
			self.execute(lua, args)
				.with_context(|| format!(r#"In callback "{name}":"#)).to_lua_err())?)
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
	list_keys: RootForest<ListKeys<'a>>,
// 	select_row: RootForest<SelectRow<'a>>,
// 	insert_row: RootForest<InsertRow<'a>>,
// 	update_row: RootForest<InsertRow<'a>>,
// 	delete_row: RootForest<InsertRow<'a>>,
}
impl<'a> LuaStatements<'a> {
	pub fn new(db: &'a impl DbInterface)->Result<Self> {
		Ok(Self {
// 			schemas,
			list_keys: RootForest::<_>::prepare(db)?,
// 			select_row: RootForest::<_>::prepare(db, schemas)?,
// 			insert_row: RootForest::<_>::prepare(db, schemas)?,
// 			update_row: RootForest::<_>::prepare(db, schemas)?,
// 			delete_row: RootForest::<_>::prepare(db, schemas)?,
		})
	}
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
	use crate::sql_rows::RowExt;
	let lua = Lua::new();
	let lua_file = Path::new("/home/jerome/src/infinity_compiler/init.lua");
	let mut statements = LuaStatements::new(&db)?;
	// We need to wrap the rusqlite calls in a Lua scope to preserve  the
	// lifetimes of the references therein:
	lua.scope(|scope| {
		let simod = lua.create_table()?;
		let lua_schema = lua.create_table()?;
		ALL_SCHEMAS.branches.try_map_mut(|schema| {
			let fields = lua.create_table()?;
// 			let context = lua.create_table()?;
			for f in schema.fields.iter() {
				fields.set(f.fname, f.ftype.description())?;
			}
// 			for col in schema.context() {
// 				context.push(col.fname)?;
// 			}
			let res_schema = lua.create_table()?;
			res_schema.set("fields", fields)?;
// 			res_schema.set("context", context)?;
// 			res_schema.set("primary", schema.primary().fname)?;
			lua_schema.set(schema.to_string(), res_schema)?;
			mlua_ok(())
		})?;
		// Step 2: build the sub-schema relations
		// We do this in a second pass since by now all resource tables exist
		ALL_SCHEMAS.try_map_mut(|schema| {
			if let TableType::Sub {  .. } = schema.table_type {
				todo!()
			}
			mlua_ok(())
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
			mlua_ok(())
		})?)?;
		statements.list_keys.install_callback(scope, &simod, "list")?;
// 		statements.select_row.install_callback(scope, &simod, "select")?;
// 		statements.insert_row.install_callback(scope, &simod, "insert")?;
// 		statements.update_row.install_callback(scope, &simod, "update")?;
// 		statements.delete_row.install_callback(scope, &simod, "delete")?;
		lua.globals().set("simod", simod)?;

		info!("loading file {lua_file:?}");
		lua.load(lua_file).exec()?;
		Ok(())
	})?;
	Ok(())
}
} // mod lua_api

pub(crate) mod restypes;

use crate::prelude::*;
use clap::Parser;

use gamefiles::{GameIndex};
use toolbox::{Progress};
use crate::restypes::*;
use crate::resources::{ALL_SCHEMAS};

fn type_of<T>(_:&T)->&'static str { std::any::type_name::<T>() }

/// Saves all modified game strings and resources to current directory.
///
/// This function saves in the current directory;
/// a chdir to the appropriate override directory needs to have been done
/// first.
fn save_resources(db: &impl DbInterface, game: &GameIndex)->Result<()> {
	let pb = Progress::new(2, "save all"); pb.as_ref().tick();
	gamestrings::save(db, game)?;
	pb.inc(1);
	let mut tables = ALL_SCHEMAS.try_map(|schema|
		db.prepare(schema.select_dirty_sql())
	)?;
// 	let mut tables = SCHEMAS().map(|schema| db.prepare(format!(
// 	r#"select "id", {cols} from "save_{schema}" where "parent"=? sort by "position""#,
// 		cols=schema.fields)))?;
// 	Item::save_all_dirty(db, &mut tables, "save_items")?;
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
#[allow(unused_variables)]
fn command_show(db: &GameDB, target: impl AsRef<str>)->Result<()> {
	let target = target.as_ref();
	let (resref, resext) = match target.rfind('.') {
		None => return Ok(()),
		Some(i) => (&target[..i], &target[i+1..]),
	};
	match resext.to_lowercase().as_ref() {
		"itm" => todo!(), 
		x => { warn!("unknown resource type: {x}"); Ok(()) },
	}
}
/// Restores all backed up files.
fn command_restore(game: &GameIndex)->Result<()> {
	game.restore(&game.backup_dir)
		.with_context(|| format!("failed to restore game files from {:?}",
			game.backup_dir))
}

mod arguments {
//! A tiny wrapper for command-line arguments.
//! Mainly serves to prevent useless warnings from
//! `unused_qualifications`.
	#![allow(unused_qualifications)]
use crate::prelude::*;
/// The runtime options parser for `clap` crate.
#[derive(clap::Parser,Debug)]
#[command(version,about=r#"Exposes Infinity Engine game data as a SQLite database."#)]
pub struct RuntimeOptions {
	#[arg(short='G',long,help="sets the game directory (containing chitin.key)", default_value=".")]
	pub gamedir: PathBuf,
	#[arg(short='F',long,help="sets the sqlite file")]
	pub database: Option<PathBuf>,
	#[arg(short='O',long,help="sets log output",default_value="simod.log")]
	pub log_output: String,
	#[arg(short='L',long,default_value_t=0,help="sets log level")]
	pub log_level: i32,
	#[command(subcommand)]
	pub command: Command,
}
/// Subcommands passed to the executable.
#[derive(clap::Subcommand,Debug)]
#[allow(unused_qualifications)]
pub enum Command {
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
}
use arguments::*;


fn main() -> Result<()> {
// 	use crate::resources::{ALL_SCHEMAS,ResourceTree};
// 	println!("{:?}", *ALL_SCHEMAS);
// 	ALL_SCHEMAS9.recurse(|x,_n,_state| {
// 		x.describe(); infallible(nothing2)
// 	}, "", &())?;
// 	if 1 > 0 { return Ok(nothing) }
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
			None => { ALL_SCHEMAS.map(|schema| println!("{}", schema.name)); },
			Some(s) => {
				ALL_SCHEMAS.by_name(&s)?.describe(); },
		},
		_ => todo!(),
	};
	info!("execution terminated with flying colors");
	Ok(())
}
