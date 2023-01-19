 - inserting a Resref or a Strref populate the `*_dict` tables.
 + error propagation
 + try to make namedrows a true Iterator
 - move compile views to sql-side?
  pros: eases debugging
  cons: possibly slightly less efficient (i.e. not filtering...)?
	  need to add dirty bit as an extra column
 - expand `current` to add file+line reference to Lua code
 + add a dump method for sqlite rows: debug self.columns()
  + see how rusqlite accesses column names
	+ everything is private, doh
 - try compiling a few items
 + reorg into modules:
 gameindex => everything belonging to BIF side: Pack, KeyHdr etc.
 interface:
  + GameIndex: new, for_each
	+ Resref, Strref
	+ Pack
 database => everything belonging to SQLite side: Row
  + Row
 gametypes => non-(Bif/Key) game types
  + Item etc.
	+ imports Resref, Strref from gameindex
  + called with `use gametypes::*`

 + use rusqlite's ToSql trait
 + and make affinity depend only on FieldType value
 + the SELECT views probably belong to the SQL side (for compatibility
	 with INSERT statements)
	 + and there might also be a structure holding all resources
 - add a new column to current: "all_dirty" = false
 + `primary_key` method on Schema
 + replace this stupid &[Column] by &str for create_statement
 + use a named tuple for global resources; macro from:
https://users.rust-lang.org/t/anon-struct-or-named-tuple-fields/20979
