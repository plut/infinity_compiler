 - fill `strref_dict` table
  - move GameString to gametypes and ensure correct `Table` implem.
	  (we already have a customized unpack)
	- but we need to not include it in the global resource table!
	 => a special field in attributes to skip it
 + simplify interaction between Table and ResourceInsert (etc.)
 - for binaries: have a special variant of Resref which does interact with
	 the db in a different way
 - try compiling a few items
 + fill `resref_dict` table before compiling
 + inserting a Resref or a Strref populate the `*_dict` tables.
 + error propagation
 + try to make namedrows a true Iterator
 - move compile views to sql-side?
  pros: eases debugging
  cons: possibly slightly less efficient (i.e. not filtering...)?
	  need to add dirty bit as an extra column
 - expand `current` to add file+line reference to Lua code
 - see if need to split bitfields
