
# Game strings
3. ... with a trigger that picks the *smallest* available strref:
    create view string_keys as select "key" from strref_dict;
    create trigger "strref_auto" instead of insert on "string_keys"
    begin
      insert into "strref_dict" ("key", "strref") values
      (new."key", (select min("strref")+1 from "strref_dict" where
			"strref"+1 not in (select "strref" from "strref_dict")));
		end;

 **modify the above code so that selection is done from `orig_stref` ∪
 new strref**
# Todo:
 + fill `strref_dict` table
 - use default value ("" for Resref, 0 for strref)
 - fill language strings tables
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
