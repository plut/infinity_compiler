
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
 - a global option to limit languages
 + fill `strref_dict` table
 + use default value ("" for Resref, 0 for strref)
 - fill language strings tables
  - decide how to convert `.po` to sql
	- the {M} and {F} markers work in this conversion (sql treats all
		languages as separate)
	- lua to `.po` using Python seems to work
	- for simplicity, running lua file should insert everything needed in sql
  - also strings should be marked with component to be easily uninstallable
  - so lua init should do the translation; i.e. we assume that the
		strings are already inserted in standardized translation tables
  => do just like with game resources: expose a view with strings as keys
  and a set of triggers to manipulate this
	- default values (untranslated) are best left to global select instead
		of 20 triggers...
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
