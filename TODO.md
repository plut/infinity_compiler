
# Game strings

have a special view listing all Strref introduced by mods
i.e. `new_strref`
  => computed from `edit_foo` and `add_foo`
  => over all values of `foo` in resources and all Strref fields

they are *non-unique* (only unique per component)

pb:
  mod1 inserts "string1" strref=34001
  mod2 inserts "string2" strref=34002

  then mod1 is deleted; status of strref_dict is probably
  "string2" 34002
  mod3 inserts "string3", should grab smallest available strref

0. `new_strings` is a computed view of all strref introduced by mods
   (live).
compilation process is the following:
1. purge stale strings from `strref_dict`:
    delete from strref_dict where {key ∉ new_strings}
2. insert all new strrefs:...
    select key from new_strings where key not in (select key from
		strref_dict)
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
 - fill `strref_dict` table
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
