# Internals: Database structure

**ALL STRINGS ARE UTF-8**. No exceptions; we live in the 21st century.


The exposed interface for accessing game objects is through a number of
views:

 - `items`, `item_abilities`, `item_effects` for game items,
 - (other views are **TODO**: we are building high rather than wide for
   now).

These views implement all the infrastructure necessary for inserting and
updating game objects; in case more detail is needed, see the
“Internals” section below.

This implies that game modding can be performed directly as SQL queries
on a small number of tables with structure mirroring that of game files.
An ad-hoc library is also being built to make this comfortable for mod
authors (TODO).

## Resource view
For each resource `X`:

 - `X` is the user-facing view of all resources (original and modded).
	 This is the
 - `res_X` is the table of all original resources;
 - `add_X` is the table of all mod-inserted resources;
 - `edit_X` is the table of all mod changes on this resource;

For top-level resources, a few additional tables are used to mark their
status in the database with respect to the override directory:
 - `dirty_X` is the table listing all resources which have been modified
   and which need to be recompiled to `override`;
 - `orphan_X` is the list of all resources which have been removed from
   the database, but not yet from the `override` directory.

A (large) number of triggers are attached to the main view `X`:
 - attempts at modifying `X` are propagated back to the appropriate table
	 (either `add_X` or `edit_X`);
 - at the same time, modifying `X` records the resource as dirty in
	 `dirty_X`;
 - deleting entries from `X` marks resources as orphan in `orphan_X`.
The `res_X` table always contains exactly the resources found in
`key/bif` and pre-existing override files.
This table is never touched again after it is built by `simod init`.

## Game lists (`IDS` files)

TODO.

## Game tables (`2DA` files)

TODO.

## Scripts

TODO.

## Binary resources

**TODO:**

Binary resources generally do not need concurrent access between various
mods and are not handled by the database.
Where a resref pointing to a binary resource is expected,
the database uses instead a string referring to a file in the filesystem.

When the database is saved to the filesystem,
the filename for these resources is translated to a resref
using the general algorithm;
this resref in turn gives the name of the override file
under which the resource will be saved.

## Resources namespacing and translation

In-game resources are identified by “resrefs”, which are 8-byte ASCII
strings.
These resrefs are also used as file names in the override directory,
which puts a number of additional constraints on allowed characters:
namely, the characters `"+<>/\|?*:` are forbidden.

### Namespacing

To each mod component is attached a *namespace*, in the form of a string.
The first stage of translation is the mapping of resource names (as
expressed by the mod) to global resource names. This is done in the
following way: when a mod with namespace `N` accesses resource `R`,
1. if the name `R` does not contain a slash character, then
   the namespace `N` is prepended: the full resource name is thus
	 `"N/R"`;
2. if `R` contains a slash, then it is assumed to be a fully-qualified
   resource identifier.
3. as a special case, resources from the base game are accessible
   using the empty namespace, e.g. as `"/sw1h01"`.

These rules allow referring to either a base game resource (case 1)
or a resource from another mod (case 3), while still providing namespace
separation by default (case 2).

### Implicit resref access

**TODO:** the Lua interface has the following feature:
whenever a structure field is of the “resref” type,
it is possible to assign a full structure to this field;
this will result in the assignment being made
with the reference to this structure instead.

For instance,
```lua
small_sword = simod.item("/sw1h01");
store.inventory:push(small_sword); -- only pushes the resref "/sw1h01"
```

### Translation to game format

These strings are then converted to the resref format when the database
is saved to the filesystem.

More precisely, the `resref_dict` table contains a dictionary between
mod-facing (long) resource names and game-facing resrefs.
This table is filled by triggers: when a new resource field is inserted,
the values `("long_resource_name", null)` are inserted.
The compiler takes care of replacing all `null` fields by
game-unique resref as needed.

(These resrefs are deduced from the long resource name by truncating and
enumerating).

The `resref_dict` table is persistent.

## Translating strings

In-game strings are collected in one or two files, `dialog.tlk` and
(depending on user language) `dialogF.tlk`,
and referred to by 4-byte integers (“strrefs”) in game structures.
In the database, the female form `dialogF.tlk`, where it exists, is
handled as a (almost) completely separate language from the male form.
Languages are represented by their 5-letter name (as in `"en_US"`);
female variants have a 6-letter name (as in `"fr_FRF"`).

Since game strings are not used as references, namespacing rules do not
apply here; the only rule is that strings from different namespaces will
never be merged. The way to set a game string (e.g. to set the name of an
item) is to just use the _native string_ itself,
i.e. usually the string as written in the mod's native language.

### Strings translations

String translation is handled through the use of the `translations_X`
table, where `X` is the 5- or 6- letter name of one of the game
languages. This table contains data similar to the game's `.tlk` file,
except that it is indexed by native strings instead of strrefs.

Any native string absent from this dictionary is left untranslated.
This is intended as a sane default value where the player,
when a string has not (yet) been translated, will see the string in the
original language, which is most often English.

### Conversion to strref

Conversion between native strings and strref is performed iby the
`strref_dict` table in a way similar to the `resref_dict` table.
The algorithm for generating new strrefs is of course different (the
lowest available value is used).

## TODO

Special cases:
 - dialog,
 - IDs,
 - 2da,
 - script (de)compiling,
