# Internals: Database structure

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
 - `dirty_X` is the table listing all resources which have been modified
	 and which need to be recompiled to `override`.

A (large) number of triggers are attached to the main view `X`:
 - attempts at modifying `X` are propagated back to the appropriate table
	 (either `add_X` or `edit_X`);
 - at the same time, modifying `X` records the resource as dirty in
	 `dirty_X`.
The `res_X` table always contains exactly the resources found in
`key/bif` files and is never touched again after it is built.

## Translating resources

In-game resources are referred by “resrefs”, which are 8-byte ASCII
strings.

### Resource identifiers for mods

New resources inserted by mods are referred by arbitrary strings instead.
To prevent conflict between various mods, resource names are also
namespaced. More precisely, when mod `M` refers to resource `"xxx"`:
1. if `"xxx"` exists in the base game then the (untranslated) reference
	 is used;
2. if `"xxx"` does not contain a slash ("/") character then the resource
	 reference is `"M/xxx"`;
3. if `"xxx"` contains a slash then it is also used untranslated.

These rules allow referring to either a base game resource (case 1)
or a resource from another mod (case 3), while still providing namespace
separation by default (case 2).

### Translation to game format

These strings are then converted to the resref format by the compiler.

More precisely, the `resref_dict` table contains a dictionary between
mod-facing (long) resource names and game-facing resrefs.
This table is filled by triggers: when a new resource field is inserted,
the values `("long_resource_name", null)` are inserted.
The compiler takes care of replacing all `null` fields by
game-unique resref as needed.

(These resrefs are deduced from the long resource name by truncating and
enumerating).


## Translating strings

In-game strings are collated in one or two files, `dialog.tlk` and
`dialogF.tlk`, and refrred to by 4-byte integers in game structures.

New strings inserted by mods are referred to as text.
The process is similar to that of resource names.

### Strings translations

TODO: decide whether `.po` or SQL is the best way to store this.

## TODO

Special cases:
 - dialog,
 - IDs,
 - 2da,
 - script (de)compiling,
