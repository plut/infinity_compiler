# Resource identifiers and namespacing

In-game resources are identified by “resrefs”, which are 8-byte ASCII
strings.
These resrefs are also used as file names in the override directory,
which puts a number of additional constraints on allowed characters:
namely, the characters `"+<>/\|?*:` are forbidden.

Inside the `simod` database however, resources are identified by
arbitrary strings (hereafter designated as “longrefs”).

## Namespacing

To protect from resource conflicts, mod components generally do not
access them globally.

To each mod component is attached a *namespace*, in the form of a string.
Whenever a mod accesses a resource identifier `R`
it is translated to a longref in the following way,
assuming that `N` is the current namespace.

1. if the name `R` does not contain a slash character, then
   the namespace `N` is prepended: the longref is thus `"N/R"`;
2. if `R` contains a slash, then it is assumed to be a fully-qualified
   longref.
3. as a special case, resources from the base game are accessible
   using the empty namespace, e.g. as `"/sw1h01"`.

These rules allow referring to either a base game resource (case 3)
or a resource from another mod (case 2), while still providing namespace
separation by default (case 1).

### Namespacing and storage

When stored inside the database,
longrefs are stored as fully-qualified strings
(in the form `"namespace/identifier"`).
Original game resrefs are stored in their original form
as 8-byte strings.


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
## Conversion to game format

The resource identifier strings are converted to the resref format
when the database is saved to the filesystem.

More precisely, the `resref_dict` table contains a dictionary
between string identifiers and resrefs.
This table is filled by triggers: whenever a game value
pointing to a resource is modified,
the values `("long_resource_name", null)` are inserted.
As part of the `save` operation,
The compiler takes care of replacing all `null` fields by
game-unique resref as needed.

(These resrefs are deduced from the long resource name by truncating and
enumerating).

### Internal representation of resrefs from the base game

Internally, resrefs from the base game are imported untouched as
longrefs (no slash character is prepended; rule 3. above actually removes
the slash character).
This means that longrefs without a slash always designate base resources,
while longrefs with a slash always designate mod-owned resources.
