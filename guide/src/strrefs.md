# String references

In-game strings are collected in one or two files, `dialog.tlk` and
(depending on user language) `dialogF.tlk`,
and referred to by 4-byte integers (“strrefs”) in game structures.

> In the database, strings are identified by **“native strings”**,
> which are arbitrary strings.
> (The native string is usually the game string itself
> in the mod's native language).

### Namespacing
Since game strings are not used as references,
namespacing rules do not apply for native strings.
The only rule is that strings from different namespaces will never be merged.

## Conversion to strref

The `strref_dict` table is the dictionary
between native strings and strrefs.

The `new_strings` view is a list of all the native strings
introduced by currently installed mods,
together with their game flags (**TODO** explain why the flags).

As a part of the `save` operation,
the compiler rebuilds `strref_dict`
from the list of strings present in `new_strings`.
This procedure happens in two steps:
1. entries absent from `new_strings`
   and entries with a too-high `strref` value
   are purged from the dictionary,
2. then entries form `new_strings` are inserted in `strref_dict`,
   each one successively using the lowest available strref.

For step 1:
Let \\(C\\) be the number of constant strrefs
(e.g. \\(C=34000\\) for a BG1EE install)
and \\(S\\) be the number of entries in `new_strings`.
Then the new resrefs need to be allocated in the interval
\\([0,C+S-1]\\).
This means that entries where \\(\mathtt{strref} \geq C+S \\)
need to be purged from the `strref_dict` table.



The `string_keys` view is used for sequential generation of strrefs.

