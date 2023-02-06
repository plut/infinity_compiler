# `simod`: Structured IE mod tool

A mod tool for Infinity Engine games
using SQL as an intermediate representation.

This tool exposes [Infinity Engine's files](https://gibberlings3.github.io/iesdp/file_formats/index.htm)
as a [SQLite database](https://www.sqlite.org/index.html)
and gives access to a number of functions for modifying this database.

## Workflow

 - `simod init` initializes the database and loads all data from the IE
   files (key/bif and existing override files).
 - `simod add` *target* installs a mod or mod component,
   by running a script (written by the mod author) which edits the database.
 - `simod save` compiles all changes in the database back to the
   `override` directory.

See `simod --help` for (slightly) more details.

## Why a new modding tool?

This should offer the following advantages over WeiDU mods,
either in terms of robustness or in ease-of-use for mod authors.

### Replacing the mod stack by a proper database

The database offers at any given time a coherent view of all
currently-installed mods. Uninstalling a single mod can be done by
running a somewhat simple (set of) SQL `DELETE` statement(s).
In particular, this is very fast (quasi-constant time w.r.t the number of
mods installed) compared to WeiDU's stack model (when modifying a mod
deep down the stack requires recomputing the whole stack, which has O(n)
cost).

### Easier conflict detection between mods

With whole access to the database it becomes trivial to detect when two
mods are trying to access the same resource.

(This is still **TODO** however; mostly, we need to fix an interface
about what to do in the case of conflict).

### Namespacing

Identifiers for game resources and strings are abstracted as strings
and namespaced per mod component.
This completely removes the need for using mod prefixes
and fitting names in 8 bytes (minus the prefix).
(On the other hand, the namespace model still allow access to original
game resources, and even resources from other mods, when this is really
needed).

Moreover, this also circumvents
a number of “bad behaviours” by mod authors,
such as fully overwriting a game file
or using inconsistent case for file names.

### Translations

This tool uses [`.po`
files](https://www.gnu.org/software/gettext/manual/html_node/PO-Files.html)
for string translations.
This format is easy to edit; a number of free and open source tools
exist, and even a plain text editor will work in most cases.
This format has also proven to be quite robust e.g. when strings evolve
between versions of software.
(This is in contrast with WeiDU's `.tra` files, which are very brittle:
a single missing translation when a mod is updating will crash the
component).

**TODO:**
the translation manager also contains a number of features
making it easy to annotate syntactically ambiguous sentences
(e.g. “Guard” may be either a verb or a noun in English;
both cases have different translations in most languages).

### Mod writing in SQL or Lua

This tool offers two levels of API for accessing the database.
The first level is plain SQL given by the description of the database;
for instance, `UPDATE "items" SET "enchantment"=5 WHERE
"itemref"='sw1h34'` is a perfectly valid mod and will update Albruin's
enchantment.

However, it is expected that most of mods will use
the higher level represented as [Lua](https://www.lua.org/) scripts.
Indeed, this tools offer the option to run a Lua script in an environment
where a simplified API to the database is exposed.
For example, the following code has the same effect as the SQL statement
above:
```lua
albruin = simod.item("sw1h34")
albruin.enchantment = 5
```

Lua is a easy-to-use programming language
(and definitely easier to handle for a beginner than WeiDU);
moreover, it is already the language used in some parts of the games
themselves.

The SQL interface also allows authors to write mods in any language
containing a library for SQL access.

### Mod manager

**TODO**: define a common API from the Lua side for describing a mod +
metadata (author, description, compatibility list)
and write on the an interactive mod selection tool which uses this mod
database.

### Portability and robustness

The tool is mostly written in Rust, which takes great pains to be as
portable as possible; and mod scripts written in Lua should be portable
by construction.

In particular, it is a design goal to prevent mod authors from needing to
run shell scripts or batch files (which is a nightmare from a maintenance
POV).

### Performance

Any single mod installation only accesses the SQLite database;
thus all work is deferred to SQLite, which is quite fast.
Access to the game files is done only once when compiling the full mod
database to the override directory.
Moreover, this tool supports differential compilation:
only those files which did change since the previous compilation
will be regenerated.

## Current status

**Proof-of-concept**.
This tool is not very capable (yet).
It can only access game items for now, and do some very basic editing on
them.

However, a significant part of basic infrastruture has been implemented:
 - resource loading from key/bif/override,
 - database structure,
 - Lua script loading (and API for database access),
 - strref and resref translation,
 - resource and string saving.
Rough roadmap for next features to implement includes:
 - differential or atomic compilation,
 - translation generator from Lua source code,
 - binary resource handling,
 - dialog access,
 - script (de)compiler,
 - other resource types,
 - mod manager

## Database structure

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

## Usage

### Creating the database

TODO

### Compiling back to override

TODO

## Internals

### Resource view
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

### Translating resources

In-game resources are referred by “resrefs”, which are 8-byte ASCII
strings.

#### Resource identifiers for mods

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

#### Translation to game format

These strings are then converted to the resref format by the compiler.

More precisely, the `resref_dict` table contains a dictionary between
mod-facing (long) resource names and game-facing resrefs.
This table is filled by triggers: when a new resource field is inserted,
the values `("long_resource_name", null)` are inserted.
The compiler takes care of replacing all `null` fields by
game-unique resref as needed.

(These resrefs are deduced from the long resource name by truncating and
enumerating).


### Translating strings

In-game strings are collated in one or two files, `dialog.tlk` and
`dialogF.tlk`, and refrred to by 4-byte integers in game structures.

New strings inserted by mods are referred to as text.
The process is similar to that of resource names.

#### Strings translations

TODO: decide whether `.po` or SQL is the best way to store this.

## TODO

Special cases:
 - dialog,
 - IDs,
 - 2da,
 - script (de)compiling,
