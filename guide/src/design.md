# Why a new modding tool?

This should offer the following advantages over WeiDU mods,
either in terms of robustness or in ease-of-use for mod authors.

## Replacing the mod stack by a proper database

The database offers at any given time a coherent view of all
currently-installed mods. Uninstalling a single mod can be done by
running a somewhat simple (set of) SQL `DELETE` statement(s).
In particular, this is very fast (quasi-constant time w.r.t the number of
mods installed) compared to WeiDU's stack model (when modifying a mod
deep down the stack requires recomputing the whole stack, which has O(n)
cost).

## Easier conflict detection between mods

With whole access to the database it becomes trivial to detect when two
mods are trying to access the same resource.

(This is still **TODO** however; mostly, we need to fix an interface
about what to do in the case of conflict).

## Namespacing

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

## Translations

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

## Mod writing in SQL or Lua

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

## Mod manager

**TODO**: define a common API from the Lua side for describing a mod +
metadata (author, description, compatibility list)
and write on the an interactive mod selection tool which uses this mod
database.

## Portability and robustness

The tool is mostly written in Rust, which takes great pains to be as
portable as possible; and mod scripts written in Lua should be portable
by construction.

In particular, it is a design goal to prevent mod authors from needing to
run shell scripts or batch files (which is a nightmare from a maintenance
POV).

## Performance

Any single mod installation only accesses the SQLite database;
thus all work is deferred to SQLite, which is quite fast.
Access to the game files is done only once when compiling the full mod
database to the override directory.
Moreover, this tool supports differential compilation:
only those files which did change since the previous compilation
will be regenerated.

