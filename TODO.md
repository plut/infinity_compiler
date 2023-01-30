# Global
 + the program is named `simod`
 - embed lua into rust (and give sql access through rusqlite)
 + accept subcommands: (PARTLY DONE)
    simod select <file.lua> <component>
    simod select <file.lua> # opens a menu
    simod select # opens a menu
    simod generate
    simod compile
    simod restore # restores backup files
# Lua side:
 - each mod component is stored in a normalized table containing
   metadata: name, categorization, description, code (function or string),
   compatibility
 - type-check (based on schema) for updates
  - e.g. Strref accepts int or string,
 - implement sql stuff
 - sub-resources: accessing this field returns a sub-resource vector
  - pushing on the sub-resource vector writes sql
   - we probably need a method here: item.abilities:push(...)
  - schema for this field must include both the type (constructor) and
    the sql table name
 `mlua::Lua::load(Path)` should work
# Rust side:
 - move all `*_item` etc. functions to `gametypes`
  - define a `ToplevelResource` trait for this...
 - show: add a flag to keep original resref and a language selector
 - we can probably replace TypedStatement by some trait StructuredRead<T>
   (or even better, include this in Table trait)
   (or just in `as_table` function)
 + replace `write_columns` by a `display` method for `Schema`
 - clarify views:
  - human-readable vs. compiled
  + include the views in database file
  + this will **heavily** simplify the rust select query
  + with extra fields for (*untranslated*) parent and dirty bit
 - a global option to limit languages
 + fill language strings tables
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
 - for binaries: have a special variant of Resref which does interact with
	 the db in a different way
 - expand `current` to add file+line reference to Lua code
 - see if need to split bitfields
# Rust resources
	 itertools? paste? bitflags
	 lexopt/clap/pico-args for argument parsing
	 termcolor
	https://github.com/brson/rust-anthology/blob/master/master-list.md
	https://github.com/mre/idiomatic-rust
	cargo watch -s 'clear; cargo check --tests --color=always 2>&1 | head -40'

vim: et:
