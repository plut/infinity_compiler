 - a command-line option to make it headless (disable progress bars)
# Lua side
 - [ ] make every field access lazy (this is probably simplest way)
 - [ ] decide when conversion from native to strref (etc.1
## Primitive operations
 - [x] `update`
 - [x] `select`
 - [x] `list`
 - [x] `insert`
 - [x] `delete`
## Argument checking
 - [ ] `current`, `namespace` handling
 - [ ] strref namespacing
 - [ ] arguments to function calls
 - [ ] values for filling db
 - [ ] from lua or rust?
 - [ ] type-check (based on schema) for updates
 - [ ] e.g. Strref accepts int or string,
 - [ ] pass `current` when executing Lua code
 - [ ] expand `current` to add file+line reference to Lua code
 - [ ] see if need to split bitfields
## Strings
 - [ ] decide how to convert `.po` to sql
	- the {M} and {F} markers work in this conversion (sql treats all
		languages as separate)
	- lua to `.po` using Python seems to work
	- for simplicity, running lua file should insert everything needed in sql
 - [ ] also strings should be marked with component to be easily uninstallable
 - [ ] so lua init should do the translation; i.e. we assume that the
		strings are already inserted in standardized translation tables
  => do just like with game resources: expose a view with strings as keys
  and a set of triggers to manipulate this
	- default values (untranslated) are best left to global select instead
		of 20 triggers...
## Optimization
 - [ ] (optional) call `luac` on init.lua to save a bit of loading time
# Higher level
 - [ ] each mod component is stored in a normalized table containing
   metadata: name, categorization, description, code (function or string),
   compatibility
# Other functions
 - add a few functions:
  - [ ] `translate`: lua to .po
   - [ ] write a minimal Lua lexer to identify all `_(str)` calls
   - [ ] just like we did in Julia, have `_(str)` put stuff in a normalized
     table
  - [ ] adding a mod reads the .po strings
 - [ ] show: add a flag to keep original resref and a language selector
 - [ ] clarify views:
 - [ ] human-readable vs. compiled
 - [ ] a global option to limit languages
 - [ ] for binaries: have a special variant of Resref which does interact with
	 the db in a different way
# Rust resources
	 itertools? paste? bitflags
	 lexopt/clap/pico-args for argument parsing
	 termcolor
	https://github.com/brson/rust-anthology/blob/master/master-list.md
	https://github.com/mre/idiomatic-rust
	cargo watch -s 'clear; cargo check --tests --color=always 2>&1 | head -40'

vim: et:
