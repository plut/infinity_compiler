# `simod`: Structured IE mod tool


[![Documentation|Dev](https://img.shields.io/badge/docs-latest-blue.svg)](https://plut.github.io/infinity_compiler/)

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

