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
 - `load_X` is the table of all original resources;
 - `add_X` is the table of all mod-inserted resources;
 - `edit_X` is the table of all mod changes on this resource;
 - `save_X` is the view used for saving game resources.

> In general, mods should only interact with the main view `X`.
> The structure of all other views listed here is **unstable**.

The columns of the table `X` are the following:
 - the primary key is always called `"id"`; for the top-level resources,
   it is the resource identifier, while for sub-resources this is a
   numeric key;
 - for sub-resources only, a column called `"parent"`, which refers to
   the primary key of the parent resource, followed by a column called
   `"position"`, which is used as a sort key for collecting
   sub-resources;
 - then all “payload” fields as described in e.g. IESDP. All the fields
   describing sub-resources (offset, count etc.) are removed from this
   list, since sub-resources are described in their own tables.


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

The `load_X` table always contains exactly the resources found in
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

## TODO

Special cases:
 - dialog,
 - IDs,
 - 2da,
 - script (de)compiling,
