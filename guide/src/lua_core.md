# Core functions in the LUA API


The `simod` library contains the lowest-level API exported to Lua mod
scripts. The remainder of the API is built on top of these functions.

Most of the functions in this library take, as their first parameter,
a string `table` containing the name of one of the game's resource tables
(`"items"`, `"item_abilities"` etc.).
String matching is case-sensitive.

## `simod.list(table, [parent])`: list primary keys in a table

If `table` describes a top-level resource (e.g. `"items"`) then
the `[parent]` value is not allowed.
This will return an array containing the list of all primary keys
appearing in this table.

If `table` describes a sub-resource (e.g. `"item_abilities"`) then
`[parent]` is a primary key for the table's parent;
this will return a list of all sub-resources whose parent attribute
matches this primary key.

If no rows match the query, then an empty table is returned.

## `simod.insert(table, row)`: insert a new game object

`row` is a table containing the row to be inserted, as key-value
pairs; the keys are strings matching the column headers for this table
(with the same case).

This returns the number of lines inserted.
If the row does not match the format for this table then an error is
thrown.

Since subresources use automatically-generated row IDs as their primary
keys,
if the `table` describes a subresource then any existing `row.id` value
is ignored when this function is called; when the function returns,
the field will be (over)written with the rowid of the newly inserted row.

## `simod.select(table, primary)`: read one row in a table

Returns the content of the row with primary key `primary`
in the table, as a Lua table whose keys are strings
corresponding to the table's column names.

If no row with the given primary key exists, then `nil` is returned.

## `simod.update(table, fieldname, primary, value)`: modify an entry in a table.

Updates the row with primary key `primary` in the given table,
setting the field with column name `fieldname` to the given `value`.
The `fieldname` must be a string corresponding to one of the table's
column names (otherwise an error is thrown).

This returns a boolean value, which is `true` if a row was updated
and `false` if no row with the given primary key was found.

## `simod.delete(table, primary)`: delete an entry in a table.

Deletes the row with the given primary key from the table.

This returns a boolean value, which is `true` if a row was deleted,
and `false` otherwise.

Note that rows from the base game (those present in the `load_*` tables)
currently cannot be deleted,
since the data they represent is stored in the BIF files
and not in the override directory.

## `simod.schema`

This contains the description for the format of the game resource tables.
For example, the entry `simod.schema.items` contains the description of
items, as the following fields:

 - `simod.schema.items.fields`: list of fields and types, as key-pair values;
 - `simod.schema.items.is_subresource`: `nil` since this is not a
   subresource (otherwise `true`);

The type of a given field is indicated as a string; it is one of
`"integer"`, `"text"`, `"strref"`, `"resref"`, or `"subresource"`.
The `"subresource"` value is special: it indicates that the field's value
in a concrete resource will be a vector of values of the subresource
type.
The subresource will use the schema built as `name_subresource`;
for example, since `simod.schema.items.abilities = "subresource"`,
for each item, say `ring04`, `ring04.abilities` is a vector
whose elements follow the `simod.schema.items_abilities` schema.
