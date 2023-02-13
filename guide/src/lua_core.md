# Core functions in the LUA API


The `simod` library contains the lowest-level API exported to Lua mod
scripts. The remainder of the API is built on top of these functions.

Most of the functions in this library take, as their first parameter,
a string `table` containing the name of one of the game's resource tables
(`"items"`, `"item_abilities"` etc.).
String matching is case-sensitive.

## `simod.insert`: insertin a new game object

`simod.insert(table, row)`

`row` is a table containing the row to be inserted, as key-value
pairs; the keys are strings matching the column headers for this table
(with the same case).

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

## `simod.select(table, primary)`: read one row in a table

This returns the content of the row with primary key `primary`
in the table, as a Lua table whose keys are strings
corresponding to the table's column names.

If no row with the given primary key exists, then an error is thrown.

## `simod.update(table, fieldname, primary, value)`: modify an entry in a table.

This updates the row with primary key `primary` in the given table,
setting the field with column name `fieldname` to the given `value`.
The `fieldname` must be a string corresponding to one of the table's
column names (otherwise an error is thrown).

This returns a boolean value, which is `true` if a row was updated
and `false` if no row with the given primary key was found.

## `simod.delete(table, primary)`: delete an entry in a table.

This deletes the row with the given primary key from the table.

This returns a boolean value, which is `true` if a row was deleted,
and `false` otherwise.

Note that rows from the base game (those present in the `load_*` tables)
currently cannot be deleted,
since the data they represent is stored in the BIF files
and not in the override directory.
