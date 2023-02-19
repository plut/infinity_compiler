# Representation of objects for the Lua API


## Resources

Resources are stored as a table `{_table, _key}`,
where `_table` is a string containing the name of the SQL table
to which the resource is attached,
and `_key` contains the primary key for the row mapped to the resource.

Access to the contents of the resource via `resource.field`
is overloaded to a function which returns
the value currently found in a database.
Thus, the value of `resource.field` will remain up-to-date
even if a SQL operation modified the contents of the database.


## Resource vectors

Resource vectors (e.g. the list of abilities of an item)
are stored as a table `{_table, _parent}`,
where `_table` is a string containing the name of the SQL table
containing this kind of resource (e.g. `"items_abilities"`)
and `_parent` is the primary key of the parent resource.

Accessing the `i`-th entry of this resource vector
triggers a SQL request which retrieves the corresponding
primary key.
