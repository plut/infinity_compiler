window.SIDEBAR_ITEMS = {"fn":[["command_add","Runs the lua script for adding a mod component to the database."],["delete","Implementation of `simod.delete`."],["pop_arg_as","Helper function for reading arguments passed to Lua callbacks."],["sql_to_lua","Value conversion in the Sql->Lua direction."],["update","Implementation of `simod.update`."]],"macro":[["fail","Small simplification for frequent use in callbacks."]],"struct":[["InsertRow","Implementation of `simod.insert`."],["ListKeys","Implementation of `simod.list`."],["LuaStatements","A collection of all the prepared SQL statements used for implementing the Lua API."],["LuaToSql","Runtime errors during interface between SQL and Lua. A simple wrapper allowing `Value` to be converted to SQL."],["LuaValueRef","A newtype around [`&mlua::Value`], allowing us to implement various extra traits: [`rusqlite::ToSql`], custom [`Debug`] etc."],["SelectRow","Implementation of the `simod.select` callback."]],"trait":[["AllResourcesTExt","This extension trait allows factoring the code for selecting the appropriate table for a Lua callback. Once this is done, the individual [`Callback`] instance for this table is run."],["Callback",""],["ValueluaExt",""]],"type":[["MluaMultiIter","Small simplification for [`mlua::MultiValue`] impl of [`AsParams`]."]]};