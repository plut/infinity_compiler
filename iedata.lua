-- howto:
-- 1. download luarocks, install binary to ~/usr/bin or somewhere
-- 2. luarocks config lua_version=5.3
-- 3. luarocks ---local install lsqlite3
--[[ Desired interface (user pov)
I. Main
a. mods should *never* have to contain location of game.sqlite — this is passed
either through a config/env. or as a default filesystem location.
b. we *probably* want to import a number of names to _G:
   Item, Dialog etc.
c. components are top-level functions + special metadata (e.g. component name)
   with some api allowing listing / picking components etc.
   

II. Resources
a. Item("sw1h") returns an Item object
b. Item { ... } defines an item
c. Item is the metatable for all Item objects
d. overrides are set for reading fields: item.weight
e. overrides are set for setting fields: item.weight = 5

ac: meta(Item).__call(string) grabs a row from SQL
bc: meta(Item).__call(table) creates a new line (and inserts it into SQL)
d: Item.__index is routed to SQL (with schema!)
e: Item.__newindex is routed to SQL

Then a factory for building resources:
Item = Resource { schema... }

]]--
--««1 Generalities
local function strdump(o)
	if type(o) == 'table' then
		local s = '{ '
		for k,v in pairs(o) do
			if type(k) ~= 'number' then k = '"'..k..'"' end
			s = s .. '['..k..'] = ' .. strdump(v) .. ','
		end
		return s .. '} '
	else
		return tostring(o)
	end
end

local function dump(n)
	print('\x1b[34m'..n..'\x1b[m = '..strdump(load("return "..n)()))
end
function table.contains(table, element)
  for _, value in pairs(table) do
    if value == element then
      return true
    end
  end
  return false
end
local Resource = { }
function Resource:__index(key)
	print("reading field: "..key)
	return self.values[key]
end
function Resource:__newindex(key, value)
	print("setting field: "..key.."="..value)
end
function Resource:__call(args)
	print("building a resource from schema :"..strdump(self.schema))
	print(" and args: "..args)
	-- set values from args!
	local values = { "set from args..." }
	local row = { values = value }
	setmetatable(row, self)
	return row
end
function Resource:build(schema)
	-- generates a new `Resource` type with given schema
	local restype = { schema = schema,
	}
	setmetatable(restype, self)
	return restype
end
setmetatable(Resource, { __call = Resource.build })

return {
	dump = dump,
	Resource = Resource,
}
