-- #! /usr/bin/env -S ./infinity_compiler -G /home/jerome/jeux/bg/game -O add.log -L 5 add
-- interface is as follows:
-- simod.allkeys
-- simod.select
-- simod.insert
-- simod.update
-- simod.dump
local function strdump(o)
	if type(o) == 'table' then
		local s = '{ '
		local i = 1
		for k,v in pairs(o) do
			if k == i then
				i = i+1
			elseif type(k) == 'string' then
				s = s..'\x1b[32m'..k..'\x1b[m='
			else
				s = s..'[\x1b[32m'..k..'\x1b[m]='
			end
			s = s.. strdump(v)..', '
		end
		return s .. '} '
	elseif type(o) == 'string' then
		return '\x1b[34m"'..o..'"\x1b[m'
	else
		return tostring(o)
	end
end

local resource_mt = {}
function resource_mt:__newindex(key, val)
	if self.fields[key] == nil then
		error("unknown key for"..self.table..": '"..key.."'")
	end
	self.fields[key] = val
	simod.update(self.table, self.key, key, val)
end
function resource_mt:__index(key)
	return self.fields[key]
end
function resource_mt:__call(key, values)
	-- clones the resource, modifying the following vlaues
	print("resource "..self.key.." clone to "..key.." with changes: ",
		strdump(values))
end

local family_mt = {}
local function get_item(name)
	local fields = simod.select("items", name)
	return setmetatable({ fields = fields, key = ame, table = "items" }, resource_mt)
end

local ablist = simod.list("item_effects", "sw1h01")
print(strdump(ablist))
local ab = simod.select("item_effects", 5736)
print(strdump(ab))
-- row = simod.select("items", "sw1h34")
-- print(strdump(row))
-- albruin = get_item("sw1h34")
-- print(strdump(albruin))
-- albruin.enchantment=1
-- print(strdump(albruin))
-- print(albruin.kit2)
-- albruin("big sword", { name = "big sword" })
