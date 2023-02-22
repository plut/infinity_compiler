function loadstring(s) return load(s) end
--[[ todo
 fat Lua objects
 what is needed: x:save()
 and newindex etc. wrappers as well!
]]--
--««1 Display tools
local function strdump(o, level, limit)
	if level == nil then level = 0 end
  if type(o) == 'table' then
		local self_dump = rawget(o, "__dump")
		if type(self_dump) == "string" and level >= 1 then return self_dump end
    local s = '{ '
    local i = 1
		local n = 0
    for k,v in pairs(o) do
			n = n+1
			if type(limit) == type(0) and n > limit then
				s = s..'…'
				break
			end
      if k == i then
        i = i+1
      elseif type(k) == 'string' then
        s = s..'[38;5;65m'..k..'[m='
      else
        s = s..'[[38;5;65m'..k..'[m]='
      end
			if v == o then
				s = s .. "[31m!self![m,"
			else
				s = s.. strdump(v, level+1, limit)..', '
			end
    end
    return s .. '} '
  elseif type(o) == 'string' then
    return '[38;5;25m"'..o..'"[m'
  else
    return tostring(o)
  end
end

local function dump(o, limit)
	print(strdump(o, limit))
end
local function evaldump(s, limit)
	print("[35m"..s.."[m:", strdump(loadstring("return "..s)(), limit))
end
d = evaldump
local function count(tbl)
	local l = 0
	for _, _ in pairs(tbl) do l = l+1 end
	return l
end
local function deep_copy(tbl)
	-- ⚠ WARNING, this naive implementation does not support cycles!!
	local res = { }
	for k, v in pairs(tbl) do
		if type(v) == "table" then
			res[k] = deep_copy(v)
		else
			res[k] = v
		end
	end
	return res
end

local function mkfn(name)
	return function(...)
	  print("[33;1m"..name.."[m called with: "..strdump(arg))
	end
end
local function mkdisplay(prefix)
	return function(...)
		local s = ""
		for i, v in ipairs({...}) do
			s = s .. tostring(v)
		end
		return "["..prefix.."m"..s.."[m"
	end
end
local red = mkdisplay("31")
local green = mkdisplay("32")
local yellow = mkdisplay("33")
local blue = mkdisplay("34")
local magenta = mkdisplay("35")
local cyan = mkdisplay("36")
local bold = mkdisplay("1")
local function todo(text) error("todo!: "..tostring(text)) end
--««1 Schema accessors
local function table_schema(tbl)
	local sch = simod.schema[tbl]
	if sch == nil then
		error("unknown table: "..tostring(tbl))
	end
	return sch
end

--««1 Methods for resources
local resource_mt = {}
resource_mt.__index = resource_mt -- let derived classes inherit from this
function resource_mt:derive(table)
	-- builds a derived class from `resource_mt`
	local meta = { _table = table }
	meta.__index = meta -- this is a class
	-- in other words: member:method() will look in the metatable
	setmetatable(meta, self)
	return meta
end
function resource_mt.load_rec(table, key)
	-- loads a resource from row `key` of table `table`, recursively with
	-- all subresources.
	local new = simod.select(table, key)
	for fn, ft in pairs(simod.schema[table].fields) do
		if ft == "subresource" then
			local subtable = table..'_'..fn
			local list = simod.list(subtable, key)
			local subresource = {}
			for k, v in pairs(list) do
				subresource[k] = resource_mt.load_rec(subtable, v)
			end
			new[fn] = subresource
		end
	end
	return setmetatable(new, simod.schema[table].methods)
end
function resource_mt:save()
	local methods = getmetatable(self)
	local table = methods._table
	local schema = simod.schema[table]
	local fields = schema.fields
	print(red("saving to table ", table))
	local id = self.id
	local done = false
	if id ~= nil then
		local old = simod.select(table, id)
		if old ~= nil then -- an existing row was found for this id
			for fn, val in pairs(old) do
				local new_val = self[fn]
				if val ~= new_val then
					simod.update(table, fn, id, new_val)
				end
			end
			done = true
		end
	else -- id not attributed yet: generate a primary key
		local id_tmp = { 95 } -- '_' character
		-- TODO: generate a proper base-64 (or something) string
		for i = 2,8 do
			id_tmp[i] = math.random(97, 122)
		end
		id = string.char(table.unpack(id_tmp))
		self.id = id
		print("generated id: ", self.id)
	end
	for k, v in pairs(simod) do
		print(k, v)
	end
	if not done then
		simod.insert(table, self)
	end
	for fn, ft in pairs(fields) do
		if ft == "subresource" then
			local list = self[fn]
			for i, sub in ipairs(list) do
				sub.position = i
				sub:save()
			end
		end
	end
end

function resource_mt:clone_resource(args)
	-- Clones a resource, modifying values passed as arguments.
	print(yellow("cloning a resource: ", strdump(self)))
	print("  with modifiers: ", strdump(args))
	local sch = table_schema(self._table)
	local fields = sch.fields
	local dk = sch.default_key
	if dk == nil then
		error("only resources with a defined default_key can be cloned")
	end
	print("for table "..self._table.." dk is ", dk)
	local id, changes = normalize_changes(args, dk)
	
	local values = simod.select(self._table, self._key)
	for k,v in pairs(changes) do
		if values[k] == nil then
			error('field "'..k..'" is absent in target')
		end
		values[k] = v
	end
	-- TODO: also clone subresources (recursively)
	-- TODO: if there are any vector data for subresource fields, use it
	-- TODO: insert this into table
	return setmetatable({ _table = self._table, _key = id }, getmetatable(self))
end

--««1 Methods for resource vectors (resvecs)
-- Resource vectors have the following form:
-- { _table = "items_abilities", _parent = "sw1h34" }

local resvec_mt = {}
resvec_mt.__dump = "<resvec_mt>"
function resvec_mt:create_resvec_mt(mt)
	-- we pass the (already existing) metatable for the subresource
	local new = { each = mt }
	new.__dump = "[38;5;100m<resvec_mt "..mt._table..">[m"
	new.__index = self.index
	new.__newindex = self.newindex
	new.__call = self.iterate
	return setmetatable(new, self)
end

--««1 Methods for resource builders
-- Resource builders (E.g. "item") have the following form:
-- { _table = "items" } -- and they catch the appropriate metatable
local function normalize_changes(changes, dk)
	-- when passed a table of changes from a resource template,
	-- together with the table's default key name,,
	-- normalizes the changes (modifying the input table)
	--
	-- This assumes that dk is not nil.
	--
	-- This returns a pair:
	-- (new row id, modified table)
		-- in all cases we return (new id, new changes):
	if type(changes) == "string" then -- sword("carsomyr")
		return changes, { [dk] = changes }
	end
	local changes1 = changes[1]
	if changes1 ~= nil then
		changes[1] = nil -- this key is now useless, erase it
		-- sword{ "carsomyr", ... } sets name to "carsomyr"
		if changes[dk] == nil then changes[dk] = changes1 end
		return changes1, changes
	end
	local changes_dk = changes[dk]
	if changes_dk ~= nil then
		-- sword { name = "carsomyr", ... }
		return changes_dk, changes
	end
	error("no id given for new resource")
end
function resource_mt:create_resource(key)
	-- Creates a resource from a database row.
	-- this gets invoked as: resourcetype("row")
	--  => getmetatable(resourcetype).__call(resourcetype, "row")
	--  => resource_mt.__call(resourcetype, "row")
	-- so this must be `resource_mt.__call`
	print(green("initializing a value from "..strdump(key)))
	-- TODO: check that the key exists
	local new = { _table = self._table, _key = key }
	return setmetatable(new, self)
end

--««1 Updating `simod.schema` and creating all the metatables
-- compute all metatables for resources:
for table, schema in pairs(simod.schema) do
	schema.methods = resource_mt:derive(table)
end

--««1 Test code
local function group(x) print(red(bold("\ntesting "..x))) end
function test_core()
-- 	group("simod.list")
-- 		print(blue("listing a few item IDs:"))
		local v = simod.list("items")
-- 		print(v[1], v[2], v[3], v[4])
		assert(v[1] == "abazring")
		assert(v[2] == "abisred1")
		assert(v[3] == "acidbl")
-- 	local obj = simod.select("items", "sw1h34")
-- 	dump(obj)
-- 		print(blue("listing abilities from sw1h34:"))
		local v2 = simod.list("items_abilities", "sw1h34")
		assert(v2[1] == 808 and v2[2] == 809)
-- 		dump(v2)
-- 		print(blue("listing effects of ability ", v2[2]))
		local v3 = simod.list("items_abilities_effects", v2[2])
		assert(v3[1] == 1879 and v3[2] == 1880 and v3[3] == 1881)
-- 		dump(v3)
-- 	group("simod.select")
-- 		print(blue("selecting item sw1h34:"))
		local x = simod.select("items", "sw1h34")
		assert(x.description_icon == "csw1h34" and x.min_intelligence == 0
			and x.stack_amount == 1 and x.price == 10000)
-- 		dump(x)
-- 	group("simod.get")
		local x = simod.get("items", "name", "sw1h34")
		assert(x == 31707)
-- 		dump(x)
-- 	group("simod.set")
		local x = simod.set("items", "name", "sw1h34", "strange sword!")
-- 		dump(x)
-- 		print(blue("now sw1h34 name is:"))
		local x = simod.get("items", "name", "sw1h34")
		assert(x == "strange sword!")
-- 		dump(x)
-- 	group("simod.schema is:")
-- 		dump(simod.schema)
end
function test_objects0()
	-- show schema
	for k, v in pairs(simod.schema) do
		print(k, v.primary, strdump(v.context))
	end
	-- test item cloning
	sword = item("sw1h34")
	sword.weight = 18
-- 	d"sword"
	assert(sword.weight == 18)
-- 	d"sword.abilities[1].use_icon"
	sword.abilities[1].use_icon="spwi101b"
	carsomyr1 = sword("crasomyr")
	ab = carsomyr1.abilities[1]
	print(bold(red("computing effect now:")))
	debug=true
	ef = ab.effects[0]
	print(bold(red("computing effect done!")))
	dump(ef._context)
	if true then return end
-- 	d"crasomyr1"
	assert(carsomyr1.itemref == "crasomyr")
	assert(carsomyr1.name == "crasomyr")
	assert(carsomyr1.abilities[0]._context.itemref == carsomyr1.itemref)
	carsomyr2 = sword{"crasomyr", weight=200, itemref="cars2"}
	print("cocou")
	assert(carsomyr2.itemref == "cars2")
	assert(carsomyr2.name == "crasomyr")
	assert(carsomyr2.weight == 200)
	assert(carsomyr2.abilities[0]._context.itemref == carsomyr2.itemref)
	print("delete!")
	carsomyr2:delete()
	print("after delete")
end
function test_resources()
-- 	group("resource builder")
end
function test_inherit()
	function all_resources_mt:foo()
		print("called all_resources_mt:foo() on:", strdump(self))
	end
	function all_resources_mt.items:foo()
		print("called item:foo() on:", strdump(self))
	end
	local albruin = setmetatable({_table="items", _key="sw1h34"}, all_resources_mt.items)
	albruin:foo()
end
albruin = resource_mt.load_rec("items", "sw1h34")
print(albruin.id)
albruin.id = "no such id"
albruin:save()
-- simod.dump("select * from edit_items_abilities")
-- simod.dump("select * from edit_items")
