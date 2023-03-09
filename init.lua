function loadstring(s) return load(s) end
--[[ todo
 fat Lua objects
 what is needed: x:save()
 and newindex etc. wrappers as well!
]]--
--««1 Display tools
local function strdump(o, limit, level)
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
				s = s.. strdump(v, limit, level+1)..', '
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
local function todo(text) error("\x1b[1mtodo!\x1b[m: "..tostring(text)) end
--««1 Logging
local log = {}
log.ign = {}
function log:ignore(...)
	for i,v in ipairs({...}) do
		self.ign[v] = true
	end
end
log:ignore(strdump,dump,bold,red,green,yellow,blue,magenta,cyan)
log.level = 0
function log:print(...)
	if self.level > 10 then
		error("deep recursion")
	end
	for i = 1, self.level do
		io.write("│")
	end
	io.write(...)
	io.write("\n")
end
function log:isenabled(frame)
-- 	return frame.what == "Lua"
	return frame.what == "Lua" and frame.name ~= nil and self.ign[frame.func] == nil
end
function log:report(event)
	if event == "call" then
		local frame = debug.getinfo(3)
		if self:isenabled(frame) then
			self.level = 0
			local stack = ""
			for i=4,100 do
				if i > 99 then error("deep recursion") end
				local f = debug.getinfo(i)
				if f == nil then break end
				if self:isenabled(f) then
					self.level = self.level + 1
					stack = stack.."/"..i.." "..f.namewhat.." "..tostring(f.name)
				end
			end
			local caller = debug.getinfo(4)
			self:print("┌─▶"..frame.namewhat.." "..bold(yellow(frame.name).." "
				..cyan(frame.short_src..":"..frame.linedefined))
				.." " ..blue("from "..caller.currentline))
			self.level = self.level + 1
			self:print(stack)
-- 			self:print(strdump(frame))
-- 			self:print(strdump(caller))
		end
	elseif event == "return" then
		local frame = debug.getinfo(3)
-- 		for i=1,8 do
-- 			local f = debug.getinfo(i)
-- 			if f ~= nil then
-- 				self:print(i, "  ", f.namewhat, " ", tostring(f.name), ": ", f.currentline)
-- 			end
-- 		end
		if self:isenabled(frame) then
			self.level = self.level - 1
			self:print("└─ "..frame.namewhat.." "..frame.name)
		end
	end
end
local log_mt = {}
function log_mt:__call(...)
	self:print(...)
end
setmetatable(log, log_mt)
log:ignore(log.print,log.isenabled,log.report,log_mt.__call)
--««1 Schema accessors
local function table_schema(tbl)
	local sch = simod.schema[tbl]
	if sch == nil then
		error("unknown table: "..tostring(tbl))
	end
	return sch
end
--««1 Builder from schema
function simod.validate(fn, ft, value)
	-- validates the provided `value` against the type `ft`.
	-- fieldname `fn` is provided for context
	-- Return value : `value` if success, otherwise throws an error
	local t = type(value)
	if ft == "text" then
		if value == nil then value = ""
		elseif t ~= "string" then
			error("field '"..fn.."' expects a string, got "..totring(value))
		end
	elseif ft == "integer" then
		-- Rust API will translate nil to 0 for us
	elseif ft == "resref" then
		-- Rust API will translate nil to "" for us
	elseif ft == "strref" then
		-- TODO: here is the place where we check that gettext was used
		if value == nil then value = 0 -- default strref: <NO TEXT>
		end
	elseif ft == "subresource" then
		if value == nil then value = {}
		else
			todo("set subresource field from table value")
		end
	else
		error("unknown type in schema! "..tostring(ft))
	end
	return value
end
function simod.validate_all(schema, values)
	-- check all entries in a table against the provided schema
	for fn, ft in pairs(schema) do
		local v = values[fn]
	end
end
function simod.template(fields)
	-- build an empty template from a schema
	local values = {}
	for fn, ft in pairs(fields) do
		if ft == "text" then
			values[fn] = ""
		elseif ft == "integer" then
			values[fn] = 0
		elseif ft == "strref" then
			values[fn] = 0 -- default strref: <NO TEXT>
		elseif ft == "resref" then
			values[fn] = ""
		elseif ft == "subresource" then
			values[fn] = {} -- TODO
		else
			error("unknown type in schema! "..tostring(ft))
		end
	end
	return values
end
function simod.build_resource(fields, values)
	local resource = simod.template(fields)
	for k, v in pairs(values) do
		resource[k] = simod.validate(k, fields[k], v)
	end
	-- TODO: check for unitialized entries
	return resource
end
--««1 Methods for resources
local function copy_replace(orig, repl, del)
	-- builds a deep copy of `orig`, replacing by values from `repl` each
	-- time one of those is not `nil`.
	-- This propagates recursively along sub-tables:
	-- if `orig[i]` and `repl[i]` are tables then
	-- `copy[i]` is set to the table `orig[i]` replaced by `repl[i]`.
	--
	-- Keys present (at any level) in the `del` table are deleted.
	--
	-- * This does not handle cyclic dependencies.
	-- * Only those keys present in `orig` will be present in `copy`. (In
	-- other words, the table structure is preserved).
	-- * This transfers metatables (from `orig` only).
	if type(orig) == "table" then
		if type(repl) ~= "table" then repl = {} end
		del = del or {}
		local copy = {}
		for k, v in pairs(orig) do
			if del[k] == nil then
				copy[k] = copy_replace(v, repl[k], del)
			end
		end
		return setmetatable(copy, getmetatable(orig))
	elseif repl ~= nil then
		return repl
	else
		return orig
	end
end
local resource_mt = {}
resource_mt.__index = resource_mt -- let derived classes inherit from this
function resource_mt:derive(table)
	-- builds a derived class from `resource_mt`
	local meta = {_table = table, __index = self.index, __newindex=self.setindex }
	-- in other words: member:method() will look in the metatable
	return setmetatable(meta, self)
end
function resource_mt.load(table, key)
	-- loads a resource from row `key` of table `table`, recursively with
	-- all subresources.
	local new = simod.select(table, key)
	for fn, ft in pairs(simod.schema[table].fields) do
		if ft == "subresource" then
-- 			local subtable = table..'_'..fn
-- 			local list = simod.list(subtable, key)
-- 			local subresource = {}
-- 			for k, v in pairs(list) do
-- 				subresource[k] = resource_mt.load(subtable, v)
-- 			end
-- 			new[fn] = subresource
		end
	end
	return setmetatable(new, simod.schema[table].methods)
end
function resource_mt:read(key)
	-- reads a row from Lua tables and returns an object of the
	-- corresponding type.
	-- 
	-- Sub-resources are not read at this point but dynamically when
	-- accessing a sub-resource field.
	local table = self._table
-- 	log(blue("reading from table ", table, " line ", key))
	local vals = simod.select(table, key)
	if vals == nil then return vals end
	return setmetatable({_fields = vals, _id=key}, self)
end
function resource_mt:index(key)
	log("indexing ", strdump(self, 1), " with key ", key)
	local methods = getmetatable(self)
	-- if this key represents an existing method in the metatable,
	-- return the method:
	do local meth = methods[key]; if meth ~= nil then return meth end end
	local table = methods._table
	if table == nil then error('missing "_table" field: '..strdump(methods)) end
	local ft = simod.schema[table].fields[key]
-- 	if ft == nil then
-- 		error('field "'..key..'" not found in table "'..table..'"')
-- 	end
	if ft == "subresource" then
		local subtable = table..'_'..key
		local keys,position = simod.list(subtable, self._id)
		log(bold("keys:"), strdump(keys))
		log(bold("position:"), strdump(position))
		return setmetatable({_keys=keys, _position=position, _parent=self._id},
			simod.schema[subtable].resvec_methods)
	else
		return self._fields[key]
	end
end
function resource_mt:setindex(key, value)
	log(red("updating ", strdump(self, 1), " with ", key, value))
	local methods = getmetatable(self)
	local table = methods._table
	if table == nil then error('missing "_table" field: '..strdump(methods)) end
	local ft = simod.schema[table].fields[key]
	if ft == nil then
		error('field "'..key..'" not found in table "'..table..'"')
	end
	if ft == "subresource" then
		error("cannot set subresource vector; use push etc. instead")
-- 		local subtable = table..'_'..key
-- 		local list = simod.list(subtable, self._id)
-- 		dump(list)
-- 		return setmetatable({_list=list}, simod.schema[subtable].resvec_methods)
	else
		simod.update(table, key, self._id, value)
		self._fields[key] = value
	end
end
function resource_mt:save()
	-- saves a resource to database, recursively with all its subresources.
	local methods = getmetatable(self)
	local table = methods._table
	local schema = simod.schema[table]
	local fields = schema.fields
	local id = self.id
	log(red("saving to table ", table, " with id"), id, "parent:", self.parent)
	local done = false
	if id ~= nil then
		local old = simod.select(table, id)
		if old ~= nil then -- an existing row was found for this id
			for fn, val in pairs(old) do
				local new_val = self[fn]
				if val ~= new_val then
					log("value "..blue(fn).." has changed to ", new_val)
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
		id = string.char(unpack(id_tmp))
		self.id = id
		log("generated id: ", self.id)
	end
	if not done then
		simod.insert(table, self)
	end
	log("  after insert: id = ", self.id)
	for fn, ft in pairs(fields) do
		if ft == "subresource" then
			local list = self[fn]
			log("insert subresource: ", fn, #list)
			for i, sub in ipairs(list) do
				sub.parent = self.id
				sub.position = i
				sub:save()
			end
		end
	end
end
local function normalize_changes(changes, dk)
	-- default case: no ID provided
	-- TODO: should we generate one here, or when saving to database?
	if changes == nil then changes = {} end
	if dk == nil then return nil, changes end
	local id
	if type(changes) == "string" then
		-- sword("carsomyr")
		id = changes
		changes = { [dk] = changes }
	elseif changes[dk] == nil and changes[1] ~= nil then
		-- sword{"carsomyr", enchantment = 5 }
		id = changes[1]
		changes[dk] = id
		changes[1] = nil
	end
	return id, changes
end
function resource_mt:clone(args)
	-- Clones a resource, using the changes passed as arguments.
	log("cloning a resource: ", strdump(self, 5))
-- 	print("  with modifiers: ", strdump(args, 5))
	local methods = getmetatable(self)
	local table = methods._table
	local schema = table_schema(table)
	local fields = schema.fields
	local dk = schema.default_key
-- 	print("for table "..self._table.." dk is ", dk)
	local id, changes = normalize_changes(args, dk)
	local new = copy_replace(self, repl, {id = true})
	new.id = id
	return new
end

--««1 Methods for resource vectors
local resvec_mt = {}
resvec_mt.__index = resvec_mt
function resvec_mt:derive(table)
	local meta = {_table = table, __index = self.index,
		each=simod.schema[table].methods }
	return setmetatable(meta, self)
end
function resvec_mt:index(i)
	local methods = getmetatable(self)
	-- if this key represents an existing method in the metatable,
	-- return the method:
	do local meth = methods[i]; if meth ~= nil then return meth end end
	local key = self._keys[i]
	if key == nil then return nil end
	return methods.each:read(key)
end
function resvec_mt:insert(values, index)
	local N = #(self._keys)
	local methods = getmetatable(self)
	if index == nil then index = N+1 end
	log("inserting at index ", index)
	local newpos
	if N == 0 then
		newpos = 0
	elseif index <= 1 then
		newpos = self._position[1]-1
	elseif index > N then
		newpos = self._position[N]+1
	else
		newpos = (self._position[index-1]+self._position[index])/2
	end
	log("new position is: ", newpos)
	local tn = methods._table
	local fields = simod.schema[tn].fields
	log("build new resource of type '"..tn.."' from what we got as argument: "..strdump(values))
	local resource = simod.build_resource(fields, values)
	resource.parent = self._parent
	resource.position = newpos
	log("now resource is: ", strdump(resource, 3))
	setmetatable(resource, methods.each)
	log("simod.insert:")
	local id = simod.insert(tn, resource)
	log("simod.insert done")
	table.insert(self._keys, id, i)
	table.insert(self._position, newpos, i)
	log("now self is "..strdump(self))
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
	log(green("initializing a value from "..strdump(key)))
	-- TODO: check that the key exists
	local new = { _table = self._table, _key = key }
	return setmetatable(new, self)
end

--««1 Updating `simod.schema` and creating all the metatables
-- compute all metatables for resources:
for table, schema in pairs(simod.schema) do
	schema.methods = resource_mt:derive(table)
	schema.resvec_methods = resvec_mt:derive(table)
end

simod.schema.items.default_key = "name"
--««1 Test code
debug.sethook(function(event) log:report(event) end, "cr")
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
-- 	group("simod.set")
		local x = simod.update("items", "name", "sw1h34", "strange sword!")
-- 		dump(x)
-- 	group("simod.schema is:")
-- 		dump(simod.schema)
end
local items_mt = simod.schema.items.methods
test = items_mt:read("ring02")
print("\x1b[31m████\x1b[m")
test_eff = test.effects

print("───")
dump(test_eff)
test_eff:insert({}, 2)

-- mt = getmetatable(test_eff)
-- print(magenta("mt: "), strdump(mt))
-- print(magenta("mt.insert:"), strdump(mt.insert))
-- print(magenta("test_eff.insert:"), strdump(test_eff.insert))


-- test = setmetatable(simod.pull("items", "ring02"), simod.schema.items.methods)
-- dump(simod.schema.items.methods)
-- dump(test)
-- print(getmetatable(test).clone)
-- print(test.clone)
-- foo = test:clone({"blah", name="New name for a ring", weight=3})
-- simod.push("items", foo)
