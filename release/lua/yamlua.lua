do
local _ENV = _ENV
package.preload[ "deps/lyaml/explicit" ] = function( ... ) local arg = _G.arg;
-- LYAML parse explicit token values.
-- Written by Gary V. Vaughan, 2015
--
-- Copyright (C) 2015-2017 Gary V. Vaughan
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

--- @module lyaml.explicit

local functional = require "lyaml.functional"
local implicit   = require "lyaml.implicit"

local anyof, id = functional.anyof, functional.id

local NULL       = functional.NULL


local yn = {
  y = true, Y = true, n = false, N = false,
}


--- Parse the value following an explicit `!!bool` tag.
-- @function bool
-- @param value token
-- @treturn[1] bool boolean equivalent, if a valid value was recognized
-- @treturn[2] nil otherwise, nil
-- @usage maybe_bool = explicit.bool (tagarg)
local bool = anyof {
  implicit.bool,
  function (x) return yn[x] end,
}


--- Return a function that converts integer results to equivalent float.
-- @tparam function fn token parsing function
-- @treturn function new function that converts int results to float
-- @usage maybe_float = maybefloat (implicit.decimal) (tagarg)
local function maybefloat (fn)
  return function (...)
    local r = fn (...)
    if type (r) == "number" then
      return r + 0.0
    end
  end
end


--- Parse the value following an explicit `!!float` tag.
-- @function float
-- @param value token
-- @treturn[1] number float equivalent, if a valid value was recognized
-- @treturn[2] nil otherwise, nil
-- @usage maybe_float = explicit.float (tagarg)
local float = anyof {
  implicit.float,
  implicit.nan,
  implicit.inf,
  maybefloat (implicit.octal),
  maybefloat (implicit.decimal),
  maybefloat (implicit.hexadecimal),
  maybefloat (implicit.binary),
  implicit.sexfloat,
}


--- Parse the value following an explicit `!!int` tag.
-- @function int
-- @param value token
-- @treturn[1] int integer equivalent, if a valid value was recognized
-- @treturn[2] nil otherwise, nil
-- @usage maybe_int = explicit.int (tagarg)
local int = anyof {
  implicit.octal,
  implicit.decimal,
  implicit.hexadecimal,
  implicit.binary,
  implicit.sexagesimal,
}


--- Parse an explicit `!!null` tag.
-- @treturn lyaml.null
-- @usage null = explicit.null (tagarg)
local function null ()
  return NULL
end


--- Parse the value following an explicit `!!str` tag.
-- @function str
-- @tparam string value token
-- @treturn string *value* which was a string already
-- @usage tagarg = explicit.str (tagarg)
local str = id


--- @export
return {
  bool  = bool,
  float = float,
  int   = int,
  null  = null,
  str   = str,
}

end
end

do
local _ENV = _ENV
package.preload[ "deps/lyaml/functional" ] = function( ... ) local arg = _G.arg;
-- Minimal functional programming utilities.
-- Written by Gary V. Vaughan, 2015
--
-- Copyright (C) 2015-2017 Gary V. Vaughan
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

--- @module lyaml.functional


--- `lyaml.null` value.
-- @table NULL
local NULL = setmetatable ({}, { _type = "LYAML null" })


--- `lyaml.null` predicate.
-- @param x operand
-- @treturn bool `true` if *x* is `lyaml.null`.
local function isnull (x)
  return (getmetatable (x) or {})._type == "LYAML null"
end


--- Callable predicate.
-- @param x operand
-- @treturn bool `true` if *x* is a function has a __call metamethod
-- @usage r = iscallable (x) and x (...)
local function iscallable (x)
  if type (x) ~= "function" then
    x = (getmetatable (x) or {}).__call
  end
  if type (x) == "function" then return x end
end


--- Compose a function to try each callable with supplied args.
-- @tparam table fns list of functions to try
-- @treturn function a new function to call *...* functions, stopping
--   and returning the first non-nil result, if any
local function anyof (fns)
  return function (...)
    for _, fn in ipairs (fns) do
      if iscallable (fn) then
        local r = fn (...)
        if r ~= nil then return r end
      end
    end
  end
end


--- Return arguments unchanged.
-- @param ... arguments
-- @return *...*
local function id (...)
  return ...
end

--- @export
return {
  NULL       = NULL,
  anyof      = anyof,
  id         = id,
  iscallable = iscallable,
  isnull     = isnull,
}

end
end

do
local _ENV = _ENV
package.preload[ "deps/lyaml/implicit" ] = function( ... ) local arg = _G.arg;
-- LYAML parse implicit type tokens.
-- Written by Gary V. Vaughan, 2015
--
-- Copyright (C) 2015-2017 Gary V. Vaughan
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

--- @module lyaml.implicit


local NULL = require "lyaml.functional".NULL


local is_null = {
  [""] = true, ["~"] = true, null = true, Null = true, NULL = true,
}


--- Parse a null token to a null value.
-- @param value token
-- @return[1] lyaml.null, for an empty string or literal ~
-- @return[2] nil otherwise, nil
-- @usage maybe_null = implicit.null (token)
local function null (value)
  if is_null[value] then
    return NULL
  end
end


local to_bool = {
  ["true"]  = true,  True  = true,  TRUE  = true,
  ["false"] = false, False = false, FALSE = false,
  yes       = true,  Yes   = true,  YES   = true,
  no        = false, No    = false, NO    = false,
  on        = true,  On    = true,  ON    = true,
  off       = false, Off   = false, OFF   = false,
}


--- Parse a boolean token to the equivalent value.
-- Treats capilalized, lower and upper-cased variants of true/false,
-- yes/no or on/off tokens as boolean `true` and `false` values.
-- @param value token
-- @treturn[1] bool if a valid boolean token was recognized
-- @treturn[2] nil otherwise, nil
-- @usage maybe_bool = implicit.bool (token)
local function bool (value)
  return to_bool[value]
end


--- Parse a binary token, such as "0b1010\_0111\_0100\_1010\_1110".
-- @tparam string value token
-- @treturn[1] int integer equivalent, if a valid token was recognized
-- @treturn[2] nil otherwise, nil
-- @usage maybe_int = implicit.binary (value)
local function binary (value)
  local r
  value:gsub ("^([+-]?)0b_*([01][01_]+)$", function (sign, rest)
    r = 0
    rest:gsub ("_*(.)", function (digit)
      r = r * 2 + tonumber (digit)
    end)
    if sign == "-" then r = r * -1 end
  end)
  return r
end


--- Parse an octal token, such as "012345".
-- @tparam string value token
-- @treturn[1] int integer equivalent, if a valid token was recognized
-- @treturn[2] nil otherwise, nil
-- @usage maybe_int = implicit.octal (value)
local function octal (value)
  local r
  value:gsub ("^([+-]?)0_*([0-7][0-7_]*)$", function (sign, rest)
    r = 0
    rest:gsub ("_*(.)", function (digit)
      r = r * 8 + tonumber (digit)
    end)
    if sign == "-" then r = r * -1 end
  end)
  return r
end


--- Parse a decimal token, such as "0" or "12345".
-- @tparam string value token
-- @treturn[1] int integer equivalent, if a valid token was recognized
-- @treturn[2] nil otherwise, nil
-- @usage maybe_int = implicit.decimal (value)
local function decimal (value)
  local r
  value:gsub ("^([+-]?)_*([0-9][0-9_]*)$", function (sign, rest)
    rest = rest:gsub ("_", "")
    if rest == "0" or #rest > 1 or rest:sub (1, 1) ~= "0"  then
      r = tonumber (rest)
      if sign == "-" then r = r * -1 end
    end
  end)
  return r
end


--- Parse a hexadecimal token, such as "0xdeadbeef".
-- @tparam string value token
-- @treturn[1] int integer equivalent, if a valid token was recognized
-- @treturn[2] nil otherwise, nil
-- @usage maybe_int = implicit.hexadecimal (value)
local function hexadecimal (value)
  local r
  value:gsub ("^([+-]?)(0x_*[0-9a-fA-F][0-9a-fA-F_]*)$",
    function (sign, rest)
      rest = rest:gsub ("_", "")
      r = tonumber (rest)
      if sign == "-" then r = r * -1 end
    end
  )
  return r
end


--- Parse a sexagesimal token, such as "190:20:30".
-- Useful for times and angles.
-- @tparam string value token
-- @treturn[1] int integer equivalent, if a valid token was recognized
-- @treturn[2] nil otherwise, nil
-- @usage maybe_int = implicit.sexagesimal (value)
local function sexagesimal (value)
  local r
  value:gsub ("^([+-]?)([0-9]+:[0-5]?[0-9][:0-9]*)$", function (sign, rest)
    r = 0
    rest:gsub ("([0-9]+):?", function (digit)
      r = r * 60 + tonumber (digit)
    end)
    if sign == "-" then r = r * -1 end
  end)
  return r
end


local isnan = {
  [".nan"] = true, [".NaN"] = true, [".NAN"] = true,
}


--- Parse a `nan` token.
-- @tparam string value token
-- @treturn[1] nan not-a-number, if a valid token was recognized
-- @treturn[2] nil otherwise, nil
-- @usage maybe_nan = implicit.nan (value)
local function nan (value)
  if isnan[value] then return 0/0 end
end


local isinf = {
  [".inf"]  = math.huge,  [".Inf"]  = math.huge,  [".INF"]  = math.huge,
  ["+.inf"] = math.huge,  ["+.Inf"] = math.huge,  ["+.INF"] = math.huge,
  ["-.inf"] = -math.huge, ["-.Inf"] = -math.huge, ["-.INF"] = -math.huge,
}


--- Parse a signed `inf` token.
-- @tparam string value token
-- @treturn[1] number plus/minus-infinity, if a valid token was recognized
-- @treturn[2] nil otherwise, nil
-- @usage maybe_inf = implicit.inf (value)
local function inf (value)
  return isinf[value]
end


--- Parse a floating point number token, such as "1e-3" or "-0.12".
-- @tparam string value token
-- @treturn[1] number float equivalent, if a valid token was recognized
-- @treturn[2] nil otherwise, nil
-- @usage maybe_float = implicit.float (value)
local function float (value)
  local r = tonumber ((value:gsub ("_", "")))
  if r and value:find "[%.eE]" then return r end
end


--- Parse a sexagesimal float, such as "190:20:30.15".
-- Useful for times and angles.
-- @tparam string value token
-- @treturn[1] number float equivalent, if a valid token was recognized
-- @treturn[2] nil otherwise, nil
-- @usage maybe_float = implicit.sexfloat (value)
local function sexfloat (value)
  local r
  value:gsub ("^([+-]?)([0-9]+:[0-5]?[0-9][:0-9]*)(%.[0-9]+)$",
    function (sign, rest, float)
      r = 0
      rest:gsub ("([0-9]+):?", function (digit)
        r = r * 60 + tonumber (digit)
      end)
      r = r + tonumber (float)
      if sign == "-" then r = r * -1 end
    end
  )
  return r
end


--- @export
return {
  binary      = binary,
  decimal     = decimal,
  float       = float,
  hexadecimal = hexadecimal,
  inf         = inf,
  nan         = nan,
  null        = null,
  octal       = octal,
  sexagesimal = sexagesimal,
  sexfloat    = sexfloat,
  bool        = bool,
}

end
end

do
local _ENV = _ENV
package.preload[ "deps/lyaml/init" ] = function( ... ) local arg = _G.arg;
-- Transform between YAML 1.1 streams and Lua table representations.
-- Written by Gary V. Vaughan, 2013
--
-- Copyright (C) 2013-2017 Gary V. Vaughan
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
--
-- Portions of this software were inspired by an earlier LibYAML binding
-- by Andrew Danforth <acd@weirdness.net>

--- @module lyaml


local yaml       = require "yaml"
local explicit   = require "lyaml.explicit"
local implicit   = require "lyaml.implicit"
local functional = require "lyaml.functional"

local anyof, id, isnull =
  functional.anyof, functional.id, functional.isnull


--- `lyaml.null` value.
-- @table null
local null       = functional.NULL

local TAG_PREFIX = "tag:yaml.org,2002:"


local function tag (name)
  return TAG_PREFIX .. name
end


local default = {
  -- Tag table to lookup explicit scalar conversions.
  explicit_scalar = {
    [tag "bool"]  = explicit.bool,
    [tag "float"] = explicit.float,
    [tag "int"]   = explicit.int,
    [tag "null"]  = explicit.null,
    [tag "str"]   = explicit.str,
  },
  -- Order is important, so we put most likely and fastest nearer
  -- the top to reduce average number of comparisons and funcalls.
  implicit_scalar = anyof {
    implicit.null,
    implicit.octal,	-- subset of decimal, must come earlier
    implicit.decimal,
    implicit.float,
    implicit.bool,
    implicit.inf,
    implicit.nan,
    implicit.hexadecimal,
    implicit.binary,
    implicit.sexagesimal,
    implicit.sexfloat,
    id,
  },
}


-- Metatable for Dumper objects.
local dumper_mt = {
  __index = {
    -- Emit EVENT to the LibYAML emitter.
    emit = function (self, event)
      return self.emitter.emit (event)
    end,

    -- Look up an anchor for a repeated document element.
    get_anchor = function (self, value)
      local r = self.anchors[value]
      if r then
	self.aliased[value], self.anchors[value] = self.anchors[value], nil
      end
      return r
    end,

    -- Look up an already anchored repeated document element.
    get_alias = function (self, value)
      return self.aliased[value]
    end,

    -- Dump ALIAS into the event stream.
    dump_alias = function (self, alias)
      return self:emit {
	type   = "ALIAS",
	anchor = alias,
      }
    end,

    -- Dump MAP into the event stream.
    dump_mapping = function (self, map)
      local alias = self:get_alias (map)
      if alias then
	return self:dump_alias (alias)
      end

      self:emit {
        type   = "MAPPING_START",
        anchor = self:get_anchor (map),
        style  = "BLOCK",
      }
      for k, v in pairs (map) do
        self:dump_node (k)
        self:dump_node (v)
      end
      return self:emit {type = "MAPPING_END"}
    end,

    -- Dump SEQUENCE into the event stream.
    dump_sequence = function (self, sequence)
      local alias = self:get_alias (sequence)
      if alias then
	return self:dump_alias (alias)
      end

      self:emit {
        type = "SEQUENCE_START",
        anchor = self:get_anchor (sequence),
        style = "BLOCK",
      }
      for _, v in ipairs (sequence) do
        self:dump_node (v)
      end
      return self:emit {type = "SEQUENCE_END"}
    end,

    -- Dump a null into the event stream.
    dump_null = function (self)
      return self:emit {
        type            = "SCALAR",
        value           = "~",
        plain_implicit  = true,
        quoted_implicit = true,
        style           = "PLAIN",
      }
    end,

    -- Dump VALUE into the event stream.
    dump_scalar = function (self, value)
      local alias = self:get_alias (value)
      if alias then
	return self:dump_alias (alias)
      end

      local anchor = self:get_anchor (value)
      local itsa = type (value)
      local style = "PLAIN"
      if itsa == "string" and self.implicit_scalar (value) ~= value then
	-- take care to round-trip strings that look like scalars
        style = "SINGLE_QUOTED"
      elseif value == math.huge then
	value = ".inf"
      elseif value == -math.huge then
	value = "-.inf"
      elseif value ~= value then
	value = ".nan"
      elseif itsa == "number" or itsa == "boolean" then
        value = tostring (value)
      elseif itsa == "string" and string.find (value, "\n") then
        style = "LITERAL"
      end
      return self:emit {
        type            = "SCALAR",
	anchor          = anchor,
        value           = value,
        plain_implicit  = true,
        quoted_implicit = true,
        style           = style,
      }
    end,

    -- Decompose NODE into a stream of events.
    dump_node = function (self, node)
      local itsa = type (node)
      if isnull (node) then
        return self:dump_null ()
      elseif itsa == "string" or itsa == "boolean" or itsa == "number" then
        return self:dump_scalar (node)
      elseif itsa == "table" then
        if #node > 0 then
          return self:dump_sequence (node)
        else
          return self:dump_mapping (node)
        end
      else -- unsupported Lua type
        error ("cannot dump object of type '" .. itsa .. "'", 2)
      end
    end,

    -- Dump DOCUMENT into the event stream.
    dump_document = function (self, document)
      self:emit {type = "DOCUMENT_START"}
      self:dump_node (document)
      return self:emit {type = "DOCUMENT_END"}
    end,
  },
}


-- Emitter object constructor.
local function Dumper (opts)
  local anchors = {}
  for k, v in pairs (opts.anchors) do anchors[v] = k end
  local object = {
    aliased         = {},
    anchors         = anchors,
    emitter         = yaml.emitter (),
    implicit_scalar = opts.implicit_scalar,
  }
  return setmetatable (object, dumper_mt)
end


--- Dump options table.
-- @table dumper_opts
-- @tfield table anchors map initial anchor names to values
-- @tfield function implicit_scalar parse implicit scalar values


--- Dump a list of Lua tables to an equivalent YAML stream.
-- @tparam table documents a sequence of Lua tables.
-- @tparam[opt] dumper_opts opts initialisation options
-- @treturn string equivalest YAML stream
local function dump (documents, opts)
  opts = opts or {}

  -- backwards compatibility
  if opts.anchors == nil and opts.implicit_scalar == nil then
    opts = { anchors = opts }
  end

  local dumper = Dumper {
    anchors         = opts.anchors or {},
    implicit_scalar = opts.implicit_scalar or default.implicit_scalar,
  }

  dumper:emit { type = "STREAM_START", encoding = "UTF8" }
  for _, document in ipairs (documents) do
    dumper:dump_document (document)
  end
  local ok, stream = dumper:emit { type = "STREAM_END" }
  return stream
end


-- We save anchor types that will match the node type from expanding
-- an alias for that anchor.
local alias_type = {
  MAPPING_END    = "MAPPING_END",
  MAPPING_START  = "MAPPING_END",
  SCALAR         = "SCALAR",
  SEQUENCE_END   = "SEQUENCE_END",
  SEQUENCE_START = "SEQUENCE_END",
}


-- Metatable for Parser objects.
local parser_mt = {
  __index = {
    -- Return the type of the current event.
    type = function (self)
      return tostring (self.event.type)
    end,

    -- Raise a parse error.
    error = function (self, errmsg, ...)
      error (string.format ("%d:%d: " .. errmsg, self.mark.line,
                            self.mark.column, ...), 0)
    end,

    -- Save node in the anchor table for reference in future ALIASes.
    add_anchor = function (self, node)
      if self.event.anchor ~= nil then
        self.anchors[self.event.anchor] = {
	  type  = alias_type[self.event.type],
	  value = node,
	}
      end
    end,

    -- Fetch the next event.
    parse = function (self)
      local ok, event = pcall (self.next)
      if not ok then
	-- if ok is nil, then event is a parser error from libYAML
	self:error (event:gsub (" at document: .*$", ""))
      end
      self.event = event
      self.mark  = {
	line     = self.event.start_mark.line + 1,
	column   = self.event.start_mark.column + 1,
      }
      return self:type ()
    end,

    -- Construct a Lua hash table from following events.
    load_map = function (self)
      local map = {}
      self:add_anchor (map)
      while true do
        local key = self:load_node ()
        local tag = self.event.tag
	if tag then tag = tag:match ("^" .. TAG_PREFIX .. "(.*)$") end
        if key == nil then break end
	if key == "<<" or tag == "merge" then
	  tag = self.event.tag or key
	  local node, event = self:load_node ()
	  if event == "MAPPING_END" then
	    for k, v in pairs (node) do
	      if map[k] == nil then map[k] = v end
	    end

	  elseif event == "SEQUENCE_END" then
	    for i, merge in ipairs (node) do
	      if type (merge) ~= "table" then
		self:error ("invalid '%s' sequence element %d: %s",
		            tag, i, tostring (merge))
	      end
	      for k, v in pairs (merge) do
		if map[k] == nil then map[k] = v end
              end
	    end

	  else
	    if event == "SCALAR" then event = tostring (node) end
	    self:error ("invalid '%s' merge event: %s", tag, event)
	  end
	else
          local value, event = self:load_node ()
          if value == nil then
            self:error ("unexpected %s event", self:type ())
          end
          map[key] = value
	end
      end
      return map, self:type ()
    end,

    -- Construct a Lua array table from following events.
    load_sequence = function (self)
      local sequence = {}
      self:add_anchor (sequence)
      while true do
        local node = self:load_node ()
        if node == nil then break end
        sequence[#sequence + 1] = node
      end
      return sequence, self:type ()
    end,

    -- Construct a primitive type from the current event.
    load_scalar = function (self)
      local value = self.event.value
      local tag   = self.event.tag
      local explicit = self.explicit_scalar[tag]

      -- Explicitly tagged values.
      if explicit then
	value = explicit (value)
	if value == nil then
          self:error ("invalid '%s' value: '%s'", tag, self.event.value)
        end

      -- Otherwise, implicit conversion according to value content.
      elseif self.event.style == "PLAIN" then
	value = self.implicit_scalar (self.event.value)
      end
      self:add_anchor (value)
      return value, self:type ()
    end,

    load_alias = function (self)
      local anchor = self.event.anchor
      local event  = self.anchors[anchor]
      if event == nil then
        self:error ("invalid reference: %s", tostring (anchor))
      end
      return event.value, event.type
    end,

    load_node = function (self)
      local dispatch  = {
        SCALAR         = self.load_scalar,
        ALIAS          = self.load_alias,
        MAPPING_START  = self.load_map,
        SEQUENCE_START = self.load_sequence,
        MAPPING_END    = function () end,
        SEQUENCE_END   = function () end,
        DOCUMENT_END   = function () end,
      }

      local event = self:parse ()
      if dispatch[event] == nil then
        self:error ("invalid event: %s", self:type ())
      end
     return dispatch[event] (self)
    end,
  },
}


-- Parser object constructor.
local function Parser (s, opts)
  local object = {
    anchors         = {},
    explicit_scalar = opts.explicit_scalar,
    implicit_scalar = opts.implicit_scalar,
    mark            = { line = 0, column = 0 },
    next            = yaml.parser (s),
  }
  return setmetatable (object, parser_mt)
end


--- Load options table.
-- @table loader_opts
-- @tfield boolean all load all documents from the stream
-- @tfield table explicit_scalar map full tag-names to parser functions
-- @tfield function implicit_scalar parse implicit scalar values


--- Load a YAML stream into a Lua table.
-- @tparam string s YAML stream
-- @tparam[opt] loader_opts opts initialisation options
-- @treturn table Lua table equivalent of stream *s*
local function load (s, opts)
  opts = opts or {}
  local documents = {}
  local all       = false

  -- backwards compatibility
  if opts == true then
    opts = { all = true }
  end

  local parser = Parser (s, {
    explicit_scalar = opts.explicit_scalar or default.explicit_scalar,
    implicit_scalar = opts.implicit_scalar or default.implicit_scalar,
  })

  if parser:parse () ~= "STREAM_START" then
    error ("expecting STREAM_START event, but got " .. parser:type (), 2)
  end

  while parser:parse () ~= "STREAM_END" do
    local document = parser:load_node ()
    if document == nil then
      error ("unexpected " .. parser:type () .. " event")
    end

    if parser:parse () ~= "DOCUMENT_END" then
      error ("expecting DOCUMENT_END event, but got " .. parser:type (), 2)
    end

    -- save document
    documents[#documents + 1] = document

    -- reset anchor table
    parser.anchors = {}
  end

  return opts.all and documents or documents[1]
end


--[[ ----------------- ]]--
--[[ Public Interface. ]]--
--[[ ----------------- ]]--


--- @export
return {
  dump      = dump,
  load      = load,
  null      = null,

  --- Version number from yaml C binding.
  -- @table _VERSION
  _VERSION  = yaml.version,
}

end
end

do
local _ENV = _ENV
package.preload[ "yamlua" ] = function( ... ) local arg = _G.arg;
return require("deps/lyaml/init")

end
end

