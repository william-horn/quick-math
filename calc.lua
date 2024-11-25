--[[
	@author: William J. Horn
	
	My extension of the string library
]]


local StringFunctions = {}

local StringMath = {
	OperationType = {
		Exponent = 'Exponent',
		Multiplication = 'Multiplication',
		Division = 'Division',
		Addition = 'Addition',
		Subtraction = 'Subtraction'
	}
}

local function cloneTable(t)
  local nt = {}
  for k, v in next, t do
    nt[k] = v
  end
  return nt
end


--[[
	@desc: Escapes a string by replacing all chars with their byte value
	@params: <string> str, <string> chars
	@returns: <string> escapedStr
]]
function StringFunctions:escapeChars(str, chars)
	return str:gsub("/([/"..chars:gsub(".", "%%%0").."])", function(n) return "\1"..string.byte(n).."\1" end)
end

--[[
	@desc: Escapes Lua's magic characters
	@params: <string> chars
	@returns: <string> escapedChars
]]
function StringFunctions:escapeMagicChars(chars)
	return chars:gsub("[%^%$%(%)%%%.%[%]%*%+%-%?]", "%%%1")
end

--[[
	@desc: Unescapes a string by replacing all byte values with their char value
	@params: <string> str, <string> chars
	@returns: <string> unescapedStr
]]
function StringFunctions:unescapeChars(str, chars)
	return str:gsub("\1(%d+)\1", chars or function(n) return string.char(n) end)
end

--[[
	@desc: Splits a string by a delimiter and returns a table of the split values
	@params: <string> str, <string> delim
	@returns: <table> splitStr
]]
function StringFunctions:splitWith(str, delim, usePatterns)
	str = self:escapeChars(str, delim)
	
	if (not usePatterns) then
		delim = self:escapeMagicChars(delim)
	end
	
	local t = {}
	
	for element in str:gmatch("[^"..delim.."]+") do
		t[#t + 1] = self:trimAround(self:unescapeChars(element))
	end
	
	return t
end

--[[
	@desc: Removes whitespace from the left of a string
	@params: <string> str
	@returns: <string> trimmedStr
]]
function StringFunctions:trimLeft(str)
	return str:gsub("^%s+", "")
end

--[[
	@desc: Removes whitespace from the right of a string
	@params: <string> str
	@returns: <string> trimmedStr
]]
function StringFunctions:trimRight(str)
	return str:gsub("%s+$", "")
end

--[[
	@desc: Removes whitespace from the left and right of a string
	@params: <string> str
	@returns: <string> trimmedStr
]]
function StringFunctions:trimAround(str)
	return self:trimLeft(self:trimRight(str))
end

--[[
	@desc: Removes all whitespace from a string
	@params: <string> str
	@returns: <string> trimmedStr
]]
function StringFunctions:removeWhitespace(str)
	return str:gsub("%s+", "")
end

--[[
	@desc: Checks if a string is empty or not
	@params: <string> str, <boolean> countSpaceCharAsEmpty (whether to count space chars as empty)
	@returns: <boolean> isEmpty
]]
function StringFunctions:isEmpty(str, countSpaceCharAsEmpty)
	return (countSpaceCharAsEmpty) and (self:removeWhitespace(str) == '') or (str == '')
end

--[[
	@desc: Takes an arithmatic math operation string such as '1+1' and computes the numeric value
	@params: <string> operand1, <string> operator, <string> operand2
	@returns: <number> result
]]
function StringFunctions:doMathOp(a, op, b)
	a = tonumber(a)
	b = tonumber(b)

	if (op == '/') then
		return a/b
	elseif (op == '*') then
		return a*b
	elseif (op == '^') then
		return a^b
	elseif (op == '+') then
		return a + b
	elseif (op == '-') then
		return a - b
	end
end

--[[
	@desc: Replaces all math operations of the same type in a string with their computed value
	@params: <string> str, <LocalEnum> ops
	@returns: <string> result
	
	Example:
		String:replaceWithCombinedOperationResult('1+2+3*4*5+6', StringEnum.Math.OperationType.Multiplication)
		
	output: 
		'1+2+60+6'
]]
function StringFunctions:replaceWithCombinedOperationResult(str, ops)
	local OperationType = StringMath.OperationType
	local opString

	if (ops == OperationType.Exponent) then
		opString = '^'
	elseif (ops == OperationType.Multiplication or ops == OperationType.Division) then
		opString = '*/'
	elseif (ops == OperationType.Addition or ops == OperationType.Subtraction) then
		opString = '+-'
	else
		error('OperationType is not recognized')
	end

	opString = self:escapeMagicChars(opString)

	-- Saftey measure to stop negative numbers from being detected by the initial combined operations pattern
	-- This gets picked up by multiplication parsing: 3*2-5 when only 3*2 should pass.
	-- Now we turn 3*2-5 into 3*2+-5 to ensure just 3*2 passes.
	str = str:gsub('(%d)(%-[%d%.])', '%1+%2'):gsub('%-%-', '+')

	-- match and replace all consecutive math operations of the same type
	-- ex: matches '3*4*5' and '7*8' from '1+2+3*4*5+6+7*8'
	str = str:gsub('(%-?%d*%.?%d+['..opString..'])([%-?%.?%d'..opString..']+)', function(startSequence, subSequence)
		subSequence = startSequence .. subSequence

		-- keep track of the total as we run through the consecutive math operations
		local result = 0

		-- match the individual operations and compute them
		for a, op, b in subSequence:gmatch('(%-?%d*%.?%d*)(['..opString..'])(%-?%d*%.?%d+)') do
			a = tonumber(a)
			b = tonumber(b)

			if (not a) then
				result = self:doMathOp(result, op, b)
			else
				result = self:doMathOp(a, op, b)
			end
		end

		return tostring(result)
	end)

	return str
end

--[[
	@desc: Takes a math expression and returns the numeric result
	@params: <string> expression
	@returns: <number> result
]]
function StringFunctions:getResultFromMathExpression(expression)
	expression = self:removeWhitespace(expression)
	local OperationType = StringMath.OperationType

	local function evaluate(exp)
		-- Replace all parenthesis first by recursively checking for parenthesis pairs.
		-- Then replace parenthesis with the evaluated result
		local result, count = exp:gsub('(%b())', function(subExp)
			return evaluate(subExp:sub(2, -2))
		end)

		-- If a parenthesis pair was found, we must re-evaluate the expression with the new parenthesis result.
		-- Else, perform all math operations and return the result
		if (count > 0) then
			return evaluate(result)
		else
			-- Compute the expression following the order of operations
			local combinedExponent = self:replaceWithCombinedOperationResult(result, OperationType.Exponent)
			local combinedMult = self:replaceWithCombinedOperationResult(combinedExponent, OperationType.Multiplication)
			local combinedAdd = self:replaceWithCombinedOperationResult(combinedMult, OperationType.Addition)

			return combinedAdd
		end
	end

	return tonumber(evaluate(expression))
end

--[[
	@desc: Substitutes variables and functions in a string
	@params: 
		<table> options:
			<string> source, 
			<table> vars, 
			<table> functions
	@returns: <string> substituted string
	
	Example:
		local source = 'Hello $name, how are $getPerson() today?'
		
		local substituted = StringFunctions:substitute(source, {
			vars = { name = 'Bob' },
			functions = {
				getPerson = function()
					return 'you'
				end
			}
		})
		
		print(substituted)
		> Hello Bob, how are you today?
]]
function StringFunctions:substitute(str, options)
	local globalOptions = self._substitutionOptions or {}
	
	if (options) then
		options = cloneTable(options)
		
		for opKey, opVal in pairs({ vars = globalOptions.vars, functions = globalOptions.functions }) do
			if (not options[opKey]) then
				options[opKey] = opVal
			else
				for key, val in pairs(opVal) do
					if (type(options[opKey][key]) == 'nil') then
						options[opKey][key] = val
					end
				end
			end
		end
	else
		options = globalOptions
	end
	
	local vars = options.vars or {}
	local functions = options.functions or {}

	local escapedStr = self:escapeChars(str, '$')
	local varStr = escapedStr:gsub("%$([%w_]+)", vars)

	local function replaceStr(str)
		str = str:gsub('%$([%w_]+)(%b())', function(funcName, args)
			local func = functions[funcName]

			if (func) then
				local args = replaceStr(args:sub(2, -2))
				return func(unpack(self:splitWith(args, ',')))
			else
				return '$'..funcName..args
			end
		end)

		return str
	end

	return ({ self:unescapeChars(replaceStr(varStr)) })[1]
end

--[[
	@desc: Sets the global variables and functions for the substitution function to use based on a preset
	@params: <string> option name
	@returns: <table> StringFunctions
]]
-- function StringFunctions:selectSubstitutionOptions(optionName)
-- 	local options = require(Path__SubstitutionOptions[optionName])
	
-- 	if (options) then
-- 		self._substitutionOptions = options
-- 	else
-- 		error('No such option preset exists')
-- 	end
	
-- 	return self
-- end

--[[
	@desc: Updates the global variables and functions for the substitution function to use
	@params: <table> options
	@returns: <table> StringFunctions
]]
function StringFunctions:updateSubstitutionOptions(options)
	for opKey, opValue in pairs({vars = options.vars, functions = options.functions}) do
		for key, val in pairs(opValue) do
			self._substitutionOptions[opKey][key] = val
		end
	end
	
	return self
end

--[[
	@desc: Sets the global variables and functions for the substitution function to use
	@params: <table> options
	@returns: <table> StringFunctions
]]
function StringFunctions:setSubstitutionOptions(options)
	self._substitutionOptions = {
    vars = {
      pi = math.pi,
      tau = math.pi*2,
    },
    
    functions = {
      eval = function(exp)
        return self:getResultFromMathExpression(exp)
      end,
      floor = function(n)
        return math.floor(self:getResultFromMathExpression(n))
      end,
      ceil = function(n)
        return math.ceil(self:getResultFromMathExpression(n))
      end,
      min = function(a, b)
        return math.min(
          self:getResultFromMathExpression(a),
          self:getResultFromMathExpression(b)
        )
      end,
      max = function(a, b)
        return math.max(
          self:getResultFromMathExpression(a),
          self:getResultFromMathExpression(b)
        )
      end,
    }
  };
	return self
end

--[[
	@desc: Determine if a and b match, according to the options table.
	@param a <string>
	@param b <string>
	@param options <Table>
		caseSensitive - (default: true) whether case sensitivity is used in the comparison (true = yes)
		usePatterns - (default: false) whether pattern matching is enabled (true = yes)
		
	@returns: validMatch <boolean>
]]
function StringFunctions:isValidMatch(a, b, options)
	options = options or {
		caseSensitive = true,
		usePatterns = false
	}

	if (not (type(a) == 'string' and type(b) == 'string')) then
		return a == b
	end

	if (options.caseSensitive == false) then
		a = a:lower()
		b = b:lower()
	end

	if (options.usePatterns == false) then
		return a == b
	end

	return a:match(b) ~= nil
end

StringFunctions:setSubstitutionOptions()

while true do
  local s,m = pcall(function()
    local input = io.read()
    local num = StringFunctions:getResultFromMathExpression(StringFunctions:substitute(input))
    print(num)
  end)

  if not s then
    print(m)
  end
end
