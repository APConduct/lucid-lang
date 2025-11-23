local getOrDefault = function(value, default)
    return value
end
local x = getOrDefault(42, 0)
local y = getOrDefault(nil, "default")
