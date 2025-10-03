local foo = function(x, y)
    return (x + y)
end
local add = function(a, b)
    return (a + b)
end
local result = add(1, 2)
local divmod = function(a, b)
    return (a / b), (a % b)
end
local q, r = divmod(10, 3)
local identity = function(x)
    return x
end
local a = identity(42)
local b = identity("hello")
