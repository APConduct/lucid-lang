function create_person(name, age)
    local person = {name = name, age = age}
    return person
end
function create_dog()
    local dog = {name = "Buddy", age = 5, speak = function()
    end}
    return dog
end
function create_cat(name)
    return {name = name, age = 3, speak = function()
    end}
end
local john = create_person("John", 30)
local spot = create_dog()
local whiskers = create_cat("Whiskers")
local complex_object = {person = {name = "Alice", age = 25}, metadata = {created = "2024-01-01", version = 1}}
local mixed = {simple = "value", ["complex key"] = "another value", nested = {data = 42}}
function get_config()
    return {host = "localhost", port = 8080, ssl = nil}
end
