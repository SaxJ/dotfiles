local file = io.open("/home/saxonj/.emacs.d/timelog")
local lines = {}
if file ~= nil then
  for line in file:lines() do
    table.insert(lines, line)
  end
  file:close()
end

local suggestions = {}
for _, line in ipairs(lines) do
  local m = ""
  for match in string.gmatch(line, "%a %d+/%d+/%d+ %d+:%d+:%d+ (.*)") do
    print(match)
  end
  -- if m ~= "" then
  --   table.insert(suggestions, m)
  -- end
end
