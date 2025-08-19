local readJsonFile = function(path)
  local packageFile = io.open(path, "r")
  if packageFile == nil then
    error('No package.json file.')
  end

  local json = packageFile:read('*a')
  packageFile:close()
  return vim.json.decode(json)
end

local readPackageScripts = function()
  if vim.fn.filereadable('package.json') then
    local json = readJsonFile('package.json')
    if vim.fn.filereadable('yarn.lock') then
      return { runner = 'yarn %s', json = json['scripts'] }
    else
      return { runner = 'npm run %s', json = json['scripts'] }
    end
  end

  if vim.fn.filereadable('compose.json') then
    return { runner = 'composer %s', json = readJsonFile('composer.json')['scripts'] }
  end

  return nil
end

local picker = function()
  local package = readPackageScripts()
  if package == nil then
    error('Could not read runner scripts')
  end
  local scripts = vim.fn.keys(package.json)

  local items = {}
  for idx, command in ipairs(scripts) do
    --- @type snacks.picker.Item
    local item = {
      idx = idx,
      score = 0,
      text = command,
    }

    table.insert(items, item)
  end

  Snacks.picker({
    title = "Package commands",
    items = items,
    confirm = function(picker, confItem)
      picker:norm(function()
        picker:close()
        vim.cmd(string.format("FTerm " .. package.runner, confItem.text))
      end)
    end,
    preview = function()
      return false
    end,
    format = 'text'
  })
end


vim.api.nvim_create_user_command('OpenBuildRunner', picker, { desc = 'Run a build command' })
