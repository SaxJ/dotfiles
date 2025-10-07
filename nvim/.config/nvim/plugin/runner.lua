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
  local scripts_list = vim.fn.keys(package.json)

  local items = {}
  for idx, cmd in ipairs(scripts_list) do
    local item = {
      idx = idx,
      name = cmd,
      text = cmd,
      action = string.format(package.runner, cmd),
      preview = package.json[cmd],
    }

    table.insert(items, item)
  end

  Snacks.picker({
    title = "Runner",
    layout = {
      preset = 'default',
      preview = nil,
    },
    preview = function(ctx)
      ctx.preview:reset()
      ctx.preview:highlight({ ft = 'bash' })
      ctx.preview:set_lines({ ctx.item.preview })
    end,
    format = function(item)
      return {
        { item.text }
      }
    end,
    items = items,
    confirm = function(picker, item)
      picker:close()
      vim.cmd('HTerm ' .. item.action)
    end
  })
end


vim.api.nvim_create_user_command('OpenBuildRunner', picker, { desc = 'Run a build command' })
