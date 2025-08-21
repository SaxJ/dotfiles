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
  local scriptsList = vim.fn.keys(package.json)

  local pickers = require("telescope.pickers")
  local finders = require("telescope.finders")
  local conf = require("telescope.config").values
  local actions = require "telescope.actions"
  local action_state = require "telescope.actions.state"

  local myPicker = pickers.new({}, {
    prompt_title = 'Run',
    finder = finders.new_table({
      results = scriptsList,
    }),
    sorter = conf.generic_sorter({}),
    attach_mappings = function(prompt_bufnr, map)
      actions.select_default:replace(function()
        actions.close(prompt_bufnr)
        local selection = action_state.get_selected_entry()

        vim.cmd(string.format("FTerm " .. package.runner, selection[1]))
      end)
      return true
    end
  })

  myPicker:find()
end


vim.api.nvim_create_user_command('OpenBuildRunner', picker, { desc = 'Run a build command' })
