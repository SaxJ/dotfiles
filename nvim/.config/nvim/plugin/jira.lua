local get_issues = function()
  local handle = vim.system({ 'jira', 'issue', 'list', '--raw' }, { text = true }):wait()
  if handle.code > 0 then
    error('An error occurred running jira command')
    return {}
  end

  return vim.json.decode(handle.stdout)
end

local picker = function()
  local issues = get_issues()

  local items = {}
  for idx, issue in ipairs(issues) do
    local item = {
      idx = idx,
      key = issue['key'],
      text = issue['key'] .. issue['fields']['summary'],
      summary = issue['fields']['summary'],
      preview = {
        text = issue['fields']['summary']
      },
    }

    table.insert(items, item)
  end

  Snacks.picker({
    title = "Runner",
    layout = {
      preset = 'default',
    },
    format = function(item)
      return {
        { item.key },
        { "  " },
        { item.summary },
      }
    end,
    items = items,
    confirm = function(picker, item)
      picker:close()
      print(item.key)
    end,
    preview = function(ctx)
      Snacks.picker.preview.cmd({ 'jira', 'issue', 'view', ctx.item.key, '--plain' }, ctx, { ft = 'markdown' })
    end
  })
end


vim.api.nvim_create_user_command('JiraIssues', picker, { desc = 'List jira issues' })
