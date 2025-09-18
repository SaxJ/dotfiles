vim.keymap.set("i", "<A-CR>", function()
  require('orgmode').action("org_mappings.meta_return")
end, { silent = true, buffer = true })
