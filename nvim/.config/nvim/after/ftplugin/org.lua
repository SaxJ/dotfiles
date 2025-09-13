vim.keymap.set("i", "<S-CR>", function ()
  require('orgmode').action("org_mappings.meta_return")
end, {silent = true, buffer = true})
