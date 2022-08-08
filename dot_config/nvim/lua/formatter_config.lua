local util = require('formatter.util')

local formatting_options = {
    logging = false,
    log_level = vim.log.levels.WARN,
    lua = {
        require('formatter.filetypes.lua').stylua,
    },
    cs = {
        require('formatter.filetypes.cs').dotnetformat,
    },
    ts = {
        require('formatter.filetypes.typescript').prettier
    },
    tsx = {
        require('formatter.filetypes.typescript').prettier
    },
    ['*'] = {
        require("formatter.filetypes.any").remove_trailing_whitespace
    }
}

vim.cmd([[
augroup FormatAutogroup
  autocmd!
  autocmd User FormatterPre lua print "This will print before formatting"
  autocmd User FormatterPost lua print "This will print after formatting"
augroup END
]])

return formatting_options
