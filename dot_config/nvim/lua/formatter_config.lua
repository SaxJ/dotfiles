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
}

return formatting_options
