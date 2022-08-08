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

vim.api.nvim_create_autocmd({'BufWritePost'}, {pattern = {"*"}, command = "FormatWrite"})

return formatting_options
