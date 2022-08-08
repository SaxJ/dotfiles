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

-- Format on save
vim.api.nvim_exec(
	[[
augroup FormatAutogroup
  autocmd!
  autocmd BufWritePost *.tsx,*.ts,*.jsx,*.rs,*.lua,*.cpp,*.tf FormatWrite
augroup END
]],
	true
)

return formatting_options
