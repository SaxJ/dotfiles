local prettier = function()
	return {
		exe = "prettier",
		args = { "--stdin-filepath", vim.api.nvim_buf_get_name(0) },
		stdin = true,
	}
end
local phpcsfixer = function()
	return {
		exe = "php-cs-fixer",
		args = { "fix", vim.api.nvim_buf_get_name(0) },
		stdin = false,
	}
end
local uncrust = function()
	return {
		exe = "uncrustify",
		args = { "-l", "CS" },
		stdin = true,
	}
end
local elmFormat = function()
	return {
		exe = "elm-format",
		args = { "--stdin" },
		stdin = true,
	}
end
local brittany = function()
	return {
		exe = "brittany",
		stdin = true,
	}
end
require("formatter").setup({
	logging = false,
	filetype = {
		elm = { elmFormat },
		php = { phpcsfixer },
		javascript = { prettier },
		json = { prettier },
		typescript = { prettier },
		typescriptreact = { prettier },
		css = { prettier },
		scss = { prettier },
		--cs = { uncrust },
		--haskell = { brittany },
		rust = {
			-- Rustfmt
			function()
				return {
					exe = "rustfmt",
					args = { "--emit=stdout" },
					stdin = true,
				}
			end,
		},
		lua = {
			function()
				return {
					exe = "stylua",
					args = { "--indent-width", 2, "-" },
					stdin = true,
				}
			end,
		},
		cpp = {
			-- clang-format
			function()
				return {
					exe = "clang-format",
					args = { "--assume-filename", vim.api.nvim_buf_get_name(0) },
					stdin = true,
					cwd = vim.fn.expand("%:p:h"), -- Run clang-format in cwd of the file.
				}
			end,
		},
		tf = {
			function()
				return {
					exe = "terraform",
					args = { "fmt", "-" },
					stdin = true,
				}
			end,
		},
	},
})

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
