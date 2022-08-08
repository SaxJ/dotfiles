local phpcsfixer = function()
	return {
		exe = "php-cs-fixer",
		args = { "fix" },
		stdin = false,
	}
end
local csharpier = function()
	return {
		exe = "dotnet-csharpier",
		stdin = false,
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
		javascript = { require("formatter.filetypes.javascript").prettier },
		json = { require("formatter.filetypes.json").prettier },
		typescript = { require("formatter.filetypes.typescript").prettier },
		typescriptreact = { require("formatter.filetypes.typescriptreact").prettier },
		css = { require("formatter.filetypes.css").prettier },
		scss = { require("formatter.filetypes.css").prettier },
		cs = { require("formatter.filetypes.cs").dotnetformat },
		haskell = { brittany },
		rust = {
			require("formatter.filetypes.rust").rustfmt,
		},
		lua = {
			require("formatter.filetypes.lua").stylua,
		},
		cpp = {
			require("formatter.filetypes.cpp").clangformat,
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
  autocmd BufWritePost * FormatWrite
augroup END
]],
	true
)
