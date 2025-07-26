
		local configs = require("nvim-treesitter.configs")

		configs.setup({
			ensure_installed = {
				"c",
				"lua",
				"vim",
				"vimdoc",
				"query",
				"elixir",
				"heex",
				"javascript",
				"html",
				"php",
				"c_sharp",
				"typescript",
				"haskell",
				"elm",
			},
			sync_install = false,
			highlight = { enable = true },
			indent = { enable = true },
			modules = {},
			auto_install = true,
			ignore_install = {'org'},
		})
