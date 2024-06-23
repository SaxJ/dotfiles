return {
	"neovim/nvim-lspconfig",
	dependencies = {
		-- Additional tooling
		{ "nvim-lua/plenary.nvim" },
		{ "nvim-telescope/telescope.nvim" },

		-- Enhanced servers
		{ "Hoffs/omnisharp-extended-lsp.nvim" },
	},
	config = function()
		vim.api.nvim_create_autocmd("LspAttach", {
			group = vim.api.nvim_create_augroup("UserLspConfig", {}),
			callback = function(client, bufnr)
				local bufopts = { noremap = true, silent = true, buffer = bufnr }

				vim.keymap.set("n", "gD", ":Telescope lsp_references<cr>", bufopts)
				vim.keymap.set("n", "gd", vim.lsp.buf.definition, bufopts)
				vim.keymap.set("n", "K", vim.lsp.buf.hover, bufopts)
				vim.keymap.set("n", "gI", ":Telescope lsp_implementations<cr>", bufopts)
			end,
		})

		-- Lua LSP
		require("neodev").setup({})

		local lspconfig = require("lspconfig")

		lspconfig.tsserver.setup({})
		lspconfig.intelephense.setup({
			init_options = {
				licenceKey = "/home/saxonj/intelephense/licence.txt",
			},
			settings = {
				intelephense = {
					runtime = "/usr/bin/bun",
					maxMemory = 8000,
					environment = {
						phpVersion = "8.2.0",
					},
					telemetry = {
						enabled = false,
					},
				},
			},
		})
		lspconfig.omnisharp.setup({})
		lspconfig.bashls.setup({})
		lspconfig.hls.setup({})
		lspconfig.lua_ls.setup({})
	end,
}
