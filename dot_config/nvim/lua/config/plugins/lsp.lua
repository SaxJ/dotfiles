return {
	"neovim/nvim-lspconfig",
	dependencies = {
		-- Autocompletion
		{ "hrsh7th/nvim-cmp" },
		{ "hrsh7th/cmp-buffer" },
		{ "hrsh7th/cmp-path" },
		{ "saadparwaiz1/cmp_luasnip" },
		{ "hrsh7th/cmp-nvim-lsp" },
		{ "hrsh7th/cmp-nvim-lua" },

		-- Snippets
		{ "L3MON4D3/LuaSnip" },
		{ "rafamadriz/friendly-snippets" },

		-- Additional tooling
		{ "nvim-lua/plenary.nvim" },
		{ "nvim-telescope/telescope.nvim" },
		{ "ray-x/lsp_signature.nvim" },

		-- Enhanced servers
		{ "Hoffs/omnisharp-extended-lsp.nvim" },
		{ "someone-stole-my-name/yaml-companion.nvim" },
	},
	config = function()
		local luasnip = require("luasnip")
		local cmp = require("cmp")

		vim.api.nvim_create_autocmd("LspAttach", {
			group = vim.api.nvim_create_augroup("UserLspConfig", {}),
			callback = function(client, bufnr)
				local bufopts = { noremap = true, silent = true, buffer = bufnr }

				vim.keymap.set("n", "gD", ":Telescope lsp_references<cr>", bufopts)
				vim.keymap.set("n", "gd", vim.lsp.buf.definition, bufopts)
				vim.keymap.set("n", "K", vim.lsp.buf.hover, bufopts)
				vim.keymap.set("n", "gI", ":Telescope lsp_implementations<cr>", bufopts)

				require("lsp_signature").on_attach({}, bufnr)
			end,
		})

		-- Lua LSP
		require("neodev").setup({})
		local yamlConfig = require("yaml-companion").setup({})

		cmp.setup({
			snippet = {
				expand = function(args)
					luasnip.lsp_expand(args.body)
				end,
			},
			mapping = cmp.mapping.preset.insert({
				["<C-u>"] = cmp.mapping.scroll_docs(-4), -- Up
				["<C-d>"] = cmp.mapping.scroll_docs(4), -- Down
				-- C-b (back) C-f (forward) for snippet placeholder navigation.
				["<C-Space>"] = cmp.mapping.complete(),
				["<CR>"] = cmp.mapping.confirm({
					behavior = cmp.ConfirmBehavior.Replace,
					select = true,
				}),
				["<Tab>"] = cmp.mapping(function(fallback)
					if cmp.visible() then
						cmp.select_next_item()
					elseif luasnip.expand_or_jumpable() then
						luasnip.expand_or_jump()
					else
						fallback()
					end
				end, { "i", "s" }),
				["<S-Tab>"] = cmp.mapping(function(fallback)
					if cmp.visible() then
						cmp.select_prev_item()
					elseif luasnip.jumpable(-1) then
						luasnip.jump(-1)
					else
						fallback()
					end
				end, { "i", "s" }),
			}),
			sources = {
				{ name = "nvim_lsp" },
				{ name = "luasnip" },
			},
		})

		local lspconfig = require("lspconfig")

		lspconfig.tsserver.setup({})
		lspconfig.intelephense.setup({
			init_options = {
				licenceKey = "/home/saxonj/intelephense/licence.txt",
			},
			settings = {
				intelephense = {
					runtime = "/usr/bin/node",
					maxMemory = 2000,
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
