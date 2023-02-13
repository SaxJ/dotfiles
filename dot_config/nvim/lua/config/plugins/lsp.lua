return {
	"neovim/nvim-lspconfig",
	dependencies = {
		-- LSP Support
		{ "williamboman/mason.nvim" },
		{ "williamboman/mason-lspconfig.nvim" },

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
		{ "MrcJkb/haskell-tools.nvim" },
		{ "nvim-lua/plenary.nvim" },
		{ "nvim-telescope/telescope.nvim" },
		{ "ray-x/lsp_signature.nvim" },
	},
	config = function()
		local lsp_capabilities = require("cmp_nvim_lsp").default_capabilities()
		local lsp_attach = function(client, bufnr)
			local bufopts = { noremap = true, silent = true, buffer = bufnr }

			vim.keymap.set("n", "gD", vim.lsp.buf.declaration, bufopts)
			vim.keymap.set("n", "gd", vim.lsp.buf.definition, bufopts)
			vim.keymap.set("n", "K", vim.lsp.buf.hover, bufopts)
			vim.keymap.set("n", "gi", vim.lsp.buf.implementation, bufopts)
		end

		local luasnip = require("luasnip")
		local cmp = require("cmp")
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
		require('mason').setup()
		require("mason-lspconfig").setup_handlers({
			function(server_name)
				lspconfig[server_name].setup({
					on_attach = lsp_attach,
					capabilities = lsp_capabilities,
				})
			end,
		})
	end,
}
