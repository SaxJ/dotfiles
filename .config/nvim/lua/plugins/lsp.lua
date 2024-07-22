return {
	"neovim/nvim-lspconfig",
	dependencies = {
		"hrsh7th/nvim-cmp",
		"hrsh7th/cmp-nvim-lsp",
		"saadparwaiz1/cmp_luasnip",
		"L3MON4D3/LuaSnip",
	},
	config = function()
		local capabilities = require("cmp_nvim_lsp").default_capabilities()
		local lspconfig = require("lspconfig")

		local attach_keybinds = function()
			vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = true })
			vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, { buffer = true })
			vim.keymap.set("n", "<leader>cr", vim.lsp.buf.rename, { buffer = true })
			vim.keymap.set("n", "gr", vim.lsp.buf.references, { buffer = true })
			vim.keymap.set("n", "gs", vim.lsp.buf.signature_help, { buffer = true })
			vim.keymap.set("n", "gD", vim.lsp.buf.references, { buffer = true })
			vim.keymap.set("n", "gI", vim.lsp.buf.implementation, { buffer = true })
			vim.keymap.set("n", "K", vim.lsp.buf.hover, { buffer = true })
			vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { buffer = true })
			vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { buffer = true })
		end

		-- CONFIGURED SERVERS
		local servers = { "tsserver", "lua_ls", "intelephense" }
		for _, lsp in ipairs(servers) do
			lspconfig[lsp].setup({
				on_attach = attach_keybinds,
				capabilities = capabilities,
			})
		end

		-- luasnip setup
		local luasnip = require("luasnip")
		-- nvim-cmp setup
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
	end,
}
