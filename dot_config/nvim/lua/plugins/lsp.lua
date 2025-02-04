return {

	"neovim/nvim-lspconfig",
	event = { "BufReadPre", "BufNewFile" },
	dependencies = {
		"saghen/blink.cmp",
		{
			"folke/lazydev.nvim",
			ft = "lua",
			opts = {
				library = {
					{ path = "luvit-meta/library", words = { "vim%.uv" } },
				},
			},
		},
		{
			"Bilal2453/luvit-meta",
			lazy = true,
		},
	},
	config = function()
		local nvim_lsp = require("lspconfig")

		local on_attach = function(client, bufnr)
			-- format on save
			if client.server_capabilities.documentFormattingProvider then
				vim.api.nvim_create_autocmd("BufWritePre", {
					group = vim.api.nvim_create_augroup("Format", { clear = true }),
					buffer = bufnr,
					callback = function()
						vim.lsp.buf.format()
					end,
				})
			end
		end

		nvim_lsp["lua_ls"].setup({})
		nvim_lsp["intelephense"].setup({})
		nvim_lsp["omnisharp"].setup({
			cmd = { "omnisharp" },
			on_attach = on_attach,
		})
		nvim_lsp["ts_ls"].setup({
			on_attach = on_attach,
		})
		nvim_lsp["cssls"].setup({
			on_attach = on_attach,
		})
		nvim_lsp["html"].setup({
			on_attach = on_attach,
		})
		nvim_lsp["jsonls"].setup({
			on_attach = on_attach,
		})
		nvim_lsp["pyright"].setup({
			on_attach = on_attach,
		})
    nvim_lsp['ocamllsp'].setup({
      cmd = {"opam", "exec", "--", "ocamllsp"},
      on_attach = on_attach,
    })
    nvim_lsp['templ'].setup({
      on_attach = on_attach,
    })
    nvim_lsp['go'].setup({
      on_attach = on_attach,
    })
	end,
}
