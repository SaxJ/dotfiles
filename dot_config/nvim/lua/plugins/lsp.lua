return {

	"neovim/nvim-lspconfig",
	event = { "BufReadPre", "BufNewFile" },
	dependencies = {
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
		local mason_lspconfig = require("mason-lspconfig")

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

		mason_lspconfig.setup_handlers({
			-- function(server)
			-- 	nvim_lsp[server].setup({})
			-- end,
			["lua_ls"] = nvim_lsp["lua_ls"].setup({}),
			["intelephense"] = nvim_lsp["intelephense"].setup({}),
			["omnisharp"] = function()
				nvim_lsp["omnisharp"].setup({
					cmd = { "omnisharp" },
					on_attach = on_attach,
				})
			end,
			["ts_ls"] = function()
				nvim_lsp["ts_ls"].setup({
					on_attach = on_attach,
				})
			end,
			["cssls"] = function()
				nvim_lsp["cssls"].setup({
					on_attach = on_attach,
				})
			end,
			["html"] = function()
				nvim_lsp["html"].setup({
					on_attach = on_attach,
				})
			end,
			["jsonls"] = function()
				nvim_lsp["jsonls"].setup({
					on_attach = on_attach,
				})
			end,
			["pyright"] = function()
				nvim_lsp["pyright"].setup({
					on_attach = on_attach,
				})
			end,
		})
	end,
}
