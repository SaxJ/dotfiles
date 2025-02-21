return {

	"neovim/nvim-lspconfig",
	event = { "BufReadPre", "BufNewFile" },
	dependencies = {
		"saghen/blink.cmp",
		-- {
		-- 	"folke/lazydev.nvim",
		-- 	ft = "lua",
		-- 	opts = {
		-- 		library = {
		-- 			{ path = "luvit-meta/library", words = { "vim%.uv" } },
		-- 		},
		-- 	},
		-- },
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

		require("lspconfig").lua_ls.setup({
			on_init = function(client)
				if client.workspace_folders then
					local path = client.workspace_folders[1].name
					if
						path ~= vim.fn.stdpath("config")
						and (vim.loop.fs_stat(path .. "/.luarc.json") or vim.loop.fs_stat(path .. "/.luarc.jsonc"))
					then
						return
					end
				end

				client.config.settings.Lua = vim.tbl_deep_extend("force", client.config.settings.Lua, {
					runtime = {
						-- Tell the language server which version of Lua you're using
						-- (most likely LuaJIT in the case of Neovim)
						version = "LuaJIT",
					},
					-- Make the server aware of Neovim runtime files
					workspace = {
						checkThirdParty = false,
						-- library = {
						-- 	vim.env.VIMRUNTIME,
						-- 	-- Depending on the usage, you might want to add additional paths here.
						-- 	-- "${3rd}/luv/library"
						-- 	-- "${3rd}/busted/library",
						-- },
						-- or pull in all of 'runtimepath'. NOTE: this is a lot slower and will cause issues when working on your own configuration (see https://github.com/neovim/nvim-lspconfig/issues/3189)
						library = vim.api.nvim_get_runtime_file("", true)
					},
				})
			end,
			settings = {
				Lua = {},
			},
		})
		nvim_lsp["intelephense"].setup({})
		nvim_lsp["csharp_ls"].setup({
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
		nvim_lsp["ocamllsp"].setup({
			cmd = { "opam", "exec", "--", "ocamllsp" },
			on_attach = on_attach,
		})
		nvim_lsp["templ"].setup({
			on_attach = on_attach,
		})
		nvim_lsp["gopls"].setup({
			on_attach = on_attach,
		})
	end,
}
