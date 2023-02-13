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
		local lsp_capabilities = require('cmp_nvim_lsp').default_capabilities()
		local lsp_attach = function(client, bufnr)
			local bufopts = { noremap=true, silent=true, buffer=bufnr }
		  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
		  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
		  vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
		  vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
		end

		local lspconfig = require('lspconfig')
		require('mason-lspconfig').setup_handlers({
			function (server_name)
				lspconfig[server_name].setup({
					on_attach = lsp_attach,
					capabilities = lsp_capabilities
				})
			end
		})
	end,
}
