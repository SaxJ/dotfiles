return {
	"VonHeikemen/lsp-zero.nvim",
	dependencies = {
		-- LSP Support
		{ "neovim/nvim-lspconfig" },
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
		require("lsp_signature").setup({})

		local lsp = require("lsp-zero")

		lsp.preset("recommended")

		local cmp = require("cmp")
		local cmp_mappings = lsp.defaults.cmp_mappings({
			["<C-Space>"] = cmp.mapping.complete(),
			["<C-e>"] = cmp.mapping.abort(),
		})
		lsp.setup_nvim_cmp({
			mapping = cmp_mappings,
		})

		lsp.setup()
	end,
}
