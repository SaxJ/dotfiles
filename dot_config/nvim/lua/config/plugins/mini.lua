return {
	"echasnovski/mini.nvim",
	config = function()
		-- General editing
		require("mini.ai").setup({})
		require("mini.align").setup({})
		require("mini.surround").setup({})
		require("mini.operators").setup({})
		require("mini.bracketed").setup({})
		require("mini.comment").setup({})

		-- Tools
		require("mini.files").setup({})
		require("mini.cursorword").setup({})
		require("mini.pairs").setup({})
		require("mini.git").setup({})
		require("mini.visits").setup({})
	end,
}
