return {
	"echasnovski/mini.nvim",
	version = false,
	config = function()
		require("mini.ai").setup()
		require("mini.basics").setup()
		require("mini.files").setup()
		require("mini.surround").setup()
	end,
}