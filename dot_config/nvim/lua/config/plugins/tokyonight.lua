return {
	"EdenEast/nightfox.nvim",
	config = function()
		require("nightfox").setup({
			options = {
				dim_inactive = true,
				modules = {
					diagnostic = {
						enabled = true,
					},
				},
			},
		})
		vim.api.nvim_command("colorscheme duskfox")
	end,
}
-- return {
-- 	"paulmaxwell/outrun-night.nvim",
-- 	config = function()
-- 		vim.api.nvim_command("colorscheme outrun-night")
-- 	end,
-- }
