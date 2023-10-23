return {
	"EdenEast/nightfox.nvim",
	config = function()
		require('nightfox').setup({
			options = {
				dim_inactive = true,
				modules = {
					diagnostic = {
						enabled = true
					}
				}
			},
		})
		vim.api.nvim_command("colorscheme duskfox")
	end,
}
