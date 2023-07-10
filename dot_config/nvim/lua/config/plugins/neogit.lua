return {
	"NeogitOrg/neogit",
	dependencies = {
		{ "nvim-lua/plenary.nvim" },
		{ "sindrets/diffview.nvim"},
	},
	config = function()
		require("neogit").setup({
			use_magit_keybindings = true,
			disable_commit_confirmation = true,
			integrations = {
				diffview = true
			}
		})
	end,
}
