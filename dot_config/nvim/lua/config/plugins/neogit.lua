return {
	"TimUntersberger/neogit",
	dependencies = {
		{ "nvim-lua/plenary.nvim" },
	},
	config = function()
		require("neogit").setup({
			use_magit_keybindings = true,
			disable_commit_confirmation = true,
		})
	end,
}
