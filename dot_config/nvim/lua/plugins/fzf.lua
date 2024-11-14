return {
	"ibhagwan/fzf-lua",
	-- optional for icon support
	dependencies = { "nvim-tree/nvim-web-devicons" },
	config = function()
		-- calling `setup` is optional for customization
		local actions = require("fzf-lua.actions")
		require("fzf-lua").setup({})
	end,
}
