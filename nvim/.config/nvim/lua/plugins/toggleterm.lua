return {
	"akinsho/toggleterm.nvim",
	version = "*",
	config = function()
		require("toggleterm").setup({
			open_mapping = "<leader>ot",
      insert_mappings = false,
      terminal_mappings = false,
      auto_scroll = false,
			size = function(term)
				if term.direction == "horizontal" then
					return vim.o.lines * 0.35
				elseif term.direction == "vertical" then
					return vim.o.columns * 0.4
				end
			end,
		})
	end,
}
