return {
	"RaafatTurki/corn.nvim",
	config = function()
		require("corn").setup({
			item_preprocess_func = function(item)
				return item
			end,
		})
	end,
}
