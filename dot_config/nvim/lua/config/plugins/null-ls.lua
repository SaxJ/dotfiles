return {
	"nvimtools/none-ls.nvim",
	config = function()
		require("null-ls").setup({
			sources = {
				require("null-ls").builtins.formatting.stylua,
				require('null-ls').builtins.formatting.prettier,
				require("null-ls").builtins.formatting.phpcsfixer,
			},
		})
	end,
}
