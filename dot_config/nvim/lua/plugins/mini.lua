return {
	"echasnovski/mini.nvim",
	version = false,
	config = function()
		require("mini.ai").setup()
		require("mini.basics").setup()
		require("mini.files").setup()
		require("mini.surround").setup()
		require("mini.icons").setup()
		require("mini.completion").setup()
		require("mini.pairs").setup()

		local icons = require("mini.icons")
		icons.tweak_lsp_kind()

		local imap_expr = function(lhs, rhs)
			vim.keymap.set("i", lhs, rhs, { expr = true })
		end
		imap_expr("<Tab>", [[pumvisible() ? "\<C-n>" : "\<Tab>"]])
		imap_expr("<S-Tab>", [[pumvisible() ? "\<C-p>" : "\<S-Tab>"]])
	end,
}
