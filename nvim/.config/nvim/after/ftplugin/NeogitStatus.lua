vim.keymap.set(
	"n",
	"<localleader>p",
	":! gh pr create --web<CR>",
	{ noremap = true, silent = true, buffer = true, desc = "Create PR" }
)
vim.keymap.set(
	"n",
	"@lp",
	":Octo pr list<CR>",
	{ noremap = true, silent = true, buffer = true, desc = "Create PR" }
)
