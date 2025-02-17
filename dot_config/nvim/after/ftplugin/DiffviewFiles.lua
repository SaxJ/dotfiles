vim.keymap.set(
	"n",
	"q",
	":tabclose<CR>",
	{ noremap = true, silent = true, buffer = true, desc = "Exit diff" }
)
