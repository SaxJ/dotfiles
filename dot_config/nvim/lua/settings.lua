local cmd = vim.cmd
local indent = 4

-- Leader/local leader
vim.g.mapleader = [[ ]]
vim.g.maplocalleader = [[,]]
vim.g.do_filetype_lua = 1

vim.filetype.add({
	extension = {
		hurl = "hurl",
	},
})

cmd("command! CopyRelPath call setreg('+', expand('%'))")

vim.opt.autoread = true
vim.opt.autoindent = true
vim.opt.breakindent = true
vim.opt.completeopt = { "menuone", "noselect" }
vim.opt.copyindent = true
vim.opt.clipboard = "unnamedplus"
vim.opt.cmdheight = 1
vim.opt.cursorline = true
vim.opt.expandtab = true
vim.opt.hidden = true
vim.opt.ignorecase = true
vim.opt.inccommand = "split"
vim.opt.incsearch = true
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.scrolloff = 8
vim.opt.scrolloff = 8
vim.opt.shiftwidth = indent
vim.opt.softtabstop = indent
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.tabstop = indent
vim.opt.termguicolors = true
vim.opt.timeoutlen = 500
vim.opt.updatetime = 300
vim.opt.signcolumn = "yes"
vim.opt.title = true
vim.opt.titlestring = [[%{luaeval("vim.fn.getcwd()")}]]
vim.opt.sessionoptions =
	{ "blank", "buffers", "curdir", "folds", "help", "tabpages", "winsize", "winpos", "terminal", "localoptions" }
vim.opt.smartindent = true
vim.opt.smartcase = true
vim.opt.wildmenu = true
vim.opt.wildmode = { "longest", "list", "full" }
vim.opt.path:append({ "**" })
vim.opt.wildignore:append({
	"*.pyc",
	"*_build/*",
	"**/coverage/*",
	"**/node_modules/*",
	"**/android/*",
	"**/ios/*",
	"**/.git/*",
})

vim.diagnostic.config({
	virtual_text = false,
})
