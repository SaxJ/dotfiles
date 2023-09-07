local cmd = vim.cmd
local indent = 4

-- Leader/local leader
vim.g.mapleader = [[ ]]
vim.g.maplocalleader = [[,]]

cmd("syntax enable")
cmd("filetype plugin indent on")
cmd("command! CopyRelPath call setreg('+', expand('%'))")

vim.opt.autoindent = true
vim.opt.breakindent = true
vim.opt.completeopt = "menuone,noselect"
vim.opt.copyindent = true
vim.opt.clipboard = "unnamed,unnamedplus"
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
vim.opt.signcolumn = 'yes'

vim.o.sessionoptions = "blank,buffers,curdir,folds,help,tabpages,winsize,winpos,terminal,localoptions"

cmd([[
set expandtab smarttab shiftround autoindent smartindent smartcase nowrap
set path+=**
set wildmode=longest,list,full
set wildmenu
set wildignore+=*.pyc
set wildignore+=*_build/*
set wildignore+=**/coverage/*
set wildignore+=**/node_modules/*
set wildignore+=**/android/*
set wildignore+=**/ios/*
set wildignore+=**/.git/*
]])

vim.diagnostic.config({
    virtual_text = false
})

-- Autoread files when they're changed
vim.opt.autoread = true
vim.api.nvim_create_autocmd({'VimEnter', 'FocusGained', 'BufEnter'}, {
    group = vim.api.nvim_create_augroup('ReloadFileOnChange', {}),
    command = 'checktime'
})
