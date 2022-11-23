local cmd = vim.cmd
local indent = 4
local opt = vim.opt

-- Leader/local leader
vim.g.mapleader = [[ ]]
vim.g.maplocalleader = [[,]]

cmd("syntax enable")
cmd("filetype plugin indent on")
cmd("command! CopyRelPath call setreg('+', expand('%'))")

opt.autoindent = true
opt.breakindent = true
opt.completeopt = "menuone,noselect"
opt.copyindent = true
opt.clipboard = "unnamed,unnamedplus"
opt.cmdheight = 1
opt.cursorline = true
opt.expandtab = true
opt.hidden = true
opt.ignorecase = true
opt.inccommand = "split"
opt.number = true
opt.relativenumber = true
opt.scrolloff = 8
opt.scrolloff = 8
opt.shiftwidth = indent
opt.softtabstop = indent
opt.splitbelow = true
opt.splitright = true
opt.tabstop = indent
opt.termguicolors = true
opt.timeoutlen = 500
opt.updatetime = 300
opt.autoread = true

vim.o.sessionoptions = "blank,buffers,curdir,folds,help,tabpages,winsize,winpos,terminal"

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
