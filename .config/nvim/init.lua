vim.g.mapleader = " "
vim.g.maplocalleader = ","

require("config.lazy")

--- Colourscheme
vim.cmd("colorscheme neon")

local opt = vim.opt
opt.cursorline = true
opt.termguicolors = true -- enabled 24bit RGB color
opt.signcolumn = "yes" -- always draw the sign column
opt.updatetime = 50 -- update time for the swap file and for the cursorHold event
opt.colorcolumn = "80" -- colorized the 80th column
opt.clipboard:append({ "unnamedplus" }) -- force to use the clipboard for all the operations
-- BACKUP
opt.backup = false -- no backup of the current file is made
-- FILE LINES
opt.number = true -- show line number
opt.relativenumber = true -- set line number format to relative
opt.wrap = false -- wrap lines
opt.scrolloff = 8 -- min nb of line around your cursor (8 above, 8 below)
-- INDENT
opt.smartindent = true -- try to be smart w/ indent
opt.autoindent = true -- indent new line the same amount as the line before
opt.shiftwidth = 2 -- width for autoindents
-- TAB
opt.expandtab = true -- converts tabs to white space
opt.tabstop = 2 -- nb of space for a tab in the file
opt.softtabstop = 2 -- nb of space for a tab in editing operations
-- SEARCH
opt.ignorecase = true -- case insensitive UNLESS /C or capital in search
opt.hlsearch = true -- highlight all the result found
opt.incsearch = true -- incremental search (show result on live)
opt.wildignore:append({ "*/node_modules/*", "*/vendor/*" }) -- the search ignore this folder
-- CONTEXTUAL
opt.title = true -- set the title of the window automaticaly, usefull for tabs plugin
opt.path:append({ "**" }) -- search (gf or :find) files down into subfolders
