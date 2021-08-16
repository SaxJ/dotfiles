-- Plugins
require("plugins")

-- General settings
require("settings")

-- Plugin Config
require("configuration")

local vimp = require("vimp")
vimp.nnoremap("<Leader>/", "<cmd>lua require'hop'.hint_words()<cr>")
vimp.nnoremap("<Leader>gg", ":Neogit<cr>")
vimp.nnoremap("C-e", ":NvimTreeToggle<cr>")
vimp.nnoremap("<leader>ff", "<cmd>Telescope find_files<cr>")
vimp.nnoremap("<leader><leader>", "<cmd>Telescope find_files<cr>")
vimp.nnoremap("<leader>fg", "<cmd>Telescope live_grep<cr>")

-- Easy window nav
vimp.nnoremap("<leader>w", "<C-w>")
vimp.nnoremap("C-l", "<C-w>l")
vimp.nnoremap("C-k", "<C-w>k")
vimp.nnoremap("C-j", "<C-w>j")
vimp.nnoremap("C-h", "<C-w>h")
