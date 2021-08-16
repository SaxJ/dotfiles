-- Plugins
require("plugins")

-- General settings
require("settings")

-- Plugin Config
require("configuration")

local vimp = require("vimp")
vimp.nnoremap("<Leader>w", "<C-w>")
vimp.nnoremap("<Leader>gg", ":Neogit<cr>")
vimp.nnoremap("<leader>ff", "<cmd>lua require('telescope.builtin').find_files()<cr>")
vimp.nnoremap("<leader><leader>", "<cmd>lua require('telescope.builtin').find_files()<cr>")
vimp.nnoremap("<leader>fg", "<cmd>lua require('telescope.builtin').live_grep()<cr>")
vimp.nnoremap("<leader>/", "<cmd>lua require'hop'.hint_words()<cr>")
vimp.nnoremap("<C-e>", ":NvimTreeToggle<cr>")
