-- Plugins
require("plugins")

-- General settings
require("settings")

-- Plugin Config
require("configuration")

local vimp = require("vimp")
vimp.nnoremap("<Leader>/", "<cmd>lua require'hop'.hint_words()<cr>")
vimp.nnoremap("<Leader>gg", ":LazyGit<cr>")
vimp.nnoremap("<C-e>", ":NvimTreeToggle<cr>")
vimp.nnoremap("<leader>ff", "<cmd>Telescope find_files<cr>")
vimp.nnoremap("<leader><leader>", "<cmd>Telescope find_files<cr>")
vimp.nnoremap("<leader>sp", "<cmd>Telescope live_grep<cr>")
vimp.nnoremap("<leader>pp", "<cmd>Telescope projects<cr>")

-- Easy window nav
vimp.nnoremap("<leader>w", "<C-w>")
vimp.nnoremap("C-l", "<C-w>l")
vimp.nnoremap("C-k", "<C-w>k")
vimp.nnoremap("C-j", "<C-w>j")
vimp.nnoremap("C-h", "<C-w>h")

-- My own journalling
vimp.nnoremap(
  "<leader>nj",
  function()
    local filename = "~/Documents/wiki/journals/" .. os.date("%Y_%m_%d") .. ".md"
    vim.cmd("tabnew " .. filename)
  end
)