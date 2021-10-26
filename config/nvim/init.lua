-- Plugins
require("plugins")

-- General settings
require("settings")

-- Plugin Config
require("configuration")

local mapx = require "mapx"
-- Misc
mapx.nnoremap("<Leader>/", "<cmd>lua require'hop'.hint_words()<cr>")
mapx.nnoremap("<Leader>cc", ":ccl<CR>")

-- Project
mapx.nnoremap("<C-e>", ":CHADopen<cr>")
mapx.nnoremap("<leader>ff", "<cmd>Telescope find_files<cr>")
mapx.nnoremap("<leader><leader>", "<cmd>Telescope find_files<cr>")
mapx.nnoremap("<leader>sp", "<cmd>Telescope live_grep<cr>")
mapx.nnoremap("<leader>pp", "<cmd>Telescope projects<cr>")

-- Git
mapx.nnoremap("<leader>prc", "<cmd>Octo pr create<cr>")
mapx.nnoremap("<leader>prl", "<cmd>Octo pr list<cr>")
mapx.nnoremap("<leader>gb", ":GitBlameToggle<cr>")
mapx.nnoremap("<Leader>gg", ":LazyGit<cr>")

-- Nav
mapx.nnoremap("<leader>w", "<C-w>")
mapx.nnoremap("C-l", "<C-w>l")
mapx.nnoremap("C-k", "<C-w>k")
mapx.nnoremap("C-j", "<C-w>j")
mapx.nnoremap("C-h", "<C-w>h")

-- FileType
mapx.nnoremap("<leader>xx", "<Plug>RestNvim")

-- My own journalling
mapx.nnoremap(
  "<leader>nj",
  function()
    local filename = "~/Documents/wiki/journals/" .. os.date("%Y_%m_%d") .. ".md"
    vim.cmd("tabnew " .. filename)
  end
)
