-- Plugins
require("plugins")

-- General settings
require("settings")

-- Plugin Config
require("configuration")

local wk = require('which-key')
local mapx = require "mapx"
-- Misc
wk.register({
    o = {
        name = "open",
        m = { ":FloatermNew aerc<CR>", "Mail" },
        t = { ":FloatermNew<CR>", "Terminal" },
        p = { "NvimTreeToggle<cr>", "Project" },
    },
    ["<leader>"] = { "<cmd>Telescope find_files<cr>", "Recent Files" },
    ["<tab>"] = { "<cmd>Telescope buffers<cr>", "Buffers" },
    f = {
        name = "files",
        f = { "<cmd>Telescope find_files<cr>", "Files" },
        r = { "<cmd>Telescope frecency<cr>", "Recent" },
    },
    s = {
        name = "search",
        p = { "<cmd>Telescope live_grep<cr>", "Search Project" },
    },
    p = {
        name = "project",
        p = { "<cmd>Telescope repo list<cr>", "Projects" },
        t = { ":TodoTelescope<cr>", "Todos" },
    },
    g = {
        name = "git",
        c = { "<cmd>Octo pr create<cr>", "Create PR" },
        l = { "<cmd>Octo pr list<cr>", "List PRs" },
        b = { ":GitBlameToggle<cr>", "Blame" },
        g = { ":Neogit<cr>", "Git Commit" },
    },
    t = {
        name = "terminal",
        t = { ":FloatermToggle<cr>", "Terminal" },
        ["]"] = { ":FloatermNext<cr>", "Next Terminal" },
        ["["] = { ":FloatermPrev<cr>", "Prev Terminal" },
    },
    T = {
        name = 'tasks',
        t = { ":FloatermNew taskwarrior-tui<CR>", 'Show Tasks' },
    },
    n = {
        name = "notes",
        v = { ':Neorg gtd views<cr>', 'View Notes' },
        c = { ':Neorg gtd capture<cr>', 'Create Note' },
        j = {
            name = "journal",
            t = { ':Neorg journal today<cr>', 'Today' },
            y = { ':Neorg journal yesterday<cr>', 'Yesterday' },
            n = { ':Neorg journal tomorrow<cr>', 'Tomorrow' },
            c = { ':Neorg journal ', 'Create' },
        }
    }
}, { prefix = "<leader>" })

-- misc
mapx.nnoremap("<Leader>cc", ":ccl<CR>")
mapx.nnoremap("<C-e>", ":NvimTreeToggle<cr>")

-- Terminal
mapx.tnoremap('<C-[>', '<C-\\><C-n>')

-- Nav
mapx.nnoremap("<leader>w", "<C-w>")
mapx.nnoremap("C-l", "<C-w>l")
mapx.nnoremap("C-k", "<C-w>k")
mapx.nnoremap("C-j", "<C-w>j")
mapx.nnoremap("C-h", "<C-w>h")

-- FileType Specific --
-- HTTP
mapx.nmap("<localleader>e", "<Plug>RestNvim", "silent", { ft = "http" })
mapx.nmap('<localleader>d', ":put =strftime('%m/%d/%y')<cr>", "silent", { ft = "norg" })

-- autorun
vim.cmd [[silent! NeorgStart silent=true]]
