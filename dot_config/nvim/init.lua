-- Plugins
require("plugins")

-- General settings
require("settings")

-- Plugin Config
require("configuration")

local wk = require('which-key')
local mapx = require "mapx"

local Terminal = require('toggleterm.terminal').Terminal
local lazyGit = Terminal:new({
    cmd = "lazygit",
    dir = "git_dir",
    hidden = true,
    close_on_exit = true,
    direction = 'float',
})
local mail = Terminal:new({
    dir = '~',
    cmd = 'aerc',
    hidden = true,
    close_on_exit = true,
    direction = 'float',
})
local floatingTerminal = Terminal:new({
    direction = 'float'
})

-- Misc
wk.register({
    b = {
        name = 'buffers',
        b = { "<cmd>Telescope buffers<cr>", "Buffers" }
    },
    o = {
        name = "open",
        m = { function() mail:toggle() end, "Mail" },
        T = { function() floatingTerminal:toggle() end, "Terminal" },
        p = { "NvimTreeToggle<cr>", "Project" },
        ['-'] = { ':NnnPicker %:p:h<CR>', 'Files' }
    },
    ["<leader>"] = { "<cmd>Telescope find_files<cr>", "Recent Files" },
    ["<tab>"] = { "<cmd>Telescope buffers<cr>", "Buffers" },
    f = {
        name = "files",
        f = { "<cmd>Telescope find_files<cr>", "Files" },
    },
    s = {
        name = "search",
        p = { "<cmd>Telescope live_grep<cr>", "Search Project" },
    },
    p = {
        name = "project",
        p = { ":Telescope projects<cr>", "Projects" },
        t = { ":TodoTelescope<cr>", "Todos" },
        m = { ":Telescope harpoon marks<cr>", "Marks" }
    },
    g = {
        name = "git",
        c = { "<cmd>Octo pr create<cr>", "Create PR" },
        l = { "<cmd>Octo pr list<cr>", "List PRs" },
        b = { ":GitBlameToggle<cr>", "Blame" },
        g = { function() lazyGit:toggle() end, "LazyGit" },
    },
    t = {
        name = "toggle",
        t = { ":ToggleTerm size=30<cr>", "Terminal" },
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
mapx.nnoremap("<C-e>", ":NnnExplorer %:p:h<CR>")

-- Terminal
mapx.tnoremap('<Esc>', '<C-\\><C-n>')

-- FileType Specific --
-- HTTP
mapx.nmap("<localleader>e", "<Plug>RestNvim", "silent", { ft = "http" })
mapx.nmap('<localleader>d', ":put =strftime('%m/%d/%y')<cr>", "silent", { ft = "norg" })

-- autorun
vim.cmd [[silent! NeorgStart silent=true]]
