-- Plugins
require("plugins")

-- General settings
require("settings")

local wk = require("which-key")
local mapx = require("mapx")

local Terminal = require("toggleterm.terminal").Terminal
local mail = Terminal:new({
    dir = "~",
    cmd = "aerc",
    hidden = true,
    close_on_exit = true,
    direction = "float",
})
local tasks = Terminal:new({
    dir = "~",
    cmd = "taskwarrior-tui",
    hidden = true,
    close_on_exit = true,
    direction = "float",
})
local lazyGit = Terminal:new({
    cmd = "lazygit",
    hidden = true,
    direction = "tab",
})

-- Misc
wk.register({
    b = {
        name = "+buffers",
        b = { "<cmd>Telescope buffers<cr>", "Buffers" },
        y = { ":%y+<CR>", "Yank" },
        f = {
            function()
                vim.lsp.buf.format()
            end,
            "Format",
        },
    },
    i = {
        name = "+insert",
        u = { ":r! uuidgen<CR>", "UUID" },
    },
    o = {
        name = "open",
        m = {
            function()
                mail:toggle()
            end,
            "Mail",
        },
        T = { ":terminal<CR>", "Terminal" },
        p = { "NvimTreeToggle<cr>", "Project" },
        ["-"] = { ":Neotree<CR>", "Files" },
        t = {
            function()
                tasks:toggle()
            end,
            "Taskwarrior",
        },
    },
    ["<leader>"] = { "<cmd>Telescope find_files hidden=true<cr>", "Recent Files" },
    f = {
        name = "files",
        f = { "<cmd>Telescope file_browser hidden=true<cr>", "Files" },
        Y = { ":CopyRelPath<CR>", "Yank Path" },
    },
    s = {
        name = "search",
        p = { "<cmd>Telescope live_grep hidden=true<cr>", "Search Project" },
    },
    p = {
        name = "project",
        p = { ":Telescope projects<CR>", "Projects" },
        t = { ":TodoTelescope<cr>", "Todos" },
        f = { ":Telescope file_browser hidden=true<CR>", "Project Files" },
    },
    g = {
        name = "+git",
        b = { ":GitBlameToggle<cr>", "Blame" },
        B = { ":GitBlameToggle<cr>", "Blame" },
        g = { ":Neogit<cr>", "Git" },
        f = {
            name = "+forge",
            s = { ":Octo search assignee:SaxJ is:pr<CR>", "Search" },
            l = { ":Octo pr list<CR>", "List" },
        },
    },
    t = {
        name = "toggle",
        t = { ":ToggleTerm size=20<cr>", "Terminal" },
    },
    n = {
        name = "+notes",
    },
    r = {
        name = "+remote",
        u = { ":call SyncUploadFile()<cr>", "Upload" },
        d = { ":call SyncDownloadFile()<cr>", "Download" },
    },
    c = {
        name = "code",
        a = { ":Lspsaga code_action<CR>", "Action" },
        r = { ":Lspsaga rename<CR>", "Rename" },
        d = { ":Lspsaga show_line_diagnostics<CR>", "Diagnostic" },
        g = { ":lua require('neogen').generate()<CR>", "Generate Docs" },
    },
    ["."] = { ":Telescope file_browser path=%:p:h hidden=true<CR>", "Files" },
}, { prefix = "<leader>" })

-- misc
mapx.nnoremap("<C-e>", ":Neotree current %:p:h:h %:p<CR>")
local bufopts = { noremap = true, silent = true, buffer = bufnr }
vim.keymap.set("n", "gD", vim.lsp.buf.declaration, bufopts)
vim.keymap.set("n", "gd", vim.lsp.buf.definition, bufopts)
vim.keymap.set("n", "K", vim.lsp.buf.hover, bufopts)
vim.keymap.set("n", "gi", vim.lsp.buf.implementation, bufopts)
vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, bufopts)
vim.keymap.set("n", "gr", vim.lsp.buf.references, bufopts)

-- Terminal
mapx.tnoremap("<Esc>", "<C-\\><C-n>")

-- FileType Specific --
-- HTTP
mapx.nmap("<localleader>e", "<Plug>RestNvim", "silent", { ft = "http" })
mapx.nmap("<localleader>d", ":put =strftime('%m/%d/%y')<cr>", "silent", { ft = "norg" })
