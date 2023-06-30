-- General settings
require("settings")

-- Plugins
require("config.lazy")

local wk = require("which-key")
local mapx = require("mapx")

local Terminal = require("toggleterm.terminal").Terminal
local mail = Terminal:new({
    dir = "~",
    cmd = "neomutt",
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

local insertDate = function ()
    local pos = vim.api.nvim_win_get_cursor(0)[2]
    local line = vim.api.nvim_get_current_line()
    local nline = line:sub(0, pos) .. os.date('%a, %Y-%m-%d') .. line:sub(pos + 1)
    vim.api.nvim_set_current_line(nline)
end

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
        d = { insertDate, "Date" },
    },
    o = {
        name = "+open",
        m = {
            function()
                mail:toggle()
            end,
            "Mail",
        },
        T = { ":terminal<CR>", "Terminal Full" },
        p = { ":NvimTreeToggle<cr>", "Project" },
        ["-"] = { require('oil').open, "Files" },
        t = {
            ":ToggleTerm size=20<cr>",
            "Terminal Popup",
        },
        g = {
            function()
                lazyGit:toggle()
            end,
            "Lazy Git",
        },
        r = { ":IronRepl<cr>", "Repl" },
    },
    ["<leader>"] = { ":Telescope find_files<cr>", "Recent Files" },
    f = {
        name = "+files",
        f = { "<cmd>Telescope file_browser hidden=true<cr>", "Files" },
        Y = { ":CopyRelPath<CR>", "Yank Path" },
    },
    s = {
        name = "+search",
        p = { "<cmd>Telescope live_grep hidden=true<cr>", "Search Project" },
    },
    p = {
        name = "+project",
        p = { ":Telescope projects<CR>", "Projects" },
        f = { ":Telescope file_browser hidden=true<CR>", "Project Files" },
    },
    g = {
        name = "+git",
        b = { ":ToggleBlameLine<cr>", "Blame" },
        B = { ":ToggleBlameLine<cr>", "Blame" },
        l = {
            function()
                lazyGit:toggle()
            end,
            "Lazy Git",
        },
        g = {
            ":Neogit<CR>",
            "Lazy Git",
        },
        f = {
            name = "+forge",
        },
    },
    t = {
        name = "Tasks",
        t = {
            function()
                tasks:toggle()
            end,
            "Toggle",
        },
    },
    n = {
        name = "+notes",
        j = {
            name = "+journal",
        },
    },
    r = {
        name = "+remote",
        u = { ":call SyncUploadFile()<cr>", "Upload" },
        d = { ":call SyncDownloadFile()<cr>", "Download" },
    },
    c = {
        name = "+code",
        a = { vim.lsp.buf.code_action, "Code Action" },
        r = { vim.lsp.buf.rename, "Rename" },
        g = { require("neogen").generate, "Generate Docs" },
    },
    ["."] = { ":Telescope file_browser path=%:p:h hidden=true<CR>", "Files" },
}, { prefix = "<leader>" })

-- misc
mapx.nnoremap("<C-e>", ":NvimTreeFindFileToggle<CR>")

-- Terminal
mapx.tnoremap("<Esc>", "<C-\\><C-n>")

-- FileType Specific --
-- Neorg
mapx.nmap("<localleader>d", insertDate, "silent", {ft = "norg"})

-- HTTP
mapx.nmap("<localleader>e", "<Plug>RestNvim", "silent", { ft = "http" })

mapx.nmap('<localleader>p', ":lua require('image_preview').PreviewImage()<CR>", 'silent', {ft = "NvimTree"})

-- Neogit
mapx.nmap("@cp", ":! gh pr create --fill -w<CR>", "silent", { ft = "NeogitStatus" })
