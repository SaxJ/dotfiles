-- General settings
require("settings")

-- Plugins
require("config.lazy")

local wk = require("which-key")
local mapx = require("mapx")

-- Misc
wk.register({
    b = {
        name = "+buffers",
        b = { ":Telescope buffers<cr>", "Buffers" },
        y = { ":%y+<CR>", "Yank" },
        f = {
            vim.lsp.buf.format,
            "Format",
        },
    },
    i = {
        name = "+insert",
        u = { ":r! uuidgen<CR>", "UUID" },
        d = { ":r! date<cr>", "Date" },
    },
    o = {
        name = "+open",
        T = { ":terminal<CR>", "Terminal Full" },
        d = { ":Trouble<CR>", "Diagnostics" },
        ["-"] = { ":Oil<CR>", "File Browser" },
    },
    f = {
        name = "+files",
        Y = { ":CopyRelPath<CR>", "Yank Path" },
    },
    s = {
        name = "+search",
        p = { ":Telescope live_grep hidden=true<cr>", "Search Project" },
    },
    g = {
        name = "+git",
        b = { ":ToggleBlameLine<cr>", "Blame" },
        g = { ":! zellij run -ci -- lazygit<CR>", "Git" },
        d = { ":Telescope git_status<CR>", "Changed Files" },
        h = {
            name = "+github",
            p = { ":!gh create pr --web<CR>", "Pull Request" },
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
        d = { require("neogen").generate, "Generate Docs" },
    },
    ["."] = { ":Telescope file_browser path=%:p:h hidden=true<CR>", "Files" },
    ["<leader>"] = { ":Telescope find_files find_command=rg,--ignore,--hidden,--files prompt_prefix=🔍<cr>", "Files" },
}, { prefix = "<leader>" })

mapx.nmap("<C-c>", ":ccl<CR>")
mapx.nmap("<localleader>t", ":terminal<CR>")

-- Terminal
mapx.tnoremap("<Esc>", "<C-\\><C-n>")
mapx.tnoremap("<C-k>", "<Up>")
mapx.tnoremap("<C-j>", "<Down>")

-- FileType Specific --
mapx.nmap("<localleader>r", ":HurlRunner<CR>", "silent", { ft = "hurl" })

mapx.inoremap("C-j", "<C-n>", "silent", { ft = "TelescopePrompt" })
mapx.inoremap("C-k", "<C-p>", "silent", { ft = "TelescopePrompt" })
