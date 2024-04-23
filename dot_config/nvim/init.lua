-- General settings
require("settings")

-- Plugins
require("config.lazy")

local wk = require("which-key")
local mapx = require("mapx")

local project_terminal_toggle = function()
    local current_project = require("projectmgr.file_adapter").get_current_project()
    local size = vim.o.columns * 0.5
    local terminal_cmd =
        string.format([[ToggleTerm direction=vertical size=%s name="project:%s"<CR>]], size, current_project)
    vim.cmd(terminal_cmd)
end

local capture_todo = function()
    local todo_text = vim.fn.input("> ")
    local cmd = ([[todo.sh add "%s"]]).format(todo_text)
    vim.cmd(("! %s").format(cmd))
end

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
        f = { ":Oil<cr>", "Files" },
        Y = { ":CopyRelPath<CR>", "Yank Path" },
    },
    s = {
        name = "+search",
        p = { ":Telescope live_grep hidden=true<cr>", "Search Project" },
    },
    p = {
        name = "+project",
        p = { ":ProjectMgr<cr>", "Switch Project" },
        t = { project_terminal_toggle, "Project Terminal" },
    },
    g = {
        name = "+git",
        b = { ":ToggleBlameLine<cr>", "Blame" },
        g = { ":Neogit<CR>", "Git" },
        d = { ":Telescope git_status<CR>", "Changed Files" },
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
        d = { require("neogen").generate, "Generate Docs" },
    },
    t = {
        name = "+terminal",
        t = { ":Telescope toggleterm_manager<CR>", "Terminal" },
    },
    ["."] = { ":Telescope file_browser path=%:p:h hidden=true<CR>", "Files" },
    ["<leader>"] = { ":Telescope find_files find_command=rg,--ignore,--hidden,--files prompt_prefix=🔍<cr>", "Files" },
}, { prefix = "<leader>" })

mapx.nmap("<C-c>", ":ccl<CR>")
mapx.nmap("<C-t>", ":terminal<CR>")

-- Terminal
mapx.tnoremap("<Esc>", "<C-\\><C-n>")
mapx.tnoremap("<C-k>", "<Up>")
mapx.tnoremap("<C-j>", "<Down>")

-- FileType Specific --
mapx.nmap("<localleader>r", ":HurlRunner<CR>", "silent", { ft = "hurl" })

mapx.inoremap("C-j", "<C-n>", "silent", { ft = "TelescopePrompt" })
mapx.inoremap("C-k", "<C-p>", "silent", { ft = "TelescopePrompt" })
