vim.pack.add({
  "https://github.com/Bilal2453/luvit-meta",
  "https://github.com/MunifTanjim/nui.nvim",
  "https://github.com/NeogitOrg/neogit",
  "https://github.com/folke/trouble.nvim",
  "https://github.com/ibhagwan/fzf-lua",
  "https://github.com/ii14/neorepl.nvim",
  "https://github.com/jellydn/hurl.nvim",
  "https://github.com/kylechui/nvim-surround",
  "https://github.com/lewis6991/gitsigns.nvim",
  "https://github.com/manuuurino/autoread.nvim",
  "https://github.com/mason-org/mason.nvim",
  "https://github.com/neovim/nvim-lspconfig",
  "https://github.com/nvim-lua/plenary.nvim",
  "https://github.com/nvim-lualine/lualine.nvim",
  "https://github.com/nvim-orgmode/orgmode",
  "https://github.com/nvim-tree/nvim-web-devicons",
  "https://github.com/nvim-treesitter/nvim-treesitter",
  "https://github.com/sindrets/diffview.nvim",
  "https://github.com/stevearc/conform.nvim",
  "https://github.com/tiagovla/tokyodark.nvim",
  "https://github.com/yochem/jq-playground.nvim",
  'https://github.com/echasnovski/mini.nvim',
  'https://github.com/fredeeb/tardis.nvim',
  'https://github.com/mason-org/mason-lspconfig.nvim',
  'https://github.com/saghen/blink.cmp',
  'https://github.com/windwp/nvim-autopairs',
  'https://github.com/stevearc/oil.nvim',
})

require("nvim-surround").setup({})
require('tardis-nvim').setup({})
require("nvim-surround").setup({})
require('trouble').setup({})
vim.cmd("colorscheme tokyodark")
require("nvim-autopairs").setup({})
require('neogit').setup({ graph_style = 'kitty' })

vim.keymap.del("n", "grr")
vim.keymap.del("n", "grn")
vim.keymap.del("n", "gra")
vim.keymap.del("n", "gri")

local funcs = require("functions")

local o = vim.opt

-- Editor options
o.number = true             -- Print the line number in front of each line
o.relativenumber = true     -- Show the line number relative to the line with the cursor in front of each line.
o.clipboard = "unnamedplus" -- uses the clipboard register for all operations except yank.
o.syntax = "on"             -- When this option is set, the syntax with this name is loaded.
o.autoindent = true         -- Copy indent from current line when starting a new line.
o.cursorline = true         -- Highlight the screen line of the cursor with CursorLine.
o.expandtab = true          -- In Insert mode: Use the appropriate number of spaces to insert a <Tab>.
o.shiftwidth = 2            -- Number of spaces to use for each step of (auto)indent.
o.tabstop = 2               -- Number of spaces that a <Tab> in the file counts for.
o.encoding = "UTF-8"        -- Sets the character encoding used inside Vim.
o.ruler = true              -- Show the line and column number of the cursor position, separated by a comma.
o.mouse = "a"               -- Enable the use of the mouse. "a" you can use on all modes
o.title = true              -- When on, the title of the window will be set to the value of 'titlestring'
o.hidden = true             -- When on a buffer becomes hidden when it is |abandon|ed
o.ttimeoutlen = 0           -- The time in milliseconds that is waited for a key code or mapped key sequence to complete.
o.wildmenu = true           -- When 'wildmenu' is on, command-line completion operates in an enhanced mode.
o.showcmd = true            -- Show (partial) command in the last line of the screen. Set this option off if your terminal is slow.
o.showmatch = true          -- When a bracket is inserted, briefly jump to the matching one.
o.inccommand =
"split"                     -- When nonempty, shows the effects of :substitute, :smagic, :snomagic and user commands with the :command-preview flag as you type.
o.splitright = true
o.splitbelow = true         -- When on, splitting a window will put the new window below the current one
o.termguicolors = true
o.wrap = false              -- disable line wrapping
o.titlestring = "%{fnamemodify(getcwd(), ':t')} %m"
o.autoread = true
o.updatetime = 500
o.completeopt = { "menuone", "noselect", "noinsert" }
o.shortmess:append "c"
o.exrc = true

vim.diagnostic.config({
  virtual_text = false,
  virtual_lines = false,
  signs = true,
  update_in_insert = false,
  severity_sort = true,
  underline = true,
  float = {
    border = "rounded",
    severity_sort = true,
  },
})

-- Make sure to setup `mapleader` and `maplocalleader` before
-- loading lazy.nvim so that mappings are correct.
-- This is also a good place to setup other settings (vim.opt)
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Attach LSP keybinds
vim.api.nvim_create_autocmd("LspAttach", {
  desc = "LSP Actions",
  callback = function(event)
    vim.keymap.set("n", "K", vim.lsp.buf.hover, { desc = "Hover", buffer = event.buf })
    vim.keymap.set("n", "gd", vim.lsp.buf.definition, { desc = "Goto definition", buffer = event.buf })
    vim.keymap.set("n", "gD", vim.lsp.buf.references, { desc = "Goto definition", buffer = event.buf })
    vim.keymap.set("n", "gi", vim.lsp.buf.implementation, { desc = "Goto implementation", buffer = event.buf })
    vim.keymap.set(
      "n",
      "gr",
      vim.lsp.buf.references,
      { desc = "Goto references", buffer = event.buf, noremap = true, nowait = true }
    )
    vim.keymap.set("n", "gs", vim.lsp.buf.signature_help, { desc = "Signature help", buffer = event.buf })
    vim.keymap.set("n", "<leader>cr", vim.lsp.buf.rename, { desc = "Code rename", buffer = event.buf })
    vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, { desc = "Code actions", buffer = event.buf })

    vim.keymap.set('n', '<leader>lr', ":LspRestart<CR>", { desc = "Restart LS" })
  end,
})

vim.g.timelog_file = "/home/saxonj/time/timelog"

-- terminal
vim.keymap.set("t", "<Esc>", "<C-\\><C-n>")
vim.keymap.set('t', '<C-k>', '<up>')
vim.keymap.set('t', '<C-j>', '<down>')

-- links
vim.keymap.set("n", "gx", function()
  local path = vim.fn.expand("<cfile>")
  if path:match("^[A-Za-z]+%-[0-9]+") then
    vim.ui.open(string.format("https://hejira.atlassian.net/browse/%s", path))
  else
    vim.ui.open(path)
  end
end, { desc = "Open Link" })

-- general
vim.keymap.set("n", "<leader>/", ":FzfLua live_grep<CR>", { desc = "Grep" })
vim.keymap.set("n", "<leader>sp", ":FzfLua live_grep<CR>", { desc = "Grep" })
vim.keymap.set("n", "<leader><leader>", ":FzfLua files<CR>", { desc = "Files" })
vim.keymap.set('n', '<leader>-', ':Oil<CR>', { desc = 'File browser' })
vim.keymap.set('n', '<leader>.', function()
  local cwd = vim.fn.expand('%:p:h')
  vim.cmd(string.format("FzfLua files cwd='%s'", cwd))
end, { desc = "Siblings" })

-- buffers
vim.keymap.set("n", "<leader>b", "", { desc = "+buffer" })
vim.keymap.set("n", "<leader>bb", ":FzfLua buffers<CR>", { desc = "Buffers" })
vim.keymap.set("n", "<leader>bf", function()
  require("conform").format({ lsp_fallback = true, async = false })
end, { desc = "Format Buffer" })
vim.keymap.set('n', '<leader>bt', function()
  require('fzf-lua').buffers({
    opts = {
      filter = function(b)
        return b ~= require('fzf-lua.core').CTX().bufnr and not require("fzf-lua.utils").is_term_buffer(b)
      end
    }
  })
end, { desc = "Terminal buffers" })

-- files
vim.keymap.set("n", "<leader>f", "", { desc = "+files" })
vim.keymap.set("n", "<leader>fY", function()
  vim.cmd('let @+ = expand("%")')
  print("Yanked file name.")
end, { desc = "Yank Name" })

-- git
vim.keymap.set("n", "<leader>g", "", { desc = "+git" })
vim.keymap.set("n", "<leader>gg", ":Neogit<CR>", { desc = "Git Status" })
vim.keymap.set("n", "<leader>gd", ":FzfLua git_diff<CR>", { desc = "Git Status" })
vim.keymap.set("n", "<localleader>gb", ":Gitsigns blame<CR>", { desc = "Blame" })
vim.keymap.set("n", "<leader>gh", ":Tardis<CR>", { desc = "Timemachine" })

-- project
vim.keymap.set("n", "<leader>p", "", { desc = "+project" })
vim.keymap.set("n", "<leader>pt", ":VTerm<CR>i", { desc = "Project Terminal" })

-- remote
vim.keymap.set("n", "<leader>r", "", { desc = "+remote" })
vim.keymap.set("n", "<leader>ru", ":ScpUpload<CR>", { desc = "Upload" })
vim.keymap.set("n", "<leader>rd", ":ScpDownload<CR>", { desc = "Download" })

-- open
vim.keymap.set("n", "<leader>o", "", { desc = "+open" })
vim.keymap.set("n", "<leader>od", ":Trouble diagnostics<CR>", { desc = "Diagnostics" })
vim.keymap.set("n", "<leader>ot", ":HTerm<CR>i", { desc = "Terminal" })

vim.api.nvim_create_autocmd("VimEnter", {
  callback = function()
    funcs.util.log_work_date()
  end,
})

function Diag_if_no_float()
  for _, winid in pairs(vim.api.nvim_tabpage_list_wins(0)) do
    if vim.api.nvim_win_get_config(winid).zindex then
      return
    end
  end
  vim.diagnostic.open_float({
    scope = "cursor",
    focusable = true,
    close_events = {
      "CursorMoved",
      "CursorMovedI",
      "BufHidden",
      "InsertCharPre",
      "WinLeave",
    },
  })
end

vim.api.nvim_create_augroup("lsp_diagnostics_hold", { clear = true })
vim.api.nvim_create_autocmd("CursorHold", {
  pattern = "*",
  callback = Diag_if_no_float,
})

vim.api.nvim_create_user_command("FileToBranch", function(args)
  local local_relative_filename = vim.fn.expand("%:p:.")
  local branch = args['args']

  local result = vim.system({ 'git', 'checkout', branch, '--', local_relative_filename }, { text = true }):wait()
  if result.code == 0 then
    vim.fn.printf("Reverted file to %s", branch)
  else
    error(result.stdout)
  end
end, {
  desc = "Revert a file to the version on the given branch.",
  nargs = 1,
  complete = function(lead, _, _)
    vim.fn.printf(lead)
    local branches = vim.system(
          { 'git', 'branch', '--list', '--all', '--format=%(refname:short)', '-i', string.format("*%s*", lead) },
          { text = true })
        :wait()
    if branches.code == 0 then
      return vim.split(branches.stdout, '\n')
    else
      return {}
    end
  end
})
