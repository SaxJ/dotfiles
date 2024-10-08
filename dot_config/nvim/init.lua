local o = vim.opt

-- Editor options
o.number = true -- Print the line number in front of each line
o.relativenumber = true -- Show the line number relative to the line with the cursor in front of each line.
o.clipboard = "unnamedplus" -- uses the clipboard register for all operations except yank.
o.syntax = "on" -- When this option is set, the syntax with this name is loaded.
o.autoindent = true -- Copy indent from current line when starting a new line.
o.cursorline = true -- Highlight the screen line of the cursor with CursorLine.
o.expandtab = true -- In Insert mode: Use the appropriate number of spaces to insert a <Tab>.
o.shiftwidth = 2 -- Number of spaces to use for each step of (auto)indent.
o.tabstop = 2 -- Number of spaces that a <Tab> in the file counts for.
o.encoding = "UTF-8" -- Sets the character encoding used inside Vim.
o.ruler = true -- Show the line and column number of the cursor position, separated by a comma.
o.mouse = "a" -- Enable the use of the mouse. "a" you can use on all modes
o.title = true -- When on, the title of the window will be set to the value of 'titlestring'
o.hidden = true -- When on a buffer becomes hidden when it is |abandon|ed
o.ttimeoutlen = 0 -- The time in milliseconds that is waited for a key code or mapped key sequence to complete.
o.wildmenu = true -- When 'wildmenu' is on, command-line completion operates in an enhanced mode.
o.showcmd = true -- Show (partial) command in the last line of the screen. Set this option off if your terminal is slow.
o.showmatch = true -- When a bracket is inserted, briefly jump to the matching one.
o.inccommand = "split" -- When nonempty, shows the effects of :substitute, :smagic, :snomagic and user commands with the :command-preview flag as you type.
o.splitright = true
o.splitbelow = true -- When on, splitting a window will put the new window below the current one
o.termguicolors = true
o.wrap = false -- disable line wrapping
o.titlestring = "%{fnamemodify(getcwd(), ':t')} %m"
o.autoread = true

-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
	local lazyrepo = "https://github.com/folke/lazy.nvim.git"
	local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
	if vim.v.shell_error ~= 0 then
		vim.api.nvim_echo({
			{ "Failed to clone lazy.nvim:\n", "ErrorMsg" },
			{ out, "WarningMsg" },
			{ "\nPress any key to exit..." },
		}, true, {})
		vim.fn.getchar()
		os.exit(1)
	end
end
vim.opt.rtp:prepend(lazypath)

-- Make sure to setup `mapleader` and `maplocalleader` before
-- loading lazy.nvim so that mappings are correct.
-- This is also a good place to setup other settings (vim.opt)
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Setup lazy.nvim
require("lazy").setup({
	spec = {
		{ import = "plugins" },

		{ "voldikss/vim-floaterm" },
		-- optional for icons
		{ "nvim-tree/nvim-web-devicons" },
		{ "stevearc/dressing.nvim", opts = {} },
		{ "stevearc/overseer.nvim", opts = {} },
		{ "mistweaverco/kulala.nvim", opts = {} },
		{
			"danymat/neogen",
			config = true,
		},
	},
	-- Configure any other settings here. See the documentation for more details.
	-- colorscheme that will be used when installing plugins.
	install = { colorscheme = { "habamax" } },
	-- automatically check for plugin updates
	checker = { enabled = true },
})

-- Attach LSP keybinds
vim.api.nvim_create_autocmd("LspAttach", {
	desc = "LSP Actions",
	callback = function(event)
		vim.keymap.set("n", "K", vim.lsp.buf.hover, { desc = "Hover", buffer = event.buf })
		vim.keymap.set("n", "gd", vim.lsp.buf.definition, { desc = "Goto definition", buffer = event.buf })
		vim.keymap.set("n", "gD", vim.lsp.buf.references, { desc = "Goto definition", buffer = event.buf })
		vim.keymap.set("n", "gi", vim.lsp.buf.implementation, { desc = "Goto implementation", buffer = event.buf })
		vim.keymap.set("n", "gr", vim.lsp.buf.references, { desc = "Goto references", buffer = event.buf })
		vim.keymap.set("n", "gs", vim.lsp.buf.signature_help, { desc = "Signature help", buffer = event.buf })
		vim.keymap.set("n", "<leader>cr", vim.lsp.buf.rename, { desc = "Code rename", buffer = event.buf })
		vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, { desc = "Code actions", buffer = event.buf })
	end,
})

local function get_pass(pass_name)
	local handle = io.popen("pass " .. pass_name)
	if handle == nil then
		return "error"
	end

	local result = handle:read("*a")
	handle:close()
	return string.gsub(result, "^%s*(.-)%s*$", "%1")
end

local function get_jira_assigned()
	local auth_token = get_pass("jira")
	local payload = string.format('{"jql": "%s", "startAt": 0}', "")
	local url = "https://hejira.atlassian.net/rest/api/2/search"
	local curl_cmd = string.format(
		"curl -s -X POST -H 'Authorization: Bearer %s' -H 'Content-Type: application/json' -d '%s' '%s'",
		auth_token,
		payload,
		url
	)
	return curl_cmd
end

-- terminal
vim.keymap.set("t", "<Esc>", "<C-\\><C-n>")

-- general
vim.keymap.set("n", "<leader>/", ":Pick grep_live<CR>", { desc = "Grep" })
vim.keymap.set("n", "<leader>.", ":Pick files cwd=%:p:h<CR>", { desc = "Siblings" })
vim.keymap.set("n", "<leader><leader>", ":Pick files<CR>", { desc = "Files" })

-- files
vim.keymap.set("n", "<leader>f", "", { desc = "+files" })
vim.keymap.set("n", "<leader>ff", ":Pick files<CR>", { desc = "Find" })
vim.keymap.set("n", "<leader>fY", ':let @+ = expand("%")<CR>', { desc = "Yank Name" })

-- git
vim.keymap.set("n", "<leader>g", "", { desc = "+git" })
vim.keymap.set("n", "<leader>gg", ":Neogit<CR>", { desc = "Neogit" })
vim.keymap.set("n", "<leader>gb", ":Gitsigns blame<CR>", { desc = "Blame" })

-- project
vim.keymap.set("n", "<leader>p", "", { desc = "+project" })
vim.keymap.set("n", "<leader>pt", ":FloatermToggle<CR>", { desc = "Project Terminal" })

-- remote
vim.keymap.set("n", "<leader>r", "", { desc = "+remote" })
vim.keymap.set("n", "<leader>ru", ":RsyncUpFile<CR>", { desc = "Upload" })
vim.keymap.set("n", "<leader>rd", ":RsyncDownFile<CR>", { desc = "Download" })

-- open
vim.keymap.set("n", "<leader>o", "", { desc = "+open" })
vim.keymap.set("n", "<leader>o-", function()
	local files = require("mini.files")
	if not files.close() then
		files.open(vim.api.nvim_buf_get_name(0), false)
	end
end, { desc = "Files" })
vim.keymap.set("n", "<leader>od", ":Trouble diagnostics<CR>", { desc = "Diagnostics" })
vim.keymap.set("n", "<leader>ot", ":terminal<CR>", { desc = "Terminal" })

-- build system
vim.keymap.set("n", "<leader>ob", "", { desc = "+build" })
vim.keymap.set("n", "<leader>obs", ":OverseerToggle<CR>", { desc = "Status" })
vim.keymap.set("n", "<leader>obr", ":OverseerRun<CR>", { desc = "Run" })

-- neogit
vim.api.nvim_create_autocmd("FileType", {
	pattern = "NeogitStatus",
	callback = function()
		vim.keymap.set(
			"n",
			"<localleader>cp",
			":! gh pr create --web<CR>",
			{ noremap = true, silent = true, buffer = true }
		)
	end,
})

-- hurl
vim.api.nvim_create_autocmd("FileType", {
	pattern = "hurl",
	callback = function()
		vim.keymap.set("n", "<localleader>X", "<cmd>HurlRunner<CR>", { noremap = true, silent = true, buffer = true })
		vim.keymap.set("n", "<localleader>x", "<cmd>HurlRunnerAt<CR>", { noremap = true, silent = true, buffer = true })
	end,
})

-- json
vim.api.nvim_create_autocmd("FileType", {
	pattern = "json",
	callback = function()
		vim.keymap.set("n", "<localleader>jq", function()
			require("jq").run()
		end, { noremap = true, silent = true, buffer = true })
	end,
})
