vim.keymap.del('n', 'grr')
vim.keymap.del('n', 'grn')
vim.keymap.del('n', 'gra')
vim.keymap.del('n', 'gri')

local funcs = require("functions")
local jira = require("jira")

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
o.updatetime = 1000

vim.diagnostic.config({
	virtual_text = {
		current_line = true,
	},
	virtual_lines = true,
	signs = true,
	update_in_insert = false,
	severity_sort = true,
	underline = true,
	float = {
		border = "rounded",
		severity_sort = true,
	},
})

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
		{
			"stevearc/overseer.nvim",
			config = function()
				local seer = require("overseer")
				seer.setup({})
				seer.register_template({
					name = "Run Unicron",
					builder = function()
						return {
							cmd = { "npm" },
							args = { "run", "dev", "--", "-p", "3000" },
							name = "Unicron",
							cwd = "/home/saxonj/Documents/unicron",
						}
					end,
					condition = {
						dir = "/home/saxonj/Documents/unicron",
					},
				})
			end,
		},
		{
			"airglow923/suda.nvim",
			config = function()
				require("suda").setup()
			end,
		},
		{ "mistweaverco/kulala.nvim", opts = {} },
		{ "danymat/neogen", config = true },
		{ "ii14/neorepl.nvim" },
		{
			"windwp/nvim-autopairs",
			event = "InsertEnter",
			config = true,
			-- use opts = {} for passing setup options
			-- this is equivalent to setup({}) function
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
		vim.keymap.set("n", "gr", vim.lsp.buf.references, { desc = "Goto references", buffer = event.buf, noremap = true, nowait = true })
		vim.keymap.set("n", "gs", vim.lsp.buf.signature_help, { desc = "Signature help", buffer = event.buf })
		vim.keymap.set("n", "<leader>cr", vim.lsp.buf.rename, { desc = "Code rename", buffer = event.buf })
		vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, { desc = "Code actions", buffer = event.buf })
	end,
})

-- SCP Functions
vim.g.scp_projects = {
	["megatron"] = "/home/ubuntu/megatron",
	["hannibal"] = "/home/ubuntu/hannibal",
	["unicron"] = "/home/ubuntu/unicron",
}

local get_project = function()
	local pwd = vim.uv.cwd() or ""
	return vim.fn.fnamemodify(pwd, ":t")
end

local scp_on_exit = function()
	vim.notify("SCP transfer complete", vim.log.levels.INFO)
end

local scp_upload = function()
	local project = get_project()
	local remote_path = vim.g.scp_projects[project]
	if remote_path == nil then
		vim.notify("Project not configured for SCP", vim.log.levels.ERROR)
		return
	end

	local local_relative_filename = vim.fn.expand("%:p:.")
	local scp_cmd = {
		"scp",
		local_relative_filename,
		string.format("ubuntu@minikube:%s/%s", remote_path, local_relative_filename),
	}

	vim.system(scp_cmd, nil, vim.schedule_wrap(scp_on_exit))
end

local scp_download = function()
	local project = get_project()
	local remote_path = vim.g.scp_projects[project]
	if remote_path == nil then
		vim.notify("Project not configured for SCP", vim.log.levels.ERROR)
		return
	end

	local local_relative_filename = vim.fn.expand("%:p:.")
	local scp_cmd = {
		"scp",
		string.format("ubuntu@minikube:%s/%s", remote_path, local_relative_filename),
		local_relative_filename,
	}

	vim.system(scp_cmd, nil, vim.schedule_wrap(scp_on_exit))
end

local open_file_browser = function()
	local files = require("mini.files")
	if not files.close() then
		files.open(vim.api.nvim_buf_get_name(0), false)
	end
end

local play_headers = function()
	local header_query = vim.treesitter.query.parse("markdown", "((atx_heading) @header)")
	local root = vim.treesitter.get_parser():parse()[1]:root()
	local cmd = { "mpv" }
	for _, node, _, _ in header_query:iter_captures(root, 0, 0, -1) do
		local text = vim.treesitter.get_node_text(node, 0)
		local clean_text = string.gsub(text, "#+%s+", "")
		table.insert(cmd, "ytdl://ytsearch:" .. clean_text)
	end

	vim.system(cmd, { detach = true })
end

-- terminal
vim.keymap.set("t", "<Esc>", "<C-\\><C-n>")

-- links
vim.keymap.set("n", "gx", function()
	local path = vim.fn.expand("<cfile>")
	if path:match("^[A-Za-z]+%-[0-9]+") then
		vim.ui.open(string.format("https://hejira.atlassian.net/browse/%s", path))
	else
		vim.ui.open(path)
	end
end, { desc = "Open Link" })

local fzf = require('fzf-lua')

-- general
vim.keymap.set("n", "<leader>/", fzf.live_grep, { desc = "Grep" })
vim.keymap.set("n", "<leader>sp", fzf.live_grep, { desc = "Grep" })
vim.keymap.set("n", "<leader>.", "<cmd>FzfLua files cwd=%:p:h<CR>", { desc = "Siblings" })
vim.keymap.set("n", "<leader>-", open_file_browser, { desc = "Files" })
vim.keymap.set("n", "<leader><leader>", fzf.files, { desc = "Files" })

-- buffers
vim.keymap.set("n", "<leader>b", "", { desc = "+buffer" })
vim.keymap.set("n", "<leader>bb", fzf.buffers, { desc = "Buffers" })
vim.keymap.set("n", "<leader>bf", function()
	require("conform").format({ lsp_fallback = true, async = false })
end, { desc = "Format Buffer" })

-- files
vim.keymap.set("n", "<leader>f", "", { desc = "+files" })
vim.keymap.set("n", "<leader>ff", "<cmd>FzfLua files cwd=%:p:h<CR>", { desc = "Find" })
vim.keymap.set("n", "<leader>fY", ':let @+ = expand("%")<CR>', { desc = "Yank Name" })

-- git
vim.keymap.set("n", "<leader>g", "", { desc = "+git" })
vim.keymap.set("n", "<leader>gg", function()
	require("neogit").open()
end, { desc = "Git" })
vim.keymap.set("n", "<leader>gb", ":Gitsigns blame<CR>", { desc = "Blame" })
vim.keymap.set("n", "<leader>gp", ":!gh pr create --web<CR>", { desc = "PR" })
vim.keymap.set("n", "<leader>gh", ":Tardis<CR>", { desc = "Timemachine" })

-- project
vim.keymap.set("n", "<leader>p", "", { desc = "+project" })
vim.keymap.set("n", "<leader>pp", fzf.zoxide, { desc = "Switch Project" })
vim.keymap.set("n", "<leader>pt", function()
	local pwd = vim.uv.cwd()
	vim.api.nvim_command(string.format("FloatermToggle '%s'", pwd))
end, { desc = "Project Terminal" })

-- remote
vim.keymap.set("n", "<leader>r", "", { desc = "+remote" })
vim.keymap.set("n", "<leader>ru", scp_upload, { desc = "Upload" })
vim.keymap.set("n", "<leader>rd", scp_download, { desc = "Download" })

-- open
vim.keymap.set("n", "<leader>o", "", { desc = "+open" })
vim.keymap.set("n", "<leader>o-", open_file_browser, { desc = "Files" })
vim.keymap.set("n", "<leader>od", ":Trouble diagnostics<CR>", { desc = "Diagnostics" })
vim.keymap.set("n", "<leader>ot", function()
	vim.api.nvim_command("FloatermNew --wintype=split --name=popup --autoclose=0 bash")
end, { desc = "Terminal" })
vim.keymap.set("n", "<leader>ob", "", { desc = "+build" })
vim.keymap.set("n", "<leader>obs", ":OverseerToggle<CR>", { desc = "Status" })
vim.keymap.set("n", "<leader>obr", ":OverseerRun<CR>", { desc = "Run" })
vim.keymap.set("n", "<leader>ok", function()
	require("kubectl").toggle({ tab = true })
end, { noremap = true, silent = true, desc = "Kubectl" })

vim.api.nvim_create_autocmd("VimEnter", {
	callback = function()
		funcs.util.log_work_date()
	end,
})

vim.api.nvim_create_autocmd({ "BufEnter", "BufWinEnter", "FocusGained" }, {
	pattern = { "*" },
	callback = function()
		vim.cmd("checktime")
	end,
})

vim.keymap.set("n", "<leader>jj", jira.jira_action)
vim.keymap.set("n", "<leader>jd", jira.jira_display_details)
