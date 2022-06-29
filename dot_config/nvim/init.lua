-- Plugins
require("plugins")

-- General settings
require("settings")

-- Plugin Config
require("configuration")

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
local lazygit = Terminal:new({
	cmd = "lazygit",
	hidden = true,
	close_on_exit = true,
	direction = "float",
})
local floatingTerminal = Terminal:new({
	direction = "float",
})

-- Misc
wk.register({
	b = {
		name = "buffers",
		b = { "<cmd>Telescope buffers<cr>", "Buffers" },
	},
	o = {
		name = "open",
		m = {
			function()
				mail:toggle()
			end,
			"Mail",
		},
		T = {
			function()
				floatingTerminal:toggle()
			end,
			"Terminal",
		},
		p = { "NvimTreeToggle<cr>", "Project" },
		["-"] = { ":Neotree<CR>", "Files" },
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
		m = { ":Telescope harpoon marks<cr>", "Marks" },
	},
	g = {
		name = "git",
		b = { ":GitBlameToggle<cr>", "Blame" },
		g = {
			function()
				lazygit:toggle()
			end,
			"Open Git",
		},
		h = {
			name = "+Github",
			c = {
				name = "+Commits",
				c = { "<cmd>GHCloseCommit<cr>", "Close" },
				e = { "<cmd>GHExpandCommit<cr>", "Expand" },
				o = { "<cmd>GHOpenToCommit<cr>", "Open To" },
				p = { "<cmd>GHPopOutCommit<cr>", "Pop Out" },
				z = { "<cmd>GHCollapseCommit<cr>", "Collapse" },
			},
			i = {
				name = "+Issues",
				p = { "<cmd>GHPreviewIssue<cr>", "Preview" },
			},
			l = {
				name = "+Litee",
				t = { "<cmd>LTPanel<cr>", "Toggle Panel" },
			},
			r = {
				name = "+Review",
				b = { "<cmd>GHStartReview<cr>", "Begin" },
				c = { "<cmd>GHCloseReview<cr>", "Close" },
				d = { "<cmd>GHDeleteReview<cr>", "Delete" },
				e = { "<cmd>GHExpandReview<cr>", "Expand" },
				s = { "<cmd>GHSubmitReview<cr>", "Submit" },
				z = { "<cmd>GHCollapseReview<cr>", "Collapse" },
			},
			p = {
				name = "+Pull Request",
				c = { "<cmd>GHClosePR<cr>", "Close" },
				d = { "<cmd>GHPRDetails<cr>", "Details" },
				e = { "<cmd>GHExpandPR<cr>", "Expand" },
				o = { "<cmd>GHOpenPR<cr>", "Open" },
				p = { "<cmd>GHPopOutPR<cr>", "PopOut" },
				r = { "<cmd>GHRefreshPR<cr>", "Refresh" },
				t = { "<cmd>GHOpenToPR<cr>", "Open To" },
				z = { "<cmd>GHCollapsePR<cr>", "Collapse" },
			},
			t = {
				name = "+Threads",
				c = { "<cmd>GHCreateThread<cr>", "Create" },
				n = { "<cmd>GHNextThread<cr>", "Next" },
				t = { "<cmd>GHToggleThread<cr>", "Toggle" },
			},
		},
	},
	t = {
		name = "toggle",
		t = { ":ToggleTerm size=30<cr>", "Terminal" },
	},
	T = {
		name = "tasks",
		t = {
			function()
				tasks:toggle()
			end,
			"Taskwarrior",
		},
	},
	n = {
		name = "notes",
		v = { ":Neorg gtd views<cr>", "View Notes" },
		c = { ":Neorg gtd capture<cr>", "Create Note" },
		j = {
			name = "journal",
			t = { ":Neorg journal today<cr>", "Today" },
			y = { ":Neorg journal yesterday<cr>", "Yesterday" },
			n = { ":Neorg journal tomorrow<cr>", "Tomorrow" },
			c = { ":Neorg journal ", "Create" },
		},
	},
	r = {
		name = "remote",
		u = { ":call SyncUploadFile()<cr>", "Upload" },
		d = { ":call SyncDownloadFile()<cr>", "Download" },
	},
	c = {
		name = "code",
		a = { ":Lspsaga code_action<CR>", "Action" },
		r = { "Lspsaga rename<CR>", "Rename" },
		d = { "Lspsaga show_line_diagnostics<CR>", "Diagnostic" },
	},
}, { prefix = "<leader>" })

-- misc
mapx.nnoremap("<C-e>", ":Neotree<CR>")

-- Terminal
mapx.tnoremap("<Esc>", "<C-\\><C-n>")

-- FileType Specific --
-- HTTP
mapx.nmap("<localleader>e", "<Plug>RestNvim", "silent", { ft = "http" })
mapx.nmap("<localleader>d", ":put =strftime('%m/%d/%y')<cr>", "silent", { ft = "norg" })

-- autorun
vim.cmd([[silent! NeorgStart silent=true]])
