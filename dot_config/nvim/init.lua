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
	["<tab>"] = {
		name = "+workspaces",
		t = { ":GonvimSidebarToggle<cr>", "Workspaces" },
		n = { ":GonvimWorkspaceNew<cr>", "New" },
		["<tab>"] = { ":GonvimWorkspaceSwitch ", "Switch" },
	},
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
		g = { ":Neogit<CR>", "Neogit" },
		l = {
			function()
				lazygit:toggle()
			end,
			"Lazy Git",
		},
		f = {
			name = "+forge",
			s = { ":Octo search assignee:SaxJ is:pr<CR>", "Search" },
			l = { ":Octo pr list<CR>", "List" },
		},
	},
	t = {
		name = "toggle",
		t = { ":ToggleTerm size=15<cr>", "Terminal" },
	},
	T = {
		name = "+tasks",
		t = {
			function()
				tasks:toggle()
			end,
			"Taskwarrior",
		},
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
	["."] = { ":Neotree current %:p:h:h %:p<CR>", "Files" },
}, { prefix = "<leader>" })

-- misc
mapx.nnoremap("<C-e>", ":Neotree current %:p:h:h %:p<CR>")

-- Terminal
mapx.tnoremap("<Esc>", "<C-\\><C-n>")

-- FileType Specific --
-- HTTP
mapx.nmap("<localleader>e", "<Plug>RestNvim", "silent", { ft = "http" })
mapx.nmap("<localleader>d", ":put =strftime('%m/%d/%y')<cr>", "silent", { ft = "norg" })
