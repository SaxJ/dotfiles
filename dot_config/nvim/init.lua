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
		t = { ":! zellij ac new-pane -f<CR>", "Terminal" },
		d = { ":Trouble<CR>", "Diagnostics" },
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
		p = { ":Telescope project<cr>", "Switch Project" },
	},
	g = {
		name = "+git",
		b = { ":ToggleBlameLine<cr>", "Blame" },
		g = { ":! zellij run -ci -- lazygit<CR>", "Git" },
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
	t = { ":terminal<CR>", "+terminal" },
	["."] = { ":Telescope file_browser path=%:p:h hidden=true<CR>", "Files" },
	["<leader>"] = { ":Telescope find_files find_command=rg,--ignore,--hidden,--files prompt_prefix=🔍<cr>", "Files" },
}, { prefix = "<leader>" })

mapx.nmap("C-c", ":ccl<CR>")

-- Terminal
mapx.tnoremap("<Esc>", "<C-\\><C-n>")

-- FileType Specific --
-- Neorg
mapx.nmap("<localleader>d", insertDate, "silent", { ft = "norg" })

-- HTTP
mapx.nmap("<localleader>e", "<Plug>RestNvim", "silent", { ft = "http" })

-- Neogit
mapx.nmap("@cp", ":! gh pr create --fill -w<CR>", "silent", { ft = "NeogitStatus" })

mapx.inoremap("C-j", "<Down>", "silent", { ft = "TelescopePrompt" })
mapx.inoremap("C-k", "<Up>", "silent", { ft = "TelescopePrompt" })
