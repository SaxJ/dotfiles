-- General settings
require("settings")

-- Plugins
require("config.lazy")

local wk = require("which-key")

local function inflection()
	local line, col = unpack(vim.api.nvim_win_get_cursor(0))
	local word = vim.fn.expand("<cword>")
	local word_start = vim.fn.matchstrpos(vim.fn.getline("."), "\\k*\\%" .. (col + 1) .. "c\\k*")[2]

	if word:find("[a-z][A-Z]") then
		-- Convert camelCase to snake_case
		local snake_case_word = word:gsub("([a-z])([A-Z])", "%1_%2"):lower()
		vim.api.nvim_buf_set_text(0, line - 1, word_start, line - 1, word_start + #word, { snake_case_word })
	elseif word:find("_[a-z]") then
		-- Convert snake_case to camelCase
		local camel_case_word = word:gsub("(_)([a-z])", function(_, l)
			return l:upper()
		end)
		vim.api.nvim_buf_set_text(0, line - 1, word_start, line - 1, word_start + #word, { camel_case_word })
	else
		print("Not a snake_case or camelCase word")
	end
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
	c = {
		name = "+code",
		a = { vim.lsp.buf.code_action, "Code Action" },
		r = { vim.lsp.buf.rename, "Rename" },
		d = { require("neogen").generate, "Generate Docs" },
		i = { inflection, "inflection" },
	},
	f = {
		name = "+files",
		Y = { ":CopyRelPath<CR>", "Yank Path" },
	},
	g = {
		name = "+git",
		b = { ":ToggleBlameLine<cr>", "Blame" },
		g = { ":Neogit<CR>", "Git" },
		d = { ":Telescope git_status<CR>", "Changed Files" },
		h = {
			name = "+github",
			p = { ":!gh pr create --web<CR>", "Pull Request" },
		},
		s = { ":Git status<CR>" },
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
		t = { ":split<CR>:terminal<CR>i", "Split Terminal" },
	},
	p = {
		name = "+project",
		p = { ":Telescope project<CR>", "Projects" },
	},
	r = {
		name = "+remote",
		u = { ":call SyncUploadFile()<cr>", "Upload" },
		d = { ":call SyncDownloadFile()<cr>", "Download" },
	},
	s = {
		name = "+search",
		p = { ":Telescope live_grep hidden=true<cr>", "Search Project" },
	},
	["."] = { ":Telescope file_browser path=%:p:h hidden=true<CR>", "Files" },
	["<leader>"] = { ":Telescope find_files find_command=rg,--ignore,--hidden,--files prompt_prefix=🔍<cr>", "Files" },
	["-"] = {
		function()
			require("mini.files").open(vim.api.nvim_buf_get_name(0))
		end,
		"Files",
	},
}, { prefix = "<leader>" })

vim.keymap.set("n", "<C-c>", ":ccl<CR>")

-- Terminal
vim.keymap.set("n", "<localleader>t", ":terminal<CR>")
vim.keymap.set("t", "<Esc>", "<C-\\><C-n>")
vim.keymap.set("t", "<C-k>", "<Up>")
vim.keymap.set("t", "<C-j>", "<Down>")
