vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Set to true if you have a Nerd Font installed and selected in the terminal
vim.g.have_nerd_font = true

vim.g.background_tasks = {
	["Hannibal Dev"] = {
		dir = "~/Documents/hannibal/",
		cmd = "yarn dev -- -p 4000",
	},
	["Unicron Dev"] = {
		dir = "~/Documents/unicron/",
		cmd = "npm run dev -- -p 3000",
	},
	["Unicron Storybook"] = {
		dir = "~/Documents/unicron/",
		cmd = "npm run storybook",
	},
}

vim.o.number = true
vim.o.relativenumber = true
vim.o.wrap = false

-- Enable mouse mode, can be useful for resizing splits for example!
vim.o.mouse = "a"

-- Don't show the mode, since it's already in the status line
vim.o.showmode = false

-- Show one global statusline
vim.o.laststatus = 3

vim.schedule(function()
	vim.o.clipboard = "unnamedplus"
end)

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Case-insensitive searching UNLESS \C or one or more capital letters in the search term
vim.o.ignorecase = true
vim.o.smartcase = true

-- Keep signcolumn on by default
vim.o.signcolumn = "yes"

-- Decrease update time
vim.o.updatetime = 250

-- Decrease mapped sequence wait time
vim.o.timeoutlen = 300

-- Configure how new splits should be opened
vim.o.splitright = true
vim.o.splitbelow = true

vim.o.list = true
vim.opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }

-- Preview substitutions live, as you type!
vim.o.inccommand = "split"

-- Show which line your cursor is on
vim.o.cursorline = true

-- Minimal number of screen lines to keep above and below the cursor.
vim.o.scrolloff = 10

-- if performing an operation that would fail due to unsaved changes in the buffer (like `:q`),
-- instead raise a dialog asking if you wish to save the current file(s)
vim.o.confirm = true

-- CUSTOM FILETYPES
vim.filetype.add({ pattern = {
	[".*%.blade%.php"] = "blade",
} })
vim.filetype.add({ extension = { templ = "templ" } })

-- Clear highlights on search when pressing <Esc> in normal mode
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")

-- Misc useful keymaps
vim.keymap.set("n", "<leader>R", "<cmd>bufdo checktime<CR>", { desc = "[R]eload buffers" })

-- Diagnostic keymaps
vim.keymap.set("n", "<leader>od", vim.diagnostic.setloclist, { desc = "[D]iagnostics" })
vim.keymap.set("n", "<leader>ob", "<cmd>Runner<CR>", { desc = "Task runner" })
vim.keymap.set("n", "<leader>op", "<cmd>Background<CR>", { desc = "Background job" })

-- Terminal
vim.keymap.set("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })
vim.keymap.set("t", "<C-k>", "<Up>", { desc = "Exit terminal mode" })
vim.keymap.set("t", "<C-j>", "<Down>", { desc = "Exit terminal mode" })

-- Keybinds to make split navigation easier.
vim.keymap.set("n", "<C-h>", "<C-w><C-h>", { desc = "Move focus to the left window" })
vim.keymap.set("n", "<C-l>", "<C-w><C-l>", { desc = "Move focus to the right window" })
vim.keymap.set("n", "<C-j>", "<C-w><C-j>", { desc = "Move focus to the lower window" })
vim.keymap.set("n", "<C-k>", "<C-w><C-k>", { desc = "Move focus to the upper window" })

-- Yanking
vim.keymap.set(
	"n",
	"<leader>fY",
	'<cmd>let @+ = expand("%:.")<CR><cmd>echo "Path Yanked"<CR>',
	{ desc = "[Y]ank file name" }
)
vim.keymap.set("n", "<leader>bY", [[maggVGy'a<cmd>echo "Buffer contents yanked"<CR>]], { desc = "Yank buffer" })

-- Remote upload/download
vim.keymap.set("n", "<leader>ru", "<cmd>ScpUpload<CR>", { desc = "[R]emote [U]pload" })
vim.keymap.set("n", "<leader>rd", "<cmd>ScpDownload<CR>", { desc = "[R]emote [U]pload" })

-- Terminal
vim.keymap.set("n", "<leader>ot", "<cmd>HTerm<CR>i", { desc = "[O]pen [T]erminal" })
vim.keymap.set("n", "<leader>oT", "<cmd>term<CR>i", { desc = "[O]pen [T]erminal" })

-- Tabs
vim.keymap.set("n", "<leader><tab>c", "<cmd>tabclose<CR>", { desc = "Close Tab" })
vim.keymap.set("n", "<leader><tab>n", "<cmd>tabnext<CR>", { desc = "Next Tab" })
vim.keymap.set("n", "<leader><tab>p", "<cmd>tabp<CR>", { desc = "Prev Tab" })
vim.keymap.set("n", "<leader><tab>t", "<cmd>TTerm<CR>", { desc = "Terminal in Tab" })

-- Buffers
vim.keymap.set("n", "<leader>bb", "<cmd>Pick buffers<CR>", { desc = "Buffers" })

-- Git
vim.keymap.set("n", "<leader>gg", "<cmd>Neogit<CR>", { desc = "Git" })
vim.keymap.set("n", "<leader>gb", "<cmd>Gitsigns blame<CR>", { desc = "Blame" })

-- Power
vim.keymap.set("n", "<leader>qq", "<cmd>qa<CR>", { desc = "Quit" })
vim.keymap.set("n", "<leader>qr", "<cmd>restart<CR>", { desc = "Restart" })

-- Projects
vim.keymap.set("n", "<leader>pp", function()
	local picked = MiniPick.builtin.cli({ command = { "zoxide", "query", "--all", "--list" } })
  vim.cmd('tabnew')
  vim.cmd('tcd ' .. picked)
end, { desc = "Projects" })
vim.keymap.set("n", "<leader>pt", "<cmd>VTerm<CR>i")
vim.keymap.set("n", "<leader>sp", "<cmd>Pick grep_live<CR>", { desc = "Grep" })

-- General
vim.keymap.set("n", "<leader><leader>", "<cmd>Pick files<CR>", { desc = "Files" })
vim.keymap.set('n', '<leader>-', function ()
  MiniFiles.open(MiniFiles.get_latest_path())
end)

-- Completion
vim.keymap.set("i", "<Tab>", [[pumvisible() ? "\<C-n>" : "\<Tab>"]], { expr = true })
vim.keymap.set("i", "<S-Tab>", [[pumvisible() ? "\<C-p>" : "\<S-Tab>"]], { expr = true })
vim.keymap.set("i", "<CR>", function()
	-- If there is selected item in popup, accept it with <C-y>
	if vim.fn.complete_info()["selected"] ~= -1 then
		return "\25"
	end
	-- Fall back to plain `<CR>`. You might want to customize according
	-- to other plugins. For example if 'mini.pairs' is set up, replace
	-- next line with `return MiniPairs.cr()`
	return "\r"
end, { expr = true })

-- Highlight when yanking (copying) text
vim.api.nvim_create_autocmd("TextYankPost", {
	desc = "Highlight when yanking (copying) text",
	group = vim.api.nvim_create_augroup("kickstart-highlight-yank", { clear = true }),
	callback = function()
		vim.hl.on_yank()
	end,
})

-- Checktime on file on focus
vim.api.nvim_create_autocmd("BufEnter", {
	desc = "Refresh file on buffer focus",
	callback = function()
		vim.cmd("checktime")
	end,
})

local function short_path(path)
	if path == vim.env.HOME then
		return "~"
	end

	return vim.fn.fnamemodify(path, ":t")
end

_G.my_tabline = function()
	local result = ""

	-- Iterate over all tabpages (the order they appear in the UI)
	for _, tabpage in ipairs(vim.api.nvim_list_tabpages()) do
		local tabnr = vim.api.nvim_tabpage_get_number(tabpage) -- 1‑based index
		local cwd = vim.fn.getcwd(0, tabnr) -- tab‑local cwd

		-- Fallback: if a tab has no tab‑local cwd (very rare) use the global one
		if cwd == "" then
			cwd = vim.fn.getcwd()
		end

		-- Choose the highlight group
		if tabpage == vim.api.nvim_get_current_tabpage() then
			result = result .. "%#TabLineSel#" -- active tab
		else
			result = result .. "%#TabLine#" -- inactive tabs
		end

		-- Build the click‑area for the tab (so you can click it)
		-- %{:tabpage_number} makes the label jump to that tab when clicked
		result = result .. string.format("%%%dT", tabnr)

		-- The visible label (you can tweak it as you like)
		result = result .. " " .. short_path(cwd) .. " "

		-- Close the click‑area (Neovim automatically resets it at the next %)
	end

	-- Add a trailing filler so the line stretches across the whole window
	result = result .. "%#TabLineFill#%="

	return result
end

vim.o.tabline = "%!v:lua.my_tabline()"

local function gh(slug)
	return "https://github.com/" .. slug
end

vim.pack.add({
	-- Pretty
	gh("miikanissi/modus-themes.nvim"),
	gh("folke/tokyonight.nvim"),
	gh("3rd/image.nvim"),

	-- Util
	gh("nvim-lua/plenary.nvim"),
	gh("stevearc/conform.nvim"),

	-- LSP
	gh("j-hui/fidget.nvim"),
	gh("mason-org/mason.nvim"),
  gh("neovim/nvim-lspconfig"),
	gh("rachartier/tiny-inline-diagnostic.nvim"),

	-- Treesitter
  {
    src = gh("nvim-treesitter/nvim-treesitter"),
    version = "main",
  },

	-- Mini
	gh("nvim-mini/mini.nvim"),

	-- Notes
	gh("nvim-orgmode/orgmode"),

	-- Git
	gh("NeogitOrg/neogit"),
	gh("esmuellert/codediff.nvim"),
	gh("lewis6991/gitsigns.nvim"),

	-- AI
	gh("olimorris/codecompanion.nvim"),
})

vim.cmd("colorscheme tokyonight-night")

local treesitter_langs = {
	"bash",
	"c",
	"cpp",
	"go",
	"gomod",
	"html",
  "css",
	"javascript",
	"lua",
	"python",
	"regex",
	"rust",
	"tsx",
	"typescript",
	"vim",
	"vimdoc",
	"vue",
	"xml",
  "php",
  "templ",
}

require('nvim-treesitter').install(treesitter_langs)

vim.api.nvim_create_autocmd("FileType", {
	pattern = treesitter_langs,
	callback = function()
		vim.treesitter.start()
	end,
})

require("codecompanion").setup({
	interactions = {
		chat = {
			adapter = {
				name = "ollama",
				model = "minimax-m2.7:cloud",
			},
		},
		inline = {
			adapter = {
				name = "ollama",
				model = "minimax-m2.7:cloud",
			},
		},
		cmd = {
			adapter = {
				name = "ollama",
				model = "minimax-m2.7:cloud",
			},
		},
		background = {
			adapter = {
				name = "ollama",
				model = "minimax-m2.7:cloud",
			},
		},
	},
})

require("conform").setup({
	formatters_by_ft = {
		lua = { "stylua" },
		javascript = { "prettier" },
		typescript = { "prettier" },
		typescriptreact = { "prettier" },
	},
})

require("gitsigns").setup({
	signs = {
		add = { text = "+" },
		change = { text = "~" },
		delete = { text = "_" },
		topdelete = { text = "‾" },
		changedelete = { text = "~" },
	},
})

-- Neogit
require("neogit").setup({
	kind = "split_below_all",
	prompt_force_push = false,
	graph_style = "unicode",
	process_spinner = true,
	mappings = {
		finder = {
			["<C-j>"] = "Next",
			["<C-k>"] = "Previous",
		},
	},
	integrations = {
		codediff = true,
	},
})

-- Org
require("orgmode").setup({
	org_agenda_files = "~/Documents/wiki/**/*.org",
	org_default_notes_file = "~/Documents/wiki/inbox.org",
	org_capture_templates = {
		t = { description = "Task", template = "* TODO [#%^{A|B|C}] %? %t" },
		j = {
			description = "Journal",
			template = "%?",
			datetree = true,
			target = "~/Documents/wiki/journal.org",
		},
	},
})

-- Mini setup
require("mini.ai").setup({})
require("mini.comment").setup({})
require("mini.icons").setup()
require("mini.snippets").setup()
require("mini.completion").setup({
	source_func = "omnifunc",
	auto_setup = false,
})
require("mini.pairs").setup()
require("mini.surround").setup()
require("mini.files").setup({
	options = {
		use_as_default_explorer = false,
	},
})
require("mini.pick").setup()
require("mini.visits").setup()
require("mini.notify").setup()
require("mini.git").setup()
require("mini.diff").setup()
require("mini.statusline").setup({
	content = {
		active = function()
			local mode, mode_hl = MiniStatusline.section_mode({ trunc_width = 120 })
			local git = MiniStatusline.section_git({ trunc_width = 40 })
			local diff = MiniStatusline.section_diff({ trunc_width = 75 })
			local diagnostics = MiniStatusline.section_diagnostics({ trunc_width = 75 })
			local lsp = MiniStatusline.section_lsp({ trunc_width = 75 })
			local filename = MiniStatusline.section_filename({ trunc_width = 140 })
			local fileinfo = MiniStatusline.section_fileinfo({ trunc_width = 120 })
			local location = MiniStatusline.section_location({ trunc_width = 75 })
			local search = MiniStatusline.section_searchcount({ trunc_width = 75 })

			-- Usage of `MiniStatusline.combine_groups()` ensures highlighting and
			-- correct padding with spaces between groups (accounts for 'missing'
			-- sections, etc.)
			return MiniStatusline.combine_groups({
				{ hl = mode_hl, strings = { mode } },
				{ hl = "MiniStatuslineDevinfo", strings = { git, diff, diagnostics, lsp } },
				"%<", -- Mark general truncate point
				{ hl = "MiniStatuslineFilename", strings = { filename } },
				"%=", -- End left alignment
				{ hl = "MiniStatuslineFileinfo", strings = { fileinfo } },
				{ hl = mode_hl, strings = {} },
			})
		end,
	},
})

-- MASON
require("mason").setup()

-- LSP
require("fidget").setup({})

local capabilities = MiniCompletion.get_lsp_capabilities()
vim.lsp.config("vtsls", { capabilities = capabilities })
vim.lsp.config("intelephense", { capabilities = capabilities })
vim.lsp.config("lua_ls", {
	capabilities = capabilities,
	settings = {
		Lua = {
			completion = {
				callSnippet = "Replace",
			},
			diagnostics = {
				disable = { "missing-fields" },
				globals = { "vim" },
			},
			workspace = {
				library = {
					[vim.fn.expand("$VIMRUNTIME/lua")] = true,
					[vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
					-- Plugins
					[vim.fn.stdpath("data") .. "/site"] = true,
					-- Config
					[vim.fn.stdpath("config") .. "/lua"] = true,
				},
				checkThirdParty = false,
			},
			telemetry = {
				enable = false,
			},
		},
	},
})
vim.lsp.enable({
	"intelephense",
	"lua_ls",
	"vtsls",
	"gopls",
	"templ",
})

vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args)
		vim.keymap.set("n", "<leader>cr", vim.lsp.buf.rename)
		vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action)
		vim.keymap.set("n", "gr", vim.lsp.buf.references)
		vim.keymap.set("n", "gi", vim.lsp.buf.implementation)
		vim.keymap.set("n", "gd", vim.lsp.buf.definition)

		vim.diagnostic.config({
			float = { border = "rounded" },
		})
		vim.diagnostic.enable(true)

		vim.bo[args.buf].omnifunc = "v:lua.MiniCompletion.completefunc_lsp"
	end,
})

-- vim: ts=2 sts=2 sw=2 et
