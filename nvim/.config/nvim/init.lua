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
		cmd = "npm run dev",
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

-- Files
vim.keymap.set(
	"n",
	"<leader>fY",
	'<cmd>let @+ = expand("%:.")<CR><cmd>echo "Path Yanked"<CR>',
	{ desc = "[Y]ank file name" }
)
vim.keymap.set("n", "<leader>fD", "<cmd>!rm %<CR><cmd>bd<CR>", { desc = "Delete File" })

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
vim.keymap.set("n", "<leader>bb", "<cmd>FzfLua buffers<CR>", { desc = "Buffers" })
vim.keymap.set("n", "<leader>bY", [[maggVGy'a<cmd>echo "Buffer contents yanked"<CR>]], { desc = "Yank buffer" })

-- Git
vim.keymap.set("n", "<leader>gg", "<cmd>Neogit<CR>", { desc = "Git" })
vim.keymap.set("n", "<leader>gb", "<cmd>Gitsigns blame<CR>", { desc = "Blame" })

-- Logging
vim.keymap.set("n", "<leader>ti", function()
	TimeClock.clockin()
end, { desc = "Clock in" })
vim.keymap.set("n", "<leader>to", function()
	TimeClock.clockout()
end, { desc = "Clock out" })

-- Power
vim.keymap.set("n", "<leader>qq", "<cmd>qa<CR>", { desc = "Quit" })
vim.keymap.set("n", "<leader>qr", "<cmd>restart<CR>", { desc = "Restart" })

-- Projects
vim.keymap.set("n", "<leader>pt", "<cmd>VTerm<CR>i")
vim.keymap.set("n", "<leader>sp", "<cmd>FzfLua live_grep<CR>", { desc = "Grep" })

-- General
vim.keymap.set("n", "<leader><leader>", "<cmd>FzfLua files<CR>", { desc = "Files" })
vim.keymap.set("n", "<leader>-", "<cmd>Explore<CR>", { desc = "File Browser" })
vim.keymap.set("n", "<leader>.", "<cmd>FzfLua files cwd=%:p:h<cr>")

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
	gh("windwp/nvim-autopairs"),

	-- LSP
	gh("j-hui/fidget.nvim"),
	gh("mason-org/mason.nvim"),
	gh("neovim/nvim-lspconfig"),
	gh("rachartier/tiny-inline-diagnostic.nvim"),

	-- Completion
	{
		src = gh("saghen/blink.cmp"),
		version = vim.version.range("v1.*"),
	},
	gh("rafamadriz/friendly-snippets"),

	-- Picker
	gh("ibhagwan/fzf-lua"),

	-- Mini
	gh("nvim-mini/mini.nvim"),

	-- Notes
	gh("nvim-orgmode/orgmode"),

	-- Git
	gh("NeogitOrg/neogit"),
	gh("esmuellert/codediff.nvim"),
	gh("lewis6991/gitsigns.nvim"),

	-- AI
	gh("carlos-algms/agentic.nvim"),
  gh("pablopunk/pi.nvim"),
})

require("pi").setup({
  provider = "ollama",
  model = "qwen3-coder-next:cloud",
  thinking = "off",
})

require("fzf-lua").setup({})
-- vim.keymap.set("n", "<leader>pp", function()
-- 	FzfLua.zoxide({
-- 		actions = {
-- 			enter = function(selected, opts)
-- 				Project.open_tab_if_not_existing(selected[2])
-- 				FzfLua.actions.zoxide_cd(selected, opts)
-- 			end,
-- 		},
-- 		scope = "tab",
-- 	})
-- end, { desc = "Projects" })
vim.keymap.set("n", "<leader>pp", "<cmd>FzfLua zoxide<CR>", { desc = "Projects" })

vim.cmd("colorscheme tokyonight-night")

require("nvim-autopairs").setup({})
-- Completion
require("blink.cmp").setup({
	keymap = {
		preset = "default",
		["<Tab>"] = { "select_next", "fallback" },
		["<S-Tab>"] = { "select_prev", "fallback" },
		["<CR>"] = { "accept", "fallback" },
	},
	signature = { enabled = true },
	completion = {
		menu = { enabled = true },
		list = { selection = { preselect = false }, cycle = { from_top = false } },
		documentation = { auto_show = true },
	},
})

require("agentic").setup({})
vim.keymap.set("n", "<leader>aa", function()
	require("agentic").toggle({ auto_add_to_context = false, focus_prompt = true })
end, { desc = "Toggle Agent" })
vim.keymap.set("n", "<leader>ar", require("agentic").restore_session, { desc = "Resume Agent" })
vim.keymap.set("n", "<leader>an", require("agentic").new_session, { desc = "New Agent" })

require("conform").setup({
	formatters_by_ft = {
		lua = { "stylua" },
		javascript = { "prettier" },
		typescript = { "prettier" },
		typescriptreact = { "prettier" },
	},
})

vim.api.nvim_create_autocmd("BufWritePre", {
	pattern = "*",
	callback = function(args)
		require("conform").format({ bufnr = args.buf })
	end,
})

require("gitsigns").setup({
	signs = {
		add = { text = "+" },
		change = { text = "~" },
		delete = { text = "_" },
		topdelete = { text = "‾" },
		changedelete = { text = "~" },
	},
	on_attach = function()
		-- next/prev diff
		vim.keymap.set("n", "[c", function()
			if vim.wo.diff then
				vim.cmd.normal({ "[c", bang = true })
			else
				require("gitsigns").nav_hunk("prev")
			end
		end)
		vim.keymap.set("n", "]c", function()
			if vim.wo.diff then
				vim.cmd.normal({ "]c", bang = true })
			else
				require("gitsigns").nav_hunk("next")
			end
		end)
	end,
})

-- Neogit
require("neogit").setup({
	kind = "auto",
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
require("mini.surround").setup()
require("mini.icons").setup()
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

			vim.api.nvim_set_hl(0, "ClockedIn", { fg = "#000000", bg = "#3fec02" })
			vim.api.nvim_set_hl(0, "ClockedOut", { fg = "#c2230f", bg = "#292e42" })
			vim.api.nvim_set_hl(0, "Project", { fg = "#000000", bg = "#bb91f8" })

			local is_checked_in = TimeClock.is_checked_in()
			local status = TimeClock.status()
			local status_hl = "ClockedOut"
			if is_checked_in then
				status_hl = "ClockedIn"
			end

			local tasks = string.format("%d Tasks", vim.g.background_tasks_count)

			-- Usage of `MiniStatusline.combine_groups()` ensures highlighting and
			-- correct padding with spaces between groups (accounts for 'missing'
			-- sections, etc.)
			return MiniStatusline.combine_groups({
				{ hl = mode_hl, strings = { mode } },
				{ hl = "MiniStatuslineDevinfo", strings = { git, diff, diagnostics, lsp } },
				"%<", -- Mark general truncate point
				{ hl = "MiniStatuslineFilename", strings = { filename } },
				"%=", -- End left alignment
				{ hl = "Project", strings = { vim.fn.getcwd() } },
				{ hl = "MiniStatuslineFileinfo", strings = { fileinfo } },
				{ hl = status_hl, strings = { status } },
				{ hl = "MiniStatuslineFileinfo", strings = { tasks } },
			})
		end,
	},
})

require("tiny-inline-diagnostic").setup({})

-- MASON
require("mason").setup()

-- LSP
require("fidget").setup({})

local capabilities = require("blink.cmp").get_lsp_capabilities()
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
	callback = function()
		vim.keymap.set("n", "<leader>cr", vim.lsp.buf.rename)
		vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action)
		vim.keymap.set("n", "gr", vim.lsp.buf.references)
		vim.keymap.set("n", "gi", vim.lsp.buf.implementation)
		vim.keymap.set("n", "gd", vim.lsp.buf.definition)

		vim.diagnostic.config({
			float = { border = "rounded" },
		})
		vim.diagnostic.enable(true)
	end,
})

-- vim: ts=2 sts=2 sw=2 et
