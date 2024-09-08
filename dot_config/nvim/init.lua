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
		-- wakatime
		{ "wakatime/vim-wakatime", lazy = false },

		-- completion
		{
			"hrsh7th/nvim-cmp",
			event = "InsertEnter",
			dependencies = {
				"hrsh7th/cmp-buffer", -- source for text in buffer
				"hrsh7th/cmp-path", -- source for file system paths
				{
					"L3MON4D3/LuaSnip",
					version = "v2.*",
					-- install jsregexp (optional!).
					build = "make install_jsregexp",
				},
				"rafamadriz/friendly-snippets",
				"onsails/lspkind.nvim", -- vs-code like pictograms
			},
			config = function()
				local cmp = require("cmp")
				local luasnip = require("luasnip")

				require("luasnip.loaders.from_vscode").lazy_load()

				cmp.setup({
					snippet = {
						expand = function(args)
							luasnip.lsp_expand(args.body)
						end,
					},
					sources = cmp.config.sources({
						{ name = "nvim_lsp" },
						{ name = "luasnip" },
						{ name = "buffer" },
						{ name = "path" },
					}),
					mapping = {

						["<CR>"] = cmp.mapping(function(fallback)
							if cmp.visible() then
								if luasnip.expandable() then
									luasnip.expand()
								else
									cmp.confirm({
										select = true,
									})
								end
							else
								fallback()
							end
						end),

						["<Tab>"] = cmp.mapping(function(fallback)
							if cmp.visible() then
								cmp.select_next_item()
							elseif luasnip.locally_jumpable(1) then
								luasnip.jump(1)
							else
								fallback()
							end
						end, { "i", "s" }),

						["<S-Tab>"] = cmp.mapping(function(fallback)
							if cmp.visible() then
								cmp.select_prev_item()
							elseif luasnip.locally_jumpable(-1) then
								luasnip.jump(-1)
							else
								fallback()
							end
						end, { "i", "s" }),
					},
				})

				vim.cmd([[
      set completeopt=menuone,noinsert,noselect
      highlight! default link CmpItemKind CmpItemMenuDefault
    ]])
			end,
		},

		-- Theme
		{
			"tiagovla/tokyodark.nvim",
			lazy = false,
			priority = 1000,
			config = function()
				vim.cmd("colorscheme tokyodark")
			end,
		},

		-- Copy/paste stuff
		{
			"gbprod/yanky.nvim",
			opts = {},
			keys = {
				{
					"<leader>y",
					function()
						require("telescope").extensions.yank_history.yank_history({})
					end,
					desc = "Open Yank History",
				},
				{
					"y",
					"<Plug>(YankyYank)",
					mode = { "n", "x" },
					desc = "Yank text",
				},
				{
					"p",
					"<Plug>(YankyPutAfter)",
					mode = { "n", "x" },
					desc = "Put yanked text after cursor",
				},
				{
					"P",
					"<Plug>(YankyPutBefore)",
					mode = { "n", "x" },
					desc = "Put yanked text before cursor",
				},
				{
					"gp",
					"<Plug>(YankyGPutAfter)",
					mode = { "n", "x" },
					desc = "Put yanked text after selection",
				},
				{
					"gP",
					"<Plug>(YankyGPutBefore)",
					mode = { "n", "x" },
					desc = "Put yanked text before selection",
				},
				{
					"<c-p>",
					"<Plug>(YankyPreviousEntry)",
					desc = "Select previous entry through yank history",
				},
				{
					"<c-n>",
					"<Plug>(YankyNextEntry)",
					desc = "Select next entry through yank history",
				},
				{
					"]p",
					"<Plug>(YankyPutIndentAfterLinewise)",
					desc = "Put indented after cursor (linewise)",
				},
				{
					"[p",
					"<Plug>(YankyPutIndentBeforeLinewise)",
					desc = "Put indented before cursor (linewise)",
				},
				{
					"]P",
					"<Plug>(YankyPutIndentAfterLinewise)",
					desc = "Put indented after cursor (linewise)",
				},
				{
					"[P",
					"<Plug>(YankyPutIndentBeforeLinewise)",
					desc = "Put indented before cursor (linewise)",
				},
				{
					">p",
					"<Plug>(YankyPutIndentAfterShiftRight)",
					desc = "Put and indent right",
				},
				{
					"<p",
					"<Plug>(YankyPutIndentAfterShiftLeft)",
					desc = "Put and indent left",
				},
				{
					">P",
					"<Plug>(YankyPutIndentBeforeShiftRight)",
					desc = "Put before and indent right",
				},
				{
					"<P",
					"<Plug>(YankyPutIndentBeforeShiftLeft)",
					desc = "Put before and indent left",
				},
				{
					"=p",
					"<Plug>(YankyPutAfterFilter)",
					desc = "Put after applying a filter",
				},
				{
					"=P",
					"<Plug>(YankyPutBeforeFilter)",
					desc = "Put before applying a filter",
				},
			},
		},

		-- MASON
		{
			"williamboman/mason.nvim",
			dependencies = {
				"williamboman/mason-lspconfig.nvim",
			},
			config = function()
				require("mason").setup()

				require("mason-lspconfig").setup({
					automatic_installation = true,
					ensure_installed = {
						"cssls",
						"eslint",
						"html",
						"jsonls",
						"tsserver",
						"pyright",
						"tailwindcss",
					},
				})
			end,
		},

		-- LSP configuration
		{
			"neovim/nvim-lspconfig",
			event = { "BufReadPre", "BufNewFile" },
			dependencies = {
				"hrsh7th/cmp-nvim-lsp",
				{ "folke/neodev.nvim", opts = {} },
			},
			config = function()
				local nvim_lsp = require("lspconfig")
				local mason_lspconfig = require("mason-lspconfig")

				local on_attach = function(client, bufnr)
					-- format on save
					if client.server_capabilities.documentFormattingProvider then
						vim.api.nvim_create_autocmd("BufWritePre", {
							group = vim.api.nvim_create_augroup("Format", { clear = true }),
							buffer = bufnr,
							callback = function()
								vim.lsp.buf.format()
							end,
						})

						local opts = { buffer = bufnr }

						vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
						vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
						vim.keymap.set("n", "gD", vim.lsp.buf.references, opts)
						vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts)
						vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
						vim.keymap.set("n", "gs", vim.lsp.buf.signature_help, opts)
						vim.keymap.set("n", "<leader>cr", vim.lsp.buf.rename, opts)
						vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts)
					end
				end

				local capabilities = require("cmp_nvim_lsp").default_capabilities()

				mason_lspconfig.setup_handlers({
					function(server)
						nvim_lsp[server].setup({
							capabilities = capabilities,
						})
					end,
					["omnisharp"] = function()
						nvim_lsp["omnisharp"].setup({
							cmd = { "omnisharp" },
							on_attach = on_attach,
							capabilities = capabilities,
						})
					end,
					["tsserver"] = function()
						nvim_lsp["ts_ls"].setup({
							on_attach = on_attach,
							capabilities = capabilities,
						})
					end,
					["cssls"] = function()
						nvim_lsp["cssls"].setup({
							on_attach = on_attach,
							capabilities = capabilities,
						})
					end,
					["html"] = function()
						nvim_lsp["html"].setup({
							on_attach = on_attach,
							capabilities = capabilities,
						})
					end,
					["jsonls"] = function()
						nvim_lsp["jsonls"].setup({
							on_attach = on_attach,
							capabilities = capabilities,
						})
					end,
					["pyright"] = function()
						nvim_lsp["pyright"].setup({
							on_attach = on_attach,
							capabilities = capabilities,
						})
					end,
				})
			end,
		},

		-- Formatting
		{
			"stevearc/conform.nvim",
			event = { "BufReadPre", "BufNewFile" },
			config = function()
				local conform = require("conform")

				conform.setup({
					formatters_by_ft = {
						javascript = { "prettier" },
						typescript = { "prettier" },
						javascriptreact = { "prettier" },
						typescriptreact = { "prettier" },
						css = { "prettier" },
						html = { "prettier" },
						json = { "prettier" },
						yaml = { "prettier" },
						markdown = { "prettier" },
						lua = { "stylua" },
						python = { "isort", "black" },
						cs = { "csharpier" },
					},
					format_on_save = {
						lsp_fallback = true,
						async = false,
						timeout_ms = 1000,
					},
				})

				vim.keymap.set({ "n", "v" }, "<leader>f", function()
					conform.format({
						lsp_fallback = true,
						async = false,
						timeout_ms = 1000,
					})
				end, { desc = "Format file or range (in visual mode)" })
			end,
		},

		-- Git UI integration
		{
			"lewis6991/gitsigns.nvim",
			config = function()
				local gitsigns = require("gitsigns")
				gitsigns.setup({
					signs = {
						add = { text = "│" },
						change = { text = "│" },
						delete = { text = "_" },
						topdelete = { text = "‾" },
						changedelete = { text = "~" },
						untracked = { text = "┆" },
					},
					signcolumn = true, -- Toggle with `:Gitsigns toggle_signs`
					numhl = false, -- Toggle with `:Gitsigns toggle_numhl`
					linehl = false, -- Toggle with `:Gitsigns toggle_linehl`
					word_diff = false, -- Toggle with `:Gitsigns toggle_word_diff`
					watch_gitdir = {
						interval = 1000,
						follow_files = true,
					},
					attach_to_untracked = true,
					current_line_blame = false, -- Toggle with `:Gitsigns toggle_current_line_blame`
					current_line_blame_opts = {
						virt_text = true,
						virt_text_pos = "eol", -- 'eol' | 'overlay' | 'right_align'
						delay = 1000,
						ignore_whitespace = false,
					},
					current_line_blame_formatter = "<author>, <author_time:%Y-%m-%d> - <summary>",
					sign_priority = 6,
					update_debounce = 100,
					status_formatter = nil, -- Use default
					max_file_length = 40000, -- Disable if file is longer than this (in lines)
					preview_config = {
						-- Options passed to nvim_open_win
						border = "single",
						style = "minimal",
						relative = "cursor",
						row = 0,
						col = 1,
					},
				})
			end,
		},

		-- Telescope
		{
			"nvim-telescope/telescope.nvim",
			dependencies = { "nvim-lua/plenary.nvim" },
			config = function()
				local actions = require("telescope.actions")
				require("telescope").setup({
					defaults = {
						mappings = {
							i = {
								["<esc>"] = actions.close,
								["<C-k>"] = actions.move_selection_previous,
								["<C-j>"] = actions.move_selection_next,
							},
						},
					},
				})
			end,
		},

		{
			"ahmedkhalf/project.nvim",
			dependencies = { "nvim-telescope/telescope.nvim" },
			config = function()
				require("project_nvim").setup({
					-- Manual mode doesn't automatically change your root directory, so you have
					-- the option to manually do so using `:ProjectRoot` command.
					manual_mode = false,

					-- Methods of detecting the root directory. **"lsp"** uses the native neovim
					-- lsp, while **"pattern"** uses vim-rooter like glob pattern matching. Here
					-- order matters: if one is not detected, the other is used as fallback. You
					-- can also delete or rearangne the detection methods.
					detection_methods = { "lsp", "pattern" },

					-- All the patterns used to detect root dir, when **"pattern"** is in
					-- detection_methods
					patterns = { ".git", "_darcs", ".hg", ".bzr", ".svn", "Makefile", "package.json", ".project" },

					-- Table of lsp clients to ignore by name
					-- eg: { "efm", ... }
					ignore_lsp = {},

					-- Don't calculate root dir on specific directories
					-- Ex: { "~/.cargo/*", ... }
					exclude_dirs = {},

					-- Show hidden files in telescope
					show_hidden = true,

					-- When set to false, you will get a message when project.nvim changes your
					-- directory.
					silent_chdir = true,

					-- What scope to change the directory, valid options are
					-- * global (default)
					-- * tab
					-- * win
					scope_chdir = "global",

					-- Path where project.nvim will store the project history for use in
					-- telescope
					datapath = vim.fn.stdpath("data"),
				})
				require("telescope").load_extension("projects")
			end,
		},

		-- Treesitter
		{
			"nvim-treesitter/nvim-treesitter",
			build = ":TSUpdate",
			config = function()
				local configs = require("nvim-treesitter.configs")

				configs.setup({
					ensure_installed = {
						"c",
						"lua",
						"vim",
						"vimdoc",
						"query",
						"elixir",
						"heex",
						"javascript",
						"html",
						"php",
						"c_sharp",
						"typescript",
						"haskell",
						"elm",
					},
					sync_install = false,
					highlight = { enable = true },
					indent = { enable = true },
					modules = {},
					auto_install = true,
					ignore_install = {},
				})
			end,
		},
		{ "voldikss/vim-floaterm" },
		{
			"OscarCreator/rsync.nvim",
			build = "make",
			dependencies = "nvim-lua/plenary.nvim",
			config = function()
				require("rsync").setup({
					fugitive_sync = false,
					sync_on_save = false,
					reload_file_after_sync = true,
					-- on_exit = function(code, _command) end,
					-- on_stderr = function(data, command) end,
					project_config_path = ".nvim/rsync.toml",
				})
			end,
		},
		-- optional for icons
		{ "nvim-tree/nvim-web-devicons" },

		-- optional for the 'fzf' command
		{
			"junegunn/fzf",
			build = function()
				vim.fn["fzf#install"]()
			end,
		},

		{
			"echasnovski/mini.nvim",
			version = false,
			config = function()
				require("mini.ai").setup()
				require("mini.basics").setup()
				require("mini.files").setup()
				require("mini.surround").setup()
			end,
		},

		{
			"folke/which-key.nvim",
			event = "VeryLazy",
			opts = {
				preset = "modern",
			},
			keys = {
				{
					"<leader>?",
					function()
						require("which-key").show({ global = false })
					end,
					desc = "Buffer Local Keymaps (which-key)",
				},
				{
					"<leader>.",
					function()
						require("telescope.builtin").find_files({ cwd = vim.fn.expand("%:p:h") })
					end,
					desc = "Siblings",
				},
				{ "<leader><leader>", "<cmd>Telescope find_files<cr>", desc = "Find files" },
				{ "<leader>-", ":lua MiniFiles.open()<CR>", desc = "File Browser" },
				{ "<leader>p", group = "Project" },
				{ "<leader>pp", "<cmd>Telescope projects<cr>", desc = "Switch Project" },
				{ "<leader>g", group = "Git" },
				{
					"<leader>gg",
					function()
						local cwd = vim.fn.getcwd()
						vim.cmd(string.format("FloatermNew (cd %s && gitu)", cwd))
					end,
					desc = "Git status",
				},
				{ "<leader>r", group = "Remote" },
				{ "<leader>ru", ":RsyncUpFile<cr>", desc = "Upload File" },
				{ "<leader>rd", ":RsyncDownFile<cr>", desc = "Download File" },
			},
		},
	},
	-- Configure any other settings here. See the documentation for more details.
	-- colorscheme that will be used when installing plugins.
	install = { colorscheme = { "habamax" } },
	-- automatically check for plugin updates
	checker = { enabled = true },
})
