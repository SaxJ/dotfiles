local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
	fn.system({ "git", "clone", "https://github.com/wbthomason/packer.nvim", install_path })
	execute("packadd packer.nvim")
end

vim.cmd("packadd packer.nvim")

-- Package installation
return require("packer").startup(function(use)
	-- Packer can manage itself
	use("wbthomason/packer.nvim")

	-- Libraries
	use("b0o/mapx.nvim")
	use({
		"echasnovski/mini.nvim",
		config = function()
			require("mini.comment").setup({})
			require("mini.cursorword").setup({})
			require("mini.jump").setup({})
			require("mini.pairs").setup({})
		end,
	})
	use("arkav/lualine-lsp-progress")
	use("samjwill/nvim-unception")

	-- Theme
	use({
		"folke/tokyonight.nvim",
		config = function()
			vim.api.nvim_command("colorscheme tokyonight-storm")
		end,
	})

	-- Statusline
	use({
		"nvim-lualine/lualine.nvim",
		requires = { "kyazdani42/nvim-web-devicons", opt = true },
		config = function()
			require("lualine").setup({
				options = {
					globalstatus = true,
					theme = "tokyonight",
				},
				sections = {
					lualine_c = {
						{ "filename", path = 1 },
						{ "lsp_progress" },
					},
					lualine_x = {
						"encoding",
						"fileformat",
						"filetype",
					},
				},
			})
		end,
	})

	-- Autocomplete commands
	use({
		"gelguy/wilder.nvim",
		config = function()
			local wilder = require("wilder")
			wilder.setup({ modes = { ":", "/", "?" } })
		end,
	})

	-- Show diagnostics in top right
	use({
		"Mofiqul/trld.nvim",
		config = function()
			require("trld").setup({})
		end,
	})

	-- Languages
	use("imsnif/kdl.vim")
	use("mracos/mermaid.vim")
	use("amadeus/vim-mjml")
	use("adamclerk/vim-razor")
	use("jparise/vim-graphql")
	use({
		"norcalli/nvim-colorizer.lua",
		config = function()
			require("colorizer").setup()
		end,
	})
	use("pbrisbin/vim-syntax-shakespeare")
	use({
		"lukas-reineke/indent-blankline.nvim",
		config = function()
			require("indent_blankline").setup({
				show_current_context = true,
				show_current_context_start = true,
			})
		end,
	})
	use({
		"jose-elias-alvarez/null-ls.nvim",
		config = function()
			require("null-ls").setup({
				sources = {
					require("null-ls").builtins.formatting.stylua,
					require("null-ls").builtins.diagnostics.eslint,
					require("null-ls").builtins.formatting.phpcsfixer,
				},
			})
		end,
	})

	-- LSP
	use({
		"hrsh7th/nvim-cmp",
		requires = {
			{ "hrsh7th/nvim-cmp" },
			{ "hrsh7th/cmp-buffer" },
			{ "hrsh7th/cmp-path" },
			{ "hrsh7th/cmp-nvim-lsp" },
			{ "hrsh7th/cmp-nvim-lua" },
			{ "L3MON4D3/LuaSnip" },
			{ "saadparwaiz1/cmp_luasnip" },
		},
	})
	use({ "folke/neodev.nvim" })
	use({
		"MrcJkb/haskell-tools.nvim",
		requires = {
			"neovim/nvim-lspconfig",
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope.nvim", -- optional
		},
	})
	use({ "neovim/nvim-lspconfig" })
	use({
		"glepnir/lspsaga.nvim",
		config = function()
			require("lspsaga").init_lsp_saga({
				symbol_in_winbar = {
					in_custom = true,
				},
			})
		end,
	})

	use("b0o/schemastore.nvim")

	use({
		"someone-stole-my-name/yaml-companion.nvim",
		requires = {
			{ "neovim/nvim-lspconfig" },
			{ "nvim-lua/plenary.nvim" },
			{ "nvim-telescope/telescope.nvim" },
		},
		config = function()
			require("telescope").load_extension("yaml_schema")

			local actions = require("telescope.actions")
			require("telescope").setup({
				defaults = {
					mappings = {
						i = {
							["<C-j>"] = actions.move_selection_next,
							["<C-k>"] = actions.move_selection_previous,
						},
					},
					vimgrep_arguments = {
						"rg",
						"--color=never",
						"--no-heading",
						"--with-filename",
						"--line-number",
						"--column",
						"--smart-case",
					},
					prompt_prefix = "> ",
					selection_caret = "> ",
					entry_prefix = "  ",
					initial_mode = "insert",
					selection_strategy = "reset",
					sorting_strategy = "descending",
					layout_strategy = "horizontal",
					layout_config = {
						horizontal = {
							mirror = false,
						},
						vertical = {
							mirror = false,
						},
					},
					file_sorter = require("telescope.sorters").get_fuzzy_file,
					file_ignore_patterns = {},
					generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,
					winblend = 0,
					border = {},
					borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
					color_devicons = true,
					use_less = true,
					path_display = {},
					set_env = { ["COLORTERM"] = "truecolor" }, -- default = nil,
					file_previewer = require("telescope.previewers").vim_buffer_cat.new,
					grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
					qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
					-- Developer configurations: Not meant for general override
					buffer_previewer_maker = require("telescope.previewers").buffer_previewer_maker,
					extensions = {
						fzf = {
							fuzzy = true,
							override_generic_sorter = true,
							override_file_sorter = true,
							case_mode = "smart_case",
						},
						file_browser = {
							hijack_netrw = true,
						},
					},
				},
			})
			require("telescope").load_extension("fzf")
			require("telescope").load_extension("file_browser")
		end,
	})

	-- General code plugins
	use({
		"ray-x/lsp_signature.nvim",
		config = function()
			require("lsp_signature").setup({})
		end,
	})

	use({
		"danymat/neogen",
		config = function()
			require("neogen").setup({})
		end,
		requires = "nvim-treesitter/nvim-treesitter",
	})

	use({
		"kevinhwang91/nvim-ufo",
		requires = "kevinhwang91/promise-async",
		config = function()
			vim.o.foldcolumn = "1"
			vim.o.foldlevel = 99
			vim.o.foldlevelstart = 99
			vim.o.foldenable = true

			vim.keymap.set("n", "zR", require("ufo").openAllFolds, {})
			vim.keymap.set("n", "zM", require("ufo").closeAllFolds, {})

			require("ufo").setup({
				provider_selector = function()
					return { "treesitter", "indent" }
				end,
			})
		end,
	})

	use({
		"kylechui/nvim-surround",
		config = function()
			require("nvim-surround").setup({})
		end,
	})

	use({ "gpanders/editorconfig.nvim" })

	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
	use({ "nvim-telescope/telescope-file-browser.nvim" })
	use({ "nvim-telescope/telescope.nvim", requires = { { "nvim-lua/plenary.nvim" } } })

	use({
		"nvim-treesitter/nvim-treesitter",
		run = ":TSUpdate",
		config = function()
			require("nvim-treesitter.configs").setup({
				ensure_installed = "all",
				highlight = {
					enable = true,
					additional_vim_regex_highlighting = { "org" },
				},
				indent = {
					enable = true,
				},
			})

			local parser_configs = require("nvim-treesitter.parsers").get_parser_configs()
			parser_configs.http = {
				install_info = {
					url = "https://github.com/NTBBloodbath/tree-sitter-http",
					files = { "src/parser.c" },
					branch = "main",
				},
			}
		end,
	})

	-- Terminal
	use({
		"akinsho/toggleterm.nvim",
		tag = "v2.*",
		config = function()
			require("toggleterm").setup()
		end,
	})

	-- Tooling
	use({
		"nvim-neo-tree/neo-tree.nvim",
		branch = "v2.x",
		requires = {
			"nvim-lua/plenary.nvim",
			"kyazdani42/nvim-web-devicons", -- not strictly required, but recommended
			"MunifTanjim/nui.nvim",
		},
		config = function()
			vim.cmd([[ let g:neo_tree_remove_legacy_commands = 1 ]])
			require("neo-tree").setup({
				sync_root_with_cwd = true,
				respect_buf_cwd = true,
				update_focused_file = {
					enable = true,
					update_root = true,
				},
			})
		end,
	})
	use({
		"NTBBloodbath/rest.nvim",
		requires = { "nvim-lua/plenary.nvim" },
		config = function()
			require("rest-nvim").setup({
				-- Open request results in a horizontal split
				result_split_horizontal = false,
				-- Skip SSL verification, useful for unknown certificates
				skip_ssl_verification = true,
				-- Highlight request on run
				highlight = {
					enabled = true,
					timeout = 150,
				},
				-- Jump to request line on run
				jump_to_request = false,
			})
			vim.cmd("autocmd FileType http nmap <buffer> <Enter> <Plug>RestNvim")
		end,
	})
	use({ "eshion/vim-sync" })
	use({
		"folke/which-key.nvim",
		config = function()
			require("which-key").setup()
		end,
	})
	use({ "jghauser/mkdir.nvim" })

	use({
		"f-person/git-blame.nvim",
		config = function()
			vim.g.gitblame_enabled = 0
		end,
	})

	use({
		"lewis6991/gitsigns.nvim",
		requires = {
			"nvim-lua/plenary.nvim",
		},
		config = function()
			require("gitsigns").setup()
		end,
	})
	use({
		"TimUntersberger/neogit",
		requires = "nvim-lua/plenary.nvim",
		config = function()
			require("neogit").setup({
				use_magit_keybindings = true,
				disable_commit_confirmation = true,
			})
		end,
	})
	use({
		"ldelossa/gh.nvim",
		requires = { { "ldelossa/litee.nvim" } },
		config = function()
			require("litee.lib").setup()
			require("litee.gh").setup({
				-- deprecated, around for compatability for now.
				jump_mode = "invoking",
				-- remap the arrow keys to resize any litee.nvim windows.
				map_resize_keys = false,
				-- do not map any keys inside any gh.nvim buffers.
				disable_keymaps = false,
				-- the icon set to use.
				icon_set = "default",
				-- any custom icons to use.
				icon_set_custom = nil,
				-- whether to register the @username and #issue_number omnifunc completion
				-- in buffers which start with .git/
				git_buffer_completion = true,
				-- defines keymaps in gh.nvim buffers.
				keymaps = {
					-- when inside a gh.nvim panel, this key will open a node if it has
					-- any futher functionality. for example, hitting <CR> on a commit node
					-- will open the commit's changed files in a new gh.nvim panel.
					open = "<CR>",
					-- when inside a gh.nvim panel, expand a collapsed node
					expand = "zo",
					-- when inside a gh.nvim panel, collpased and expanded node
					collapse = "zc",
					-- when cursor is over a "#1234" formatted issue or PR, open its details
					-- and comments in a new tab.
					goto_issue = "gd",
					-- show any details about a node, typically, this reveals commit messages
					-- and submitted review bodys.
					details = "d",
					-- inside a convo buffer, submit a comment
					submit_comment = "<C-s>",
					-- inside a convo buffer, when your cursor is ontop of a comment, open
					-- up a set of actions that can be performed.
					actions = "<C-a>",
					-- inside a thread convo buffer, resolve the thread.
					resolve_thread = "<C-r>",
					-- inside a gh.nvim panel, if possible, open the node's web URL in your
					-- browser. useful particularily for digging into external failed CI
					-- checks.
					goto_web = "gx",
				},
			})
		end,
	})
end)
