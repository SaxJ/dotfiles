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
			require("mini.surround").setup({})
			require("mini.comment").setup({})
			require("mini.cursorword").setup({})
			require("mini.indentscope").setup({})
			require("mini.jump").setup({})
			require("mini.pairs").setup({})
		end,
	})
	use("folke/lua-dev.nvim")

	-- Appearance
	use("folke/tokyonight.nvim")
	use({
		"hoob3rt/lualine.nvim",
		requires = { "kyazdani42/nvim-web-devicons", opt = true },
	})

	-- Extra Languages
	use("amadeus/vim-mjml")
	use("rescript-lang/vim-rescript")
	use({
		"folke/todo-comments.nvim",
		requires = "nvim-lua/plenary.nvim",
		config = function()
			require("todo-comments").setup({})
		end,
	})
	use("adamclerk/vim-razor")
	use("jparise/vim-graphql")
	use("norcalli/nvim-colorizer.lua")

	-- LSP
	use({
		"glepnir/lspsaga.nvim",
		config = function()
			local saga = require("lspsaga")
			saga.init_lsp_saga()
		end,
	})
	use({
		"junnplus/nvim-lsp-setup",
		requires = {
			"neovim/nvim-lspconfig",
			"williamboman/nvim-lsp-installer",
			"hrsh7th/vim-vsnip",
		},
		config = function()
			require("nvim-lsp-setup").setup({
				default_mappings = true,
				gr = ":Lspsaga signature_help<CR>",
				gi = "lua require('goto-preview').goto_preview_implementation()",
				K = ":Lspsaga hover_doc<CR>",
				gs = ":Lspsaga signature_help<CR>",
				gd = ":Lspsaga preview_definition<CR>",
				gD = ":Lspsaga lsp_finder<CR>",
				on_attach = function(client, bufnr)
					--require('nvim-lsp-setup.utils').format_on_save(client)
				end,
				capabilities = vim.lsp.protocol.make_client_capabilities(),
				servers = {
					sumneko_lua = require("lua-dev").setup(),
					tsserver = {
						init_options = {
							preferences = {
								importModuleSpecifierPreference = "relative",
							},
						},
					},
					intelephense = {
						init_options = {
							licenceKey = "/home/saxonj/intelephense/licence.txt",
						},
					},
					jsonls = {
						schemas = require("schemastore").json.schemas(),
						validate = { enable = true },
					},
					omnisharp = {
						cmd = {
							"/usr/bin/omnisharp",
							"-lsp",
							"-v",
							"--hostPID",
							tostring(vim.fn.getpid()),
						},
					},
					elmls = { { cmd = "elm-language-server" } },
				},
			})
		end,
	})
	use({
		"rmagatti/goto-preview",
		config = function()
			require("goto-preview").setup({})
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
		end,
	})

	-- General code plugins
	use("gpanders/editorconfig.nvim")
	use("mhartington/formatter.nvim")
	use({
		"hrsh7th/nvim-cmp",
		requires = {
			"hrsh7th/vim-vsnip",
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-nvim-lua",
			"hrsh7th/cmp-nvim-lsp",
		},
	})
	use({ "nvim-telescope/telescope.nvim", requires = { { "nvim-lua/plenary.nvim" } } })
	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
	use("airblade/vim-rooter")
	use({
		"nvim-treesitter/nvim-treesitter",
		run = ":TSUpdate",
		config = function()
			require("nvim-treesitter.configs").setup({
				ensure_installed = "all",
				highlight = {
					enable = true,
				},
				indent = {
					enable = true,
				},
			})
		end,
	})
	use("nvim-treesitter/nvim-treesitter-context")

	-- Debuggers
	use("mfussenegger/nvim-dap")
	use("theHamsta/nvim-dap-virtual-text")
	use({ "rcarriga/nvim-dap-ui", requires = { "mfussenegger/nvim-dap" } })

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
			require("neo-tree").setup({})
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
	use({
		"nvim-neorg/neorg",
		requires = "nvim-lua/plenary.nvim",
	})
	use("eshion/vim-sync")
	use({
		"folke/which-key.nvim",
		config = function()
			require("which-key").setup()
		end,
	})
	use({
		"glacambre/firenvim",
		run = function()
			vim.fn["firenvim#install"](0)
		end,
	})
	use("jghauser/mkdir.nvim")

	-- Code navigation
	use({
		"ThePrimeagen/harpoon",
		requires = { "nvim-lua/plenary.nvim" },
	})
	use({
		"ahmedkhalf/project.nvim",
		config = function()
			require("project_nvim").setup({})
		end,
	})

	-- Version Control
	use({
		"ldelossa/gh.nvim",
		requires = { { "ldelossa/litee.nvim" } },
		config = function()
			require("litee.lib").setup()
			require("litee.gh").setup({
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
		"ruifm/gitlinker.nvim",
		requires = "nvim-lua/plenary.nvim",
		config = function()
			require("gitlinker").setup()
		end,
	})
end)
