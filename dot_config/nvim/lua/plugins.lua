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
		'echasnovski/mini.nvim',
		config = function()
			require('mini.surround').setup({})
			require('mini.comment').setup({})
			require('mini.cursorword').setup({})
			require('mini.indentscope').setup({})
			require('mini.jump').setup({})
			require('mini.pairs').setup({})
		end
	})
	use "folke/lua-dev.nvim"

	-- Appearance
	use("folke/tokyonight.nvim")
	use({
		"hoob3rt/lualine.nvim",
		requires = { "kyazdani42/nvim-web-devicons", opt = true },
	})

	-- Extra Languages
	use("amadeus/vim-mjml")
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
		"tami5/lspsaga.nvim",
		config = function()
			local saga = require('lspsaga')
			saga.setup({})
		end
	})
	use({ 'junnplus/nvim-lsp-setup',
		requires = {
			'neovim/nvim-lspconfig',
			'williamboman/nvim-lsp-installer',
			'hrsh7th/vim-vsnip',
		},
		config = function()
			require('nvim-lsp-setup').setup({
				default_mappings = true,
				gr = "lua require('goto-preview').goto_preview_references()",
				gi = "lua require('goto-preview').goto_preview_implementation()",
				on_attach = function(client, bufnr)
					require('nvim-lsp-setup.utils').format_on_save(client)
				end,
				capabilities = vim.lsp.protocol.make_client_capabilities(),
				servers = {
					sumneko_lua = require('lua-dev').setup(),
					tsserver = {
						init_options = {
							preferences = {
								importModuleSpecifierPreference = "relative"
							}
						}
					},
					intelephense = {
						init_options = {
							licenceKey = '/home/saxonj/intelephense/licence.txt'
						}
					},
					jsonls = {
						schemas = require('schemastore').json.schemas(),
						validate = { enable = true },
					},
				}
			})
		end
	})
	use {
		'rmagatti/goto-preview',
		config = function()
			require('goto-preview').setup({})
		end
	}
	use('b0o/schemastore.nvim')
	use {
		"someone-stole-my-name/yaml-companion.nvim",
		requires = {
			{ "neovim/nvim-lspconfig" },
			{ "nvim-lua/plenary.nvim" },
			{ "nvim-telescope/telescope.nvim" },
		},
		config = function()
			require("telescope").load_extension("yaml_schema")
		end,
	}

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
	use('airblade/vim-rooter')
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
	use { "akinsho/toggleterm.nvim", tag = 'v2.*', config = function()
		require("toggleterm").setup()
	end }

	-- Tooling
	use {
		"luukvbaal/nnn.nvim",
		config = function() require("nnn").setup() end
	}
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
	use {
		'glacambre/firenvim',
		run = function() vim.fn['firenvim#install'](0) end
	}
	use('jghauser/mkdir.nvim')

	-- Code navigation
	use {
		'ThePrimeagen/harpoon',
		requires = { 'nvim-lua/plenary.nvim' }
	}
	use({
		'ahmedkhalf/project.nvim',
		config = function()
			require('project_nvim').setup({})
		end
	})

	-- Version Control
	use({
		'TimUntersberger/neogit',
		requires = {
			'nvim-lua/plenary.nvim',
			'sindrets/diffview.nvim',
		},
		config = function()
			require('neogit').setup({
				integrations = {
					diffview = true
				}
			})
		end
	})
	use({ "pwntester/octo.nvim" })
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
	use {
		'ruifm/gitlinker.nvim',
		requires = 'nvim-lua/plenary.nvim',
		config = function()
			require "gitlinker".setup()
		end
	}
end)
