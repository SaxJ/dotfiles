local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
	fn.system({ "git", "clone", "https://github.com/wbthomason/packer.nvim", install_path })
	execute("packadd packer.nvim")
end

vim.cmd([[packadd packer.nvim]])

return require("packer").startup(function(use)
	-- Packer can manage itself
	use ("wbthomason/packer.nvim")

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

	-- Syntax
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
	use({ "norcalli/nvim-colorizer.lua" })

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

	-- General code plugins
	use({ "gpanders/editorconfig.nvim" })
	use({ "tami5/lspsaga.nvim" })
	use("mhartington/formatter.nvim")
	use({ "neovim/nvim-lspconfig", requires = { { "hrsh7th/vim-vsnip" } } })
	use({ "williamboman/nvim-lsp-installer" })
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
	use({
		"nvim-telescope/telescope-frecency.nvim",
		requires = { "tami5/sqlite.lua" },
	})
	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
	use 'airblade/vim-rooter'

	-- Debuggers
	use("mfussenegger/nvim-dap")
	use("theHamsta/nvim-dap-virtual-text")
	use({ "rcarriga/nvim-dap-ui", requires = { "mfussenegger/nvim-dap" } })

	-- Tooling
	use 'voldikss/vim-floaterm'
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
	use({
		"s1n7ax/nvim-terminal",
		config = function()
			vim.o.hidden = true
			require("nvim-terminal").setup({
				toggle_keymap = "<leader>tt",
			})
		end,
	})
	use("eshion/vim-sync")
	use({
		"folke/which-key.nvim",
		config = function()
			require("which-key").setup()
		end,
	})

	-- Code navigation
    use({
        'kyazdani42/nvim-tree.lua',
        requires = { 'kyazdani42/nvim-web-devicons' },
    })
    use {
        'cljoly/telescope-repo.nvim',
        requires = { 'nvim-telescope/telescope.nvim', 'nvim-lua/plenary.nvim'},
    }

	-- Hell yeah git
	use({ "pwntester/octo.nvim" })
	use({
		"TimUntersberger/neogit",
		requires = {"nvim-lua/plenary.nvim", "sindrets/diffview.nvim"},
		config = function()
			require("neogit").setup({
			    use_magit_keybindings = true,
			    integrations = {diffview = true},
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
end)
