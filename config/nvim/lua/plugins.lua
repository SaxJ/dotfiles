local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({"git", "clone", "https://github.com/wbthomason/packer.nvim", install_path})
  execute "packadd packer.nvim"
end

vim.cmd [[packadd packer.nvim]]

return require("packer").startup(
  function()
    -- Packer can manage itself
    use "wbthomason/packer.nvim"

    -- Libraries
    use "b0o/mapx.nvim"

    -- Appearance
    use "folke/tokyonight.nvim"
    use {
      "hoob3rt/lualine.nvim",
      requires = {"kyazdani42/nvim-web-devicons", opt = true}
    }

    -- Syntax
    use "amadeus/vim-mjml"
    use "b3nj5m1n/kommentary"
    use {
      "folke/todo-comments.nvim",
      requires = "nvim-lua/plenary.nvim",
      config = function()
        require("todo-comments").setup {}
      end
    }
    use "adamclerk/vim-razor"
    use "jparise/vim-graphql"
    use {"norcalli/nvim-colorizer.lua"}
    use {
      "nvim-treesitter/nvim-treesitter",
      run = ":TSUpdate",
      config = function()
        require "nvim-treesitter.configs".setup(
          {
            ensure_installed = "all",
            highlight = {
              enable = true
            },
            indent = {
              enable = true
            }
          }
        )
      end
    }

    -- General code plugins
    use "glepnir/lspsaga.nvim"
    use "mhartington/formatter.nvim"
    use {"neovim/nvim-lspconfig", requires = {{"hrsh7th/vim-vsnip"}}}
    use {"williamboman/nvim-lsp-installer"}
    use {
      "hrsh7th/nvim-cmp",
      requires = {
        "hrsh7th/vim-vsnip",
        "hrsh7th/cmp-buffer",
        "hrsh7th/cmp-nvim-lua",
        "hrsh7th/cmp-nvim-lsp"
      }
    }
    use {"nvim-telescope/telescope.nvim", requires = {{"nvim-lua/plenary.nvim"}}}
    use {
      "nvim-telescope/telescope-frecency.nvim",
      requires = {"tami5/sqlite.lua"}
    }
    use {
      "blackCauldron7/surround.nvim",
      config = function()
        require "surround".setup {mappings_style = "surround"}
      end
    }
    use {"nvim-telescope/telescope-fzf-native.nvim", run = "make"}

    -- Tooling
    use {
      "NTBBloodbath/rest.nvim",
      requires = {"nvim-lua/plenary.nvim"},
      config = function()
        require("rest-nvim").setup(
          {
            -- Open request results in a horizontal split
            result_split_horizontal = false,
            -- Skip SSL verification, useful for unknown certificates
            skip_ssl_verification = true,
            -- Highlight request on run
            highlight = {
              enabled = true,
              timeout = 150
            },
            -- Jump to request line on run
            jump_to_request = false
          }
        )
        vim.cmd "autocmd FileType http nmap <buffer> <Enter> <Plug>RestNvim"
      end
    }
    use {"voldikss/vim-floaterm"}
    use {'nvim-orgmode/orgmode', config = function()
            require('orgmode').setup{}
    end
    }

    -- Code navigation
    use {
      "phaazon/hop.nvim",
      as = "hop",
      config = function()
        -- you can configure Hop the way you like here; see :h hop-config
        require "hop".setup {keys = "etovxqpdygfblzhckisuran"}
      end
    }
    use {"ms-jpq/chadtree"}
    use {
      "ahmedkhalf/project.nvim",
      config = function()
        require("project_nvim").setup {
          silent_chdir = true
        }
      end
    }

    -- Hell yeah git
    use {"pwntester/octo.nvim"}
    use "kdheepak/lazygit.nvim"
    use {
      "f-person/git-blame.nvim",
      config = function()
        vim.g.gitblame_enabled = 0
      end
    }
    use {
      "lewis6991/gitsigns.nvim",
      requires = {
        "nvim-lua/plenary.nvim"
      },
      config = function()
        require("gitsigns").setup()
      end
    }
  end
)
