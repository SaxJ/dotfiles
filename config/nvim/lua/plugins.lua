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
    use "svermeulen/vimpeccable"

    -- Appearance
    use "joshdick/onedark.vim"
    use {
      "hoob3rt/lualine.nvim",
      requires = {"kyazdani42/nvim-web-devicons", opt = true}
    }

    -- Syntax
    use "jparise/vim-graphql"
    use "norcalli/nvim-colorizer.lua"

    -- General code plugins
    use {
      "jlesquembre/nterm.nvim",
      requires = {"Olical/aniseed"},
      config = function()
        require "nterm.main".init(
          {
            maps = true, -- load defaut mappings
            shell = "fish",
            size = 20,
            direction = "horizontal", -- horizontal or vertical
            popup = 2000, -- Number of miliseconds to show the info about the commmand. 0 to dissable
            popup_pos = "SE", --  one of "NE" "SE" "SW" "NW"
            autoclose = 2000 -- If command is sucesful, close the terminal after that number of miliseconds. 0 to disable
          }
        )
      end
    }
    --
    --
    use "glepnir/lspsaga.nvim"
    use "mhartington/formatter.nvim"
    use {"neovim/nvim-lspconfig", requires = {{"hrsh7th/vim-vsnip"}, {"hrsh7th/vim-vsnip-integ"}}}
    use {"ms-jpq/coq_nvim", branch = "coq"} -- main one
    use {"ms-jpq/coq.artifacts", branch = "artifacts"} -- 9000+ Snippets
    use {"nvim-telescope/telescope.nvim", requires = {{"nvim-lua/plenary.nvim"}}}
    use {
      "blackCauldron7/surround.nvim",
      config = function()
        require "surround".setup {}
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
    use {
      "kyazdani42/nvim-tree.lua",
      requires = "kyazdani42/nvim-web-devicons"
    }
    use {
      "ahmedkhalf/project.nvim",
      config = function()
        require("project_nvim").setup {}
      end
    }

    -- Hell yeah git
    use "kdheepak/lazygit.nvim"
    use {
      "pwntester/octo.nvim",
      config = function()
        require "octo".setup()
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
