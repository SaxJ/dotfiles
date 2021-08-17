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
    use "glepnir/lspsaga.nvim"
    use "mhartington/formatter.nvim"
    use {"neovim/nvim-lspconfig", requires = {{"hrsh7th/vim-vsnip"}, {"hrsh7th/vim-vsnip-integ"}}}
    use {"hrsh7th/nvim-compe"}
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

    -- Hell yeah git
    use {"TimUntersberger/neogit", requires = "nvim-lua/plenary.nvim"}

    -- org mode

    use {
      "kristijanhusak/orgmode.nvim",
      config = function()
        require("orgmode").setup {}
      end
    }
  end
)
