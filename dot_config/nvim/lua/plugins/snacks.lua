return {
  "folke/snacks.nvim",
  ---@type snacks.Config
  opts = {
    image = {
      enabled = true,
      resolve = function(file, src)
        return string.format("/home/saxonj/.emacs.d/emira_image_cache/%s", src)
      end,
      -- your image configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
    },
    input = {
      enabled = true,
      config = {
        os = {
          editPreset = "nvim-remote"
        }
      }
    },
    lazygit = {
      enabled = true,
      configure = true,
    },
    picker = {
      formatters = {
        file = {
          truncate = 70
        }
      }
    }
  },
}
