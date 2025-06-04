return {
  "folke/snacks.nvim",
  ---@type snacks.Config
  opts = {
    image = {
      enabled = false,
      resolve = function(file, src)
        return string.format("/home/saxonj/.emacs.d/emira_image_cache/%s", src)
      end,
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
