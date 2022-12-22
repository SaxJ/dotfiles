return {
	"nvim-lualine/lualine.nvim",
	dependencies = { "kyazdani42/nvim-web-devicons", opt = true },
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
                    {
                        require("lazy.status").updates,
                        cond = require("lazy.status").has_updates,
                        color = { fg = "#ff9e64" },
                    },
					"encoding",
					"fileformat",
					"filetype",
				},
			},
		})
	end,
}
