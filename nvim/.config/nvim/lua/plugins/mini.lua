return {
	"echasnovski/mini.nvim",
	version = false,
	config = function()
		require("mini.basics").setup()
		require("mini.files").setup()
		require("mini.pick").setup({
			mappings = {
				move_down = "<C-j>",
				move_up = "<C-k>",
			},
		})
		require("mini.extra").setup()
		require("mini.surround").setup()
		require("mini.icons").setup()
		require("mini.sessions").setup()
		require("mini.snippets").setup({
      mappings = {
        jump_next = "<tab>",
        jump_prev = "<S-tab>"
      }
    })
    require('mini.pairs').setup()

		local miniclue = require("mini.clue")
		miniclue.setup({
			triggers = {
				-- Leader triggers
				{ mode = "n", keys = "<Leader>" },
				{ mode = "x", keys = "<Leader>" },

				-- Local leader
				{ mode = "n", keys = "<localleader>" },
				{ mode = "x", keys = "<loacalleader>" },

				-- Built-in completion
				{ mode = "i", keys = "<C-x>" },

				-- `g` key
				{ mode = "n", keys = "g" },
				{ mode = "x", keys = "g" },

				-- Marks
				{ mode = "n", keys = "'" },
				{ mode = "n", keys = "`" },
				{ mode = "x", keys = "'" },
				{ mode = "x", keys = "`" },

				-- Registers
				{ mode = "n", keys = '"' },
				{ mode = "x", keys = '"' },
				{ mode = "i", keys = "<C-r>" },
				{ mode = "c", keys = "<C-r>" },

				-- Window commands
				{ mode = "n", keys = "<C-w>" },

				-- `z` key
				{ mode = "n", keys = "z" },
				{ mode = "x", keys = "z" },
			},

			clues = {
				-- Enhance this by adding descriptions for <Leader> mapping groups
				miniclue.gen_clues.builtin_completion(),
				miniclue.gen_clues.g(),
				miniclue.gen_clues.marks(),
				miniclue.gen_clues.registers(),
				miniclue.gen_clues.windows(),
				miniclue.gen_clues.z(),
			},
		})

		local icons = require("mini.icons")
		icons.tweak_lsp_kind()
	end,
}
