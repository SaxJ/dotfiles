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
		require("mini.snippets").setup()
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

		local completion = require("mini.completion")
		completion.setup({})
		local imap_expr = function(lhs, rhs)
			vim.keymap.set("i", lhs, rhs, { expr = true })
		end
		imap_expr("<Tab>", [[pumvisible() ? "\<C-n>" : "\<Tab>"]])
		imap_expr("<S-Tab>", [[pumvisible() ? "\<C-p>" : "\<S-Tab>"]])

		_G.cr_action = function()
			-- If there is selected item in popup, accept it with <C-y>
			if vim.fn.complete_info()["selected"] ~= -1 then
				return "\25"
			end
			-- Fall back to plain `<CR>`. You might want to customize according
			-- to other plugins. For example if 'mini.pairs' is set up, replace
			-- next line with `return MiniPairs.cr()`
      return MiniPairs.cr()
		end

		vim.keymap.set("i", "<CR>", "v:lua.cr_action()", { expr = true })
	end,
}
