return {
	"echasnovski/mini.nvim",
	version = false,
	config = function()
		require("mini.ai").setup()
		require("mini.basics").setup()
		require("mini.files").setup()
		require("mini.surround").setup()
		require("mini.icons").setup()
		-- require("mini.pairs").setup()
		require("mini.statusline").setup()
		-- require("mini.pick").setup({
		-- 	mappings = {
		-- 		move_up = "<C-k>",
		-- 		move_down = "<C-j>",
		-- 	},
		-- })

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

		-- -- Tab to change completion
		-- local imap_expr = function(lhs, rhs)
		-- 	vim.keymap.set("i", lhs, rhs, { expr = true })
		-- end
		-- imap_expr("<Tab>", [[pumvisible() ? "\<C-n>" : "\<Tab>"]])
		-- imap_expr("<S-Tab>", [[pumvisible() ? "\<C-p>" : "\<S-Tab>"]])
		--
		-- local keycode = vim.keycode or function(x)
		-- 	return vim.api.nvim_replace_termcodes(x, true, true, true)
		-- end
		-- local keys = {
		-- 	["cr"] = keycode("<CR>"),
		-- 	["ctrl-y"] = keycode("<C-y>"),
		-- 	["ctrl-y_cr"] = keycode("<C-y><CR>"),
		-- }
		--
		-- _G.cr_action = function()
		-- 	if vim.fn.pumvisible() ~= 0 then
		-- 		-- If popup is visible, confirm selected item or add new line otherwise
		-- 		local item_selected = vim.fn.complete_info()["selected"] ~= -1
		-- 		return item_selected and keys["ctrl-y"] or keys["ctrl-y_cr"]
		-- 	else
		-- 		-- If popup is not visible, use plain `<CR>`. You might want to customize
		-- 		-- according to other plugins. For example, to use 'mini.pairs', replace
		-- 		-- next line with `return require('mini.pairs').cr()`
		-- 		return keys["cr"]
		-- 	end
		-- end
		--
		-- vim.keymap.set("i", "<CR>", "v:lua._G.cr_action()", { expr = true })
	end,
}
