return {
	"saghen/blink.cmp",
	lazy = false, -- lazy loading handled internally
	-- optional: provides snippets for the snippet source
	dependencies = "rafamadriz/friendly-snippets",

	-- use a release tag to download pre-built binaries
	version = "v0.*",

	---@module 'blink.cmp'
	---@type blink.cmp.Config
	opts = {
		-- 'default' for mappings similar to built-in completion
		-- 'super-tab' for mappings similar to vscode (tab to accept, arrow keys to navigate)
		-- 'enter' for mappings similar to 'super-tab' but with 'enter' to accept
		-- see the "default configuration" section below for full documentation on how to define
		-- your own keymap.
		keymap = {
			preset = "enter",
			["<Tab>"] = {
				function(cmp)
					if cmp.snippet_active() then
						cmp.snippet_forward()
					else
						cmp.select_next()
					end
				end,
			},
			["<S-Tab>"] = {
				function(cmp)
					if cmp.snippet_active() then
						cmp.snippet_backward()
					else
						cmp.select_prev()
					end
				end,
			},
		},

		completion = {
			list = {
				selection = {
          preselect = false,
          auto_insert = false,
        },
			},
		},
	},
}
