return {
	"nvim-neorg/neorg",
	build = ":Neorg sync-parsers",
	opts = {
		load = {
			["core.defaults"] = {}, -- Loads default behaviour
			["core.norg.concealer"] = {}, -- Adds pretty icons to your documents
			["core.norg.journal"] = {
				config = {
					strategy = "flat",
					workspace = "journal"
				},
			},
			["core.norg.dirman"] = { -- Manages Neorg workspaces
				config = {
					workspaces = {
						notes = "~/Documents/wiki/neorg/notes",
						journal = "~/Documents/wiki/neorg/journal",
					},
					default_workspace = 'notes',
				},
			},
			["core.norg.completion"] = {
				config = {
					engine = "nvim-cmp",
				}
			},
			["core.presenter"] = {
				config = {
					zen_mode = "zen-mode",
				}
			},
		},
	},
	dependencies = { { "nvim-lua/plenary.nvim" } },
}
