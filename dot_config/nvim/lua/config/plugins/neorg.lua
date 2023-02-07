return {
    "nvim-neorg/neorg",
    dependencies = { { "nvim-lua/plenary.nvim" } },
    build = ":Neorg sync-parsers",
    config = function()
        require("neorg").setup({
            load = {
                ["core.defaults"] = {}, -- Loads default behaviour
                ["core.norg.concealer"] = {}, -- Adds pretty icons to your documents
                ["core.norg.journal"] = {
                    config = {
                        strategy = "flat",
                        workspace = "notes"
                    }
                },
                ["core.norg.dirman"] = { -- Manages Neorg workspaces
                    config = {
                        default_workspace = "notes",
                        workspaces = {
                            notes = "~/Documents/wiki/neorg",
                        },
                    },
                },
                ["core.presenter"] = {
                    config = {
                        zen_mode = "zen-mode",
                    },
                },
                ["core.norg.manoeuvre"] = {},
            },
        })
    end,
}
