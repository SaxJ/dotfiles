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
                        workspace = "journal"
                    }
                },
                ["core.norg.dirman"] = { -- Manages Neorg workspaces
                    config = {
                        default_workspace = "journal",
                        workspaces = {
                            journal = "~/journal",
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
