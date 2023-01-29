return {
    "nvim-neorg/neorg",
    build = ":Neorg sync-parsers",
    opts = {
        load = {
            ["core.defaults"] = {}, -- Loads default behaviour
            ["core.norg.concealer"] = {}, -- Adds pretty icons to your documents
            ["core.norg.dirman"] = { -- Manages Neorg workspaces
                config = {
                    workspaces = {
                        notes = "~/Documents/wiki/neorg",
                    },
                },
            },
            ['core.presenter'] = {},
            ["core.norg.manoeuvre"] = {},
        },
    },
    dependencies = { { "nvim-lua/plenary.nvim" } },
}
