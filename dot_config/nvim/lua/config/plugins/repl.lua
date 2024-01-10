local bunRepl = {
    command = { "bun", "repl" },
}

return {
    "vigemus/iron.nvim",
    config = function()
        require("iron.core").setup({
            config = {
                scratch_repl = true,
                repl_definition = {
                    sh = {
                        command = { "bash" },
                    },
                    lua = {
                        command = { "lua" },
                    },
                    php = {
                        command = { "psysh" },
                    },
                    javascript = bunRepl,
                    typescript = bunRepl,
                    typescriptreact = bunRepl,
                    haskell = {
                        command = { "stack", "repl" },
                    },
                },
                repl_open_cmd = "horizontal 20 split",
            },
            keymaps = {
                visual_send = "<space>rs",
                send_file = "<space>rb",
                send_line = "<space>rl",
                interrupt = "<ctrl>c",
            },
        })
    end,
}
