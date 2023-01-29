local deno = {
    command = { "deno" },
}

return {
    "hkupty/iron.nvim",
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
                    javascript = deno,
                    typescript = deno,
                    typescriptreact = deno,
                    haskell = {
                        command = { "stack", "repl" },
                    },
                },
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
