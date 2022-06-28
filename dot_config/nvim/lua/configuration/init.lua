require("configuration/completion")
require("configuration/formatting")
require("configuration/debugger")
require("lualine").setup({
    options = {
        theme = "auto",
        globalstatus = true,
    },
    sections = {
        lualine_c = {
            { "filename", path = 1 },
            {
                "diagnostics",
                -- table of diagnostic sources, available sources:
                -- nvim_lsp, coc, ale, vim_lsp
                sources = { "nvim_diagnostic" },
                -- displays diagnostics from defined severity
                sections = { "error", "warn", "info", "hint" },
                -- all colors are in format #rrggbb
                --color_error = nil, -- changes diagnostic's error foreground color
                --color_warn = nil, -- changes diagnostic's warn foreground color
                --color_info = nil, -- Changes diagnostic's info foreground color
                --color_hint = nil, -- Changes diagnostic's hint foreground color
                symbols = { error = "E", warn = "W", info = "I", hint = "H" },
            },
        },
    },
})
require("colorizer").setup()

-- treesitter parsers
local parser_configs = require("nvim-treesitter.parsers").get_parser_configs()
parser_configs.http = {
    install_info = {
        url = "https://github.com/NTBBloodbath/tree-sitter-http",
        files = { "src/parser.c" },
        branch = "main",
    },
}
parser_configs.org = {
    install_info = {
        url = "https://github.com/milisims/tree-sitter-org",
        files = { "src/parser.c", "src/scanner.cc" },
        revision = "f110024d539e676f25b72b7c80b0fd43c34264ef",
        filetype = "org",
    },
}

local actions = require("telescope.actions")
require("telescope").setup({
    defaults = {
        mappings = {
            i = {
                ["<C-j>"] = actions.move_selection_next,
                ["<C-k>"] = actions.move_selection_previous,
            },
        },
        vimgrep_arguments = {
            "rg",
            "--color=never",
            "--no-heading",
            "--with-filename",
            "--line-number",
            "--column",
            "--smart-case",
        },
        prompt_prefix = "> ",
        selection_caret = "> ",
        entry_prefix = "  ",
        initial_mode = "insert",
        selection_strategy = "reset",
        sorting_strategy = "descending",
        layout_strategy = "horizontal",
        layout_config = {
            horizontal = {
                mirror = false,
            },
            vertical = {
                mirror = false,
            },
        },
        file_sorter = require("telescope.sorters").get_fuzzy_file,
        file_ignore_patterns = {},
        generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,
        winblend = 0,
        border = {},
        borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
        color_devicons = true,
        use_less = true,
        path_display = {},
        set_env = { ["COLORTERM"] = "truecolor" }, -- default = nil,
        file_previewer = require("telescope.previewers").vim_buffer_cat.new,
        grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
        qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
        -- Developer configurations: Not meant for general override
        buffer_previewer_maker = require("telescope.previewers").buffer_previewer_maker,
        extensions = {
            fzf = {
                fuzzy = true,
                override_generic_sorter = true,
                override_file_sorter = true,
                case_mode = "smart_case",
            },
        },
    },
})
require("telescope").load_extension("fzf")
require('telescope').load_extension('harpoon')
require('telescope').load_extension('projects')

-- Neorg
require('neorg').setup {
    load = {
        ['core.defaults'] = {},
        ["core.norg.dirman"] = {
            config = {
                workspaces = {
                    work = "~/Documents/wiki",
                }
            }
        },
        ['core.norg.concealer'] = {},
        ['core.norg.completion'] = {
            config = {
                engine = 'nvim-cmp',
            }
        },
        ["core.norg.journal"] = {
            config = {
                strategy = 'flat',
                journal_folder = '~/Documents/wiki/journal',
            },
        },
        ["core.norg.manoeuvre"] = {},
        ["core.gtd.base"] = {
            config = {
                workspace = "work",
            },
        },
    }
}
