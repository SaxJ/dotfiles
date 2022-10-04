local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({ "git", "clone", "https://github.com/wbthomason/packer.nvim", install_path })
    execute("packadd packer.nvim")
end

vim.cmd("packadd packer.nvim")

-- Package installation
return require("packer").startup(function(use)
    -- Packer can manage itself
    use("wbthomason/packer.nvim")

    -- Libraries
    use("b0o/mapx.nvim")
    use({
        "echasnovski/mini.nvim",
        config = function()
            require("mini.comment").setup({})
            require("mini.cursorword").setup({})
            require("mini.jump").setup({})
            require("mini.pairs").setup({})
        end,
    })
    use("folke/lua-dev.nvim")
    use("arkav/lualine-lsp-progress")

    -- Appearance
    use({ "kartikp10/noctis.nvim", requires = { "rktjmp/lush.nvim" } })
    use({
        "nvim-lualine/lualine.nvim",
        requires = { "kyazdani42/nvim-web-devicons", opt = true },
        config = function()
            require("lualine").setup({
                options = {
                    theme = "noctis",
                    globalstatus = false,
                },
                sections = {
                    lualine_c = {
                        { "filename", path = 1 },
                        { "lsp_progress" },
                    },
                    lualine_x = {
                        "encoding",
                        "fileformat",
                        "filetype",
                    },
                },
            })
        end,
    })
    use({
        "gelguy/wilder.nvim",
        config = function()
            local wilder = require("wilder")
            wilder.setup({ modes = { ":", "/", "?" } })
        end,
    })

    -- Languages
    use({
        "mhartington/formatter.nvim",
        config = function()
            require("formatter_config")
        end,
    })
    use("amadeus/vim-mjml")
    use("adamclerk/vim-razor")
    use("jparise/vim-graphql")
    use({
        "norcalli/nvim-colorizer.lua",
        config = function()
            require("colorizer").setup()
        end,
    })
    use("pbrisbin/vim-syntax-shakespeare")
    use({
        "lukas-reineke/indent-blankline.nvim",
        config = function()
            require("indent_blankline").setup({
                show_current_context = true,
                show_current_context_start = true,
            })
        end,
    })

    -- LSP
    use("kyazdani42/nvim-web-devicons")
    use({
        "folke/trouble.nvim",
        requires = "kyazdani42/nvim-web-devicons",
        config = function()
            require("trouble").setup({})
        end,
    })
    use({
        "onsails/lspkind.nvim",
        config = function()
            require("lspkind").init({})
        end,
    })
    use({
        "hrsh7th/nvim-cmp",
        requires = {
            { "hrsh7th/nvim-cmp" },
            { "hrsh7th/cmp-buffer" },
            { "hrsh7th/cmp-path" },
            { "hrsh7th/cmp-nvim-lsp" },
            { "hrsh7th/cmp-nvim-lua" },
            { "L3MON4D3/LuaSnip" },
            { "saadparwaiz1/cmp_luasnip" },
        },
        config = function()
            local cmp = require("cmp")
            local lspkind = require("lspkind")

            cmp.setup({
                snippet = {
                    expand = function(args)
                        require("luasnip").lsp_expand(args.body)
                    end,
                },
                mapping = {
                    ["<Tab>"] = cmp.mapping(cmp.mapping.select_next_item()),
                    ["<S-Tab>"] = cmp.mapping(cmp.mapping.select_prev_item()),
                    ["<C-Space>"] = cmp.mapping.complete(),
                    ["<CR>"] = cmp.mapping.confirm({
                        behavior = cmp.ConfirmBehavior.Replace,
                        select = true,
                    }),
                },
                sources = {
                    { name = "nvim_lsp" },
                    { name = "luasnip" },
                    { name = "orgmode" },
                    { name = "buffer" },
                },
                formatting = {
                    format = lspkind.cmp_format({
                        mode = "symbol",
                        maxwidth = 50,
                    }),
                },
            })
        end,
    })
    use({
        "junnplus/lsp-setup.nvim",
        requires = {
            "neovim/nvim-lspconfig",
            "williamboman/mason.nvim",
            "williamboman/mason-lspconfig.nvim",
        },
        config = function()
            require("lsp-setup").setup({
                servers = {
                    sumneko_lua = require("lua-dev").setup(),
                    tsserver = {
                        init_options = {
                            preferences = {
                                importModuleSpecifierPreference = "relative",
                            },
                        },
                    },
                    intelephense = {
                        init_options = {
                            licenceKey = "/home/saxonj/intelephense/licence.txt",
                        },
                    },
                    jsonls = {
                        schemas = require("schemastore").json.schemas(),
                        validate = { enable = true },
                    },
                    omnisharp = {
                        cmd = {
                            "/usr/bin/omnisharp",
                            "-lsp",
                            "-v",
                            "--hostPID",
                            tostring(vim.fn.getpid()),
                        },
                    },
                    elmls = { { cmd = "elm-language-server" } },
                    hls = { { cmd = "haskell-language-server-wrapper" } },
                    pylsp = {
                        {
                            cmd = "pylsp",
                            filetypes = "python",
                            root_dir = function(fname)
                                local root_files = {
                                    "requirements.txt",
                                }
                                return require("lspconfig.util").root_pattern(unpack(root_files))(fname)
                                    or require("lspconfig.util").find_git_ancestor(fname)
                            end,
                            single_file_support = true,
                        },
                    },
                },
            })
        end,
    })
    use({
        "glepnir/lspsaga.nvim",
        config = function()
            local saga = require("lspsaga")
            saga.init_lsp_saga()
        end,
    })
    use({
        "rmagatti/goto-preview",
        config = function()
            require("goto-preview").setup({})
        end,
    })
    use("b0o/schemastore.nvim")
    use({
        "someone-stole-my-name/yaml-companion.nvim",
        requires = {
            { "neovim/nvim-lspconfig" },
            { "nvim-lua/plenary.nvim" },
            { "nvim-telescope/telescope.nvim" },
        },
        config = function()
            require("telescope").load_extension("yaml_schema")

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
                        file_browser = {
                            hijack_netrw = true,
                        },
                    },
                },
            })
            require("telescope").load_extension("fzf")
            require("telescope").load_extension("projects")
            require("telescope").load_extension("file_browser")
        end,
    })

    -- General code plugins
    use({
        "ray-x/lsp_signature.nvim",
        config = function()
            require("lsp_signature").setup({})
        end,
    })
    use({
        "danymat/neogen",
        config = function()
            require("neogen").setup({})
        end,
        requires = "nvim-treesitter/nvim-treesitter",
    })
    use({
        "kevinhwang91/nvim-ufo",
        requires = "kevinhwang91/promise-async",
        config = function()
            vim.o.foldcolumn = "1"
            vim.o.foldlevel = 99
            vim.o.foldlevelstart = 99
            vim.o.foldenable = true

            vim.keymap.set("n", "zR", require("ufo").openAllFolds, {})
            vim.keymap.set("n", "zM", require("ufo").closeAllFolds, {})

            require("ufo").setup({
                provider_selector = function()
                    return { "treesitter", "indent" }
                end,
            })
        end,
    })
    use({
        "kylechui/nvim-surround",
        config = function()
            require("nvim-surround").setup({})
        end,
    })
    use({ "gpanders/editorconfig.nvim" })
    use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
    use({ "nvim-telescope/telescope-file-browser.nvim" })
    use({ "nvim-telescope/telescope.nvim", requires = { { "nvim-lua/plenary.nvim" } } })

    -- Project Management
    use({ "airblade/vim-rooter" })
    use({
        "nvim-treesitter/nvim-treesitter",
        run = ":TSUpdate",
        config = function()
            require("nvim-treesitter.configs").setup({
                ensure_installed = "all",
                highlight = {
                    enable = true,
                    additional_vim_regex_highlighting = { "org" },
                },
                indent = {
                    enable = true,
                },
            })

            local parser_configs = require("nvim-treesitter.parsers").get_parser_configs()
            parser_configs.http = {
                install_info = {
                    url = "https://github.com/NTBBloodbath/tree-sitter-http",
                    files = { "src/parser.c" },
                    branch = "main",
                },
            }
        end,
    })

    -- Notes
    use({
        "phaazon/mind.nvim",
        branch = "v2.2",
        requires = { "nvim-lua/plenary.nvim" },
        config = function()
            require("mind").setup()
        end,
    })
    use({
        "nvim-orgmode/orgmode",
        config = function()
            require("orgmode").setup_ts_grammar()
            require("orgmode").setup({
                org_agenda_files = { "~/Documents/wiki/**/*.org" },
                org_default_notes_file = "~/Documents/wiki/notes.org",
                org_todo_keywords = { "TODO(t)", "PROG", "BLOCKED", "REVIEW", "|", "DONE" },
                org_capture_templates = {
                    t = {
                        description = "Todo",
                        template = "* TODO [#%^{A|B|C}] %? %t",
                        target = "~/Documents/wiki/todo.org",
                    },
                },
                mappings = {
                    org = {
                        org_todo = "<localleader>t",
                        org_priority = "<localleader>p",
                    },
                },
            })
        end,
    })
    use({
        "akinsho/org-bullets.nvim",
        config = function()
            require("org-bullets").setup({})
        end,
    })
    use({ "dhruvasagar/vim-table-mode" })

    -- Debuggers
    use("mfussenegger/nvim-dap")
    use({
        "theHamsta/nvim-dap-virtual-text",
        config = function()
            require("nvim-dap-virtual-text").setup({
                enabled = true, -- enable this plugin (the default)
                enabled_commands = true, -- create commands DapVirtualTextEnable, DapVirtualTextDisable, DapVirtualTextToggle, (DapVirtualTextForceRefresh for refreshing when debug adapter did not notify its termination)
                highlight_changed_variables = true, -- highlight changed values with NvimDapVirtualTextChanged, else always NvimDapVirtualText
                highlight_new_as_changed = false, -- highlight new variables in the same way as changed variables (if highlight_changed_variables)
                show_stop_reason = true, -- show stop reason when stopped for exceptions
                commented = false, -- prefix virtual text with comment string
                -- experimental features:
                virt_text_pos = "eol", -- position of virtual text, see `:h nvim_buf_set_extmark()`
                all_frames = false, -- show virtual text for all stack frames not only current. Only works for debugpy on my machine.
                virt_lines = false, -- show virtual lines instead of virtual text (will flicker!)
                virt_text_win_col = nil, -- position the virtual text at a fixed window column (starting from the first text column) ,
                -- e.g. 80 to position at column 80, see `:h nvim_buf_set_extmark()`
            })
        end,
    })
    use({
        "rcarriga/nvim-dap-ui",
        requires = { "mfussenegger/nvim-dap" },
        config = function()
            require("dapui").setup({
                icons = { expanded = "▾", collapsed = "▸" },
                mappings = {
                    -- Use a table to apply multiple mappings
                    expand = { "<CR>", "<2-LeftMouse>" },
                    open = "o",
                    remove = "d",
                    edit = "e",
                    repl = "r",
                },
                layouts = {
                    {
                        elements = {
                            "scopes",
                            "breakpoints",
                            "stacks",
                            "watches",
                        },
                        size = 40,
                        position = "left",
                    },
                    {
                        elements = {
                            "repl",
                            "console",
                        },
                        size = 10,
                        position = "bottom",
                    },
                },
                floating = {
                    max_height = nil, -- These can be integers or a float between 0 and 1.
                    max_width = nil, -- Floats will be treated as percentage of your screen.
                    border = "single", -- Border style. Can be "single", "double" or "rounded"
                    mappings = {
                        close = { "q", "<Esc>" },
                    },
                },
                windows = { indent = 1 },
            })
        end,
    })

    -- Terminal
    use({
        "akinsho/toggleterm.nvim",
        tag = "v2.*",
        config = function()
            require("toggleterm").setup()
        end,
    })

    -- Tooling
    use({
        "nvim-neo-tree/neo-tree.nvim",
        branch = "v2.x",
        requires = {
            "nvim-lua/plenary.nvim",
            "kyazdani42/nvim-web-devicons", -- not strictly required, but recommended
            "MunifTanjim/nui.nvim",
        },
        config = function()
            vim.cmd([[ let g:neo_tree_remove_legacy_commands = 1 ]])
            require("neo-tree").setup({})
        end,
    })
    use({
        "NTBBloodbath/rest.nvim",
        requires = { "nvim-lua/plenary.nvim" },
        config = function()
            require("rest-nvim").setup({
                -- Open request results in a horizontal split
                result_split_horizontal = false,
                -- Skip SSL verification, useful for unknown certificates
                skip_ssl_verification = true,
                -- Highlight request on run
                highlight = {
                    enabled = true,
                    timeout = 150,
                },
                -- Jump to request line on run
                jump_to_request = false,
            })
            vim.cmd("autocmd FileType http nmap <buffer> <Enter> <Plug>RestNvim")
        end,
    })
    use({ "eshion/vim-sync" })
    use({
        "folke/which-key.nvim",
        config = function()
            require("which-key").setup()
        end,
    })
    use({ "jghauser/mkdir.nvim" })
    use({
        "pwntester/octo.nvim",
        requires = {
            "nvim-lua/plenary.nvim",
            "nvim-telescope/telescope.nvim",
            "kyazdani42/nvim-web-devicons",
        },
        config = function()
            require("octo").setup()
        end,
    })

    -- Code navigation
    use({
        "ahmedkhalf/project.nvim",
        config = function()
            require("project_nvim").setup({})
        end,
    })

    -- Version Control
    use({
        "TimUntersberger/neogit",
        requires = { "nvim-lua/plenary.nvim", "sindrets/diffview.nvim" },
        config = function()
            require("neogit").setup({
                disable_commit_confirmation = true,
                use_magit_keybindings = true,
                disable_insert_on_commit = false,
                integrations = {
                    diffview = true,
                },
            })
        end,
    })
    use({
        "f-person/git-blame.nvim",
        config = function()
            vim.g.gitblame_enabled = 0
        end,
    })
    use({
        "lewis6991/gitsigns.nvim",
        requires = {
            "nvim-lua/plenary.nvim",
        },
        config = function()
            require("gitsigns").setup()
        end,
    })
end)
