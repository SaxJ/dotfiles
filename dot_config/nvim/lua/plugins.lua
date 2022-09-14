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
    use("Mofiqul/dracula.nvim")
    use({
        "nvim-lualine/lualine.nvim",
        requires = { "kyazdani42/nvim-web-devicons", opt = true },
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
    use("norcalli/nvim-colorizer.lua")
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
    use("nvim-web-devicons")
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
            require("configuration/completion")
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
    use({ "nvim-telescope/telescope.nvim", requires = { { "nvim-lua/plenary.nvim" } } })
    use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
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
    use("theHamsta/nvim-dap-virtual-text")
    use({ "rcarriga/nvim-dap-ui", requires = { "mfussenegger/nvim-dap" } })

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
