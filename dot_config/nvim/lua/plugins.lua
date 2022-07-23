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
			require("mini.indentscope").setup({})
			require("mini.jump").setup({})
			require("mini.pairs").setup({})
		end,
	})
	use("folke/lua-dev.nvim")

	-- Appearance
	use("folke/tokyonight.nvim")
	use({
		"hoob3rt/lualine.nvim",
		requires = { "kyazdani42/nvim-web-devicons", opt = true },
	})

	-- Extra Languages
	use("amadeus/vim-mjml")
	use("rescript-lang/vim-rescript")
	use({
		"folke/todo-comments.nvim",
		requires = "nvim-lua/plenary.nvim",
		config = function()
			require("todo-comments").setup({})
		end,
	})
	use("adamclerk/vim-razor")
	use("jparise/vim-graphql")
	use("norcalli/nvim-colorizer.lua")

	-- LSP
	use({
		"glepnir/lspsaga.nvim",
		config = function()
			local saga = require("lspsaga")
			saga.init_lsp_saga()
		end,
	})
	use({
		"junnplus/nvim-lsp-setup",
		requires = {
			"neovim/nvim-lspconfig",
			"williamboman/nvim-lsp-installer",
			"hrsh7th/vim-vsnip",
		},
		config = function()
			require("nvim-lsp-setup").setup({
				default_mappings = true,
				gr = ":Lspsaga signature_help<CR>",
				gi = "lua require('goto-preview').goto_preview_implementation()",
				K = ":Lspsaga hover_doc<CR>",
				gs = ":Lspsaga signature_help<CR>",
				gd = ":Lspsaga preview_definition<CR>",
				gD = ":Lspsaga lsp_finder<CR>",
				on_attach = function(client, bufnr)
					--require('nvim-lsp-setup.utils').format_on_save(client)
				end,
				capabilities = vim.lsp.protocol.make_client_capabilities(),
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
	use("gpanders/editorconfig.nvim")
	use("mhartington/formatter.nvim")
	use({
		"hrsh7th/nvim-cmp",
		requires = {
			"hrsh7th/vim-vsnip",
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-nvim-lua",
			"hrsh7th/cmp-nvim-lsp",
		},
	})
	use({ "nvim-telescope/telescope.nvim", requires = { { "nvim-lua/plenary.nvim" } } })
	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
	use("airblade/vim-rooter")
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
	use("nvim-treesitter/nvim-treesitter-context")

	-- Notes
	use({
		"nvim-orgmode/orgmode",
		config = function()
			require("orgmode").setup_ts_grammar()
			require("orgmode").setup({
				org_agenda_files = { "~/Documents/wiki/**/*.org" },
				org_default_notes_file = "~/Documents/wiki/notes.org",
			})
		end,
	})

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
	use("eshion/vim-sync")
	use({
		"folke/which-key.nvim",
		config = function()
			require("which-key").setup()
		end,
	})
	use({
		"glacambre/firenvim",
		run = function()
			vim.fn["firenvim#install"](0)
		end,
	})
	use("jghauser/mkdir.nvim")

	-- Code navigation
	use({
		"ThePrimeagen/harpoon",
		requires = { "nvim-lua/plenary.nvim" },
	})
	use({
		"ahmedkhalf/project.nvim",
		config = function()
			require("project_nvim").setup({})
		end,
	})

	--Forge
	use({
		"pwntester/octo.nvim",
		requires = {
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope.nvim",
			"kyazdani42/nvim-web-devicons",
		},
		config = function()
			require("octo").setup({
				default_remote = { "upstream", "origin" }, -- order to try remotes
				ssh_aliases = {}, -- SSH aliases. e.g. `ssh_aliases = {["github.com-work"] = "github.com"}`
				reaction_viewer_hint_icon = "", -- marker for user reactions
				user_icon = " ", -- user icon
				timeline_marker = "", -- timeline marker
				timeline_indent = "2", -- timeline indentation
				right_bubble_delimiter = "", -- Bubble delimiter
				left_bubble_delimiter = "", -- Bubble delimiter
				github_hostname = "", -- GitHub Enterprise host
				snippet_context_lines = 4, -- number or lines around commented lines
				file_panel = {
					size = 10, -- changed files panel rows
					use_icons = true, -- use web-devicons in file panel (if false, nvim-web-devicons does not need to be installed)
				},
				mappings = {
					issue = {
						close_issue = { lhs = "<space>ic", desc = "close issue" },
						reopen_issue = { lhs = "<space>io", desc = "reopen issue" },
						list_issues = { lhs = "<space>il", desc = "list open issues on same repo" },
						reload = { lhs = "<C-r>", desc = "reload issue" },
						open_in_browser = { lhs = "<C-b>", desc = "open issue in browser" },
						copy_url = { lhs = "<C-y>", desc = "copy url to system clipboard" },
						add_assignee = { lhs = "<space>aa", desc = "add assignee" },
						remove_assignee = { lhs = "<space>ad", desc = "remove assignee" },
						create_label = { lhs = "<space>lc", desc = "create label" },
						add_label = { lhs = "<space>la", desc = "add label" },
						remove_label = { lhs = "<space>ld", desc = "remove label" },
						goto_issue = { lhs = "<space>gi", desc = "navigate to a local repo issue" },
						add_comment = { lhs = "<space>ca", desc = "add comment" },
						delete_comment = { lhs = "<space>cd", desc = "delete comment" },
						next_comment = { lhs = "]c", desc = "go to next comment" },
						prev_comment = { lhs = "[c", desc = "go to previous comment" },
						react_hooray = { lhs = "<space>rp", desc = "add/remove 🎉 reaction" },
						react_heart = { lhs = "<space>rh", desc = "add/remove ❤️ reaction" },
						react_eyes = { lhs = "<space>re", desc = "add/remove 👀 reaction" },
						react_thumbs_up = { lhs = "<space>r+", desc = "add/remove 👍 reaction" },
						react_thumbs_down = { lhs = "<space>r-", desc = "add/remove 👎 reaction" },
						react_rocket = { lhs = "<space>rr", desc = "add/remove 🚀 reaction" },
						react_laugh = { lhs = "<space>rl", desc = "add/remove 😄 reaction" },
						react_confused = { lhs = "<space>rc", desc = "add/remove 😕 reaction" },
					},
					pull_request = {
						checkout_pr = { lhs = "<space>po", desc = "checkout PR" },
						merge_pr = { lhs = "<space>pm", desc = "merge commit PR" },
						squash_and_merge_pr = { lhs = "<space>psm", desc = "squash and merge PR" },
						list_commits = { lhs = "<space>pc", desc = "list PR commits" },
						list_changed_files = { lhs = "<space>pf", desc = "list PR changed files" },
						show_pr_diff = { lhs = "<space>pd", desc = "show PR diff" },
						add_reviewer = { lhs = "<space>va", desc = "add reviewer" },
						remove_reviewer = { lhs = "<space>vd", desc = "remove reviewer request" },
						close_issue = { lhs = "<space>ic", desc = "close PR" },
						reopen_issue = { lhs = "<space>io", desc = "reopen PR" },
						list_issues = { lhs = "<space>il", desc = "list open issues on same repo" },
						reload = { lhs = "<C-r>", desc = "reload PR" },
						open_in_browser = { lhs = "<C-b>", desc = "open PR in browser" },
						copy_url = { lhs = "<C-y>", desc = "copy url to system clipboard" },
						goto_file = { lhs = "gf", desc = "go to file" },
						add_assignee = { lhs = "<space>aa", desc = "add assignee" },
						remove_assignee = { lhs = "<space>ad", desc = "remove assignee" },
						create_label = { lhs = "<space>lc", desc = "create label" },
						add_label = { lhs = "<space>la", desc = "add label" },
						remove_label = { lhs = "<space>ld", desc = "remove label" },
						goto_issue = { lhs = "<space>gi", desc = "navigate to a local repo issue" },
						add_comment = { lhs = "<space>ca", desc = "add comment" },
						delete_comment = { lhs = "<space>cd", desc = "delete comment" },
						next_comment = { lhs = "]c", desc = "go to next comment" },
						prev_comment = { lhs = "[c", desc = "go to previous comment" },
						react_hooray = { lhs = "<space>rp", desc = "add/remove 🎉 reaction" },
						react_heart = { lhs = "<space>rh", desc = "add/remove ❤️ reaction" },
						react_eyes = { lhs = "<space>re", desc = "add/remove 👀 reaction" },
						react_thumbs_up = { lhs = "<space>r+", desc = "add/remove 👍 reaction" },
						react_thumbs_down = { lhs = "<space>r-", desc = "add/remove 👎 reaction" },
						react_rocket = { lhs = "<space>rr", desc = "add/remove 🚀 reaction" },
						react_laugh = { lhs = "<space>rl", desc = "add/remove 😄 reaction" },
						react_confused = { lhs = "<space>rc", desc = "add/remove 😕 reaction" },
					},
					review_thread = {
						goto_issue = { lhs = "<space>gi", desc = "navigate to a local repo issue" },
						add_comment = { lhs = "<space>ca", desc = "add comment" },
						add_suggestion = { lhs = "<space>sa", desc = "add suggestion" },
						delete_comment = { lhs = "<space>cd", desc = "delete comment" },
						next_comment = { lhs = "]c", desc = "go to next comment" },
						prev_comment = { lhs = "[c", desc = "go to previous comment" },
						select_next_entry = { lhs = "]q", desc = "move to previous changed file" },
						select_prev_entry = { lhs = "[q", desc = "move to next changed file" },
						close_review_tab = { lhs = "<C-c>", desc = "close review tab" },
						react_hooray = { lhs = "<space>rp", desc = "add/remove 🎉 reaction" },
						react_heart = { lhs = "<space>rh", desc = "add/remove ❤️ reaction" },
						react_eyes = { lhs = "<space>re", desc = "add/remove 👀 reaction" },
						react_thumbs_up = { lhs = "<space>r+", desc = "add/remove 👍 reaction" },
						react_thumbs_down = { lhs = "<space>r-", desc = "add/remove 👎 reaction" },
						react_rocket = { lhs = "<space>rr", desc = "add/remove 🚀 reaction" },
						react_laugh = { lhs = "<space>rl", desc = "add/remove 😄 reaction" },
						react_confused = { lhs = "<space>rc", desc = "add/remove 😕 reaction" },
					},
					submit_win = {
						approve_review = { lhs = "<C-a>", desc = "approve review" },
						comment_review = { lhs = "<C-m>", desc = "comment review" },
						request_changes = { lhs = "<C-r>", desc = "request changes review" },
						close_review_tab = { lhs = "<C-c>", desc = "close review tab" },
					},
					review_diff = {
						add_review_comment = { lhs = "<space>ca", desc = "add a new review comment" },
						add_review_suggestion = { lhs = "<space>sa", desc = "add a new review suggestion" },
						focus_files = { lhs = "<leader>e", desc = "move focus to changed file panel" },
						toggle_files = { lhs = "<leader>b", desc = "hide/show changed files panel" },
						next_thread = { lhs = "]t", desc = "move to next thread" },
						prev_thread = { lhs = "[t", desc = "move to previous thread" },
						select_next_entry = { lhs = "]q", desc = "move to previous changed file" },
						select_prev_entry = { lhs = "[q", desc = "move to next changed file" },
						close_review_tab = { lhs = "<C-c>", desc = "close review tab" },
						toggle_viewed = { lhs = "<leader><space>", desc = "toggle viewer viewed state" },
					},
					file_panel = {
						next_entry = { lhs = "j", desc = "move to next changed file" },
						prev_entry = { lhs = "k", desc = "move to previous changed file" },
						select_entry = { lhs = "<cr>", desc = "show selected changed file diffs" },
						refresh_files = { lhs = "R", desc = "refresh changed files panel" },
						focus_files = { lhs = "<leader>e", desc = "move focus to changed file panel" },
						toggle_files = { lhs = "<leader>b", desc = "hide/show changed files panel" },
						select_next_entry = { lhs = "]q", desc = "move to previous changed file" },
						select_prev_entry = { lhs = "[q", desc = "move to next changed file" },
						close_review_tab = { lhs = "<C-c>", desc = "close review tab" },
						toggle_viewed = { lhs = "<leader><space>", desc = "toggle viewer viewed state" },
					},
				},
			})
		end,
	})

	-- Version Control
	use({
		"TimUntersberger/neogit",
		requires = { "nvim-lua/plenary.nvim", "sindrets/diffview.nvim" },
		config = function()
			local neogit = require("neogit")

			neogit.setup({
				disable_signs = false,
				disable_hint = false,
				disable_context_highlighting = false,
				disable_commit_confirmation = true,
				-- Neogit refreshes its internal state after specific events, which can be expensive depending on the repository size.
				-- Disabling `auto_refresh` will make it so you have to manually refresh the status after you open it.
				auto_refresh = true,
				disable_builtin_notifications = false,
				use_magit_keybindings = true,
				commit_popup = {
					kind = "split",
				},
				-- Change the default way of opening neogit
				kind = "tab",
				-- customize displayed signs
				signs = {
					-- { CLOSED, OPENED }
					section = { ">", "v" },
					item = { ">", "v" },
					hunk = { "", "" },
				},
				integrations = {
					diffview = true,
				},
				-- Setting any section to `false` will make the section not render at all
				sections = {
					untracked = {
						folded = false,
					},
					unstaged = {
						folded = true,
					},
					staged = {
						folded = false,
					},
					stashes = {
						folded = true,
					},
					unpulled = {
						folded = true,
					},
					unmerged = {
						folded = false,
					},
					recent = {
						folded = true,
					},
				},
				-- override/add mappings
				mappings = {
					-- modify status buffer mappings
					status = {
						-- Adds a mapping with "B" as key that does the "BranchPopup" command
						["B"] = "BranchPopup",
						-- Removes the default mapping of "s"
						["s"] = "",
					},
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
	use({
		"ruifm/gitlinker.nvim",
		requires = "nvim-lua/plenary.nvim",
		config = function()
			require("gitlinker").setup()
		end,
	})
end)
