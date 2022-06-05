require("configuration/completion")
--require("configuration/lsp")
require("configuration/formatting")
require("configuration/debugger")
require('nvim-tree').setup({
    disable_netrw = true,
    actions = {
        open_file = {
            quit_on_open = true
        }
    }
})
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
require("telescope").load_extension("frecency")
require('telescope').load_extension('repo')
vim.g['rooter_cd_cmd'] = 'lcd'

local saga = require("lspsaga")
saga.init_lsp_saga()

require("octo").setup({
	default_remote = { "upstream", "origin" }, -- order to try remotes
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
		use_icons = true, -- use web-devicons in file panel
	},
	mappings = {
		issue = {
			close_issue = "<space>ic", -- close issue
			reopen_issue = "<space>io", -- reopen issue
			list_issues = "<space>il", -- list open issues on same repo
			reload = "<C-r>", -- reload issue
			open_in_browser = "<C-b>", -- open issue in browser
			copy_url = "<C-y>", -- copy url to system clipboard
			add_assignee = "<space>aa", -- add assignee
			remove_assignee = "<space>ad", -- remove assignee
			create_label = "<space>lc", -- create label
			add_label = "<space>la", -- add label
			remove_label = "<space>ld", -- remove label
			goto_issue = "<space>gi", -- navigate to a local repo issue
			add_comment = "<space>ca", -- add comment
			delete_comment = "<space>cd", -- delete comment
			next_comment = "]c", -- go to next comment
			prev_comment = "[c", -- go to previous comment
			react_hooray = "<space>rp", -- add/remove 🎉 reaction
			react_heart = "<space>rh", -- add/remove ❤️ reaction
			react_eyes = "<space>re", -- add/remove 👀 reaction
			react_thumbs_up = "<space>r+", -- add/remove 👍 reaction
			react_thumbs_down = "<space>r-", -- add/remove 👎 reaction
			react_rocket = "<space>rr", -- add/remove 🚀 reaction
			react_laugh = "<space>rl", -- add/remove 😄 reaction
			react_confused = "<space>rc", -- add/remove 😕 reaction
		},
		pull_request = {
			checkout_pr = "<space>po", -- checkout PR
			merge_pr = "<space>pm", -- merge PR
			list_commits = "<space>pc", -- list PR commits
			list_changed_files = "<space>pf", -- list PR changed files
			show_pr_diff = "<space>pd", -- show PR diff
			add_reviewer = "<space>va", -- add reviewer
			remove_reviewer = "<space>vd", -- remove reviewer request
			close_issue = "<space>ic", -- close PR
			reopen_issue = "<space>io", -- reopen PR
			list_issues = "<space>il", -- list open issues on same repo
			reload = "<C-r>", -- reload PR
			open_in_browser = "<C-b>", -- open PR in browser
			copy_url = "<C-y>", -- copy url to system clipboard
			add_assignee = "<space>aa", -- add assignee
			remove_assignee = "<space>ad", -- remove assignee
			create_label = "<space>lc", -- create label
			add_label = "<space>la", -- add label
			remove_label = "<space>ld", -- remove label
			goto_issue = "<space>gi", -- navigate to a local repo issue
			add_comment = "<space>ca", -- add comment
			delete_comment = "<space>cd", -- delete comment
			next_comment = "]c", -- go to next comment
			prev_comment = "[c", -- go to previous comment
			react_hooray = "<space>rp", -- add/remove 🎉 reaction
			react_heart = "<space>rh", -- add/remove ❤️ reaction
			react_eyes = "<space>re", -- add/remove 👀 reaction
			react_thumbs_up = "<space>r+", -- add/remove 👍 reaction
			react_thumbs_down = "<space>r-", -- add/remove 👎 reaction
			react_rocket = "<space>rr", -- add/remove 🚀 reaction
			react_laugh = "<space>rl", -- add/remove 😄 reaction
			react_confused = "<space>rc", -- add/remove 😕 reaction
		},
		review_thread = {
			goto_issue = "<space>gi", -- navigate to a local repo issue
			add_comment = "<space>ca", -- add comment
			add_suggestion = "<space>sa", -- add suggestion
			delete_comment = "<space>cd", -- delete comment
			next_comment = "]c", -- go to next comment
			prev_comment = "[c", -- go to previous comment
			select_next_entry = "]q", -- move to previous changed file
			select_prev_entry = "[q", -- move to next changed file
			close_review_tab = "<C-c>", -- close review tab
			react_hooray = "<space>rp", -- add/remove 🎉 reaction
			react_heart = "<space>rh", -- add/remove ❤️ reaction
			react_eyes = "<space>re", -- add/remove 👀 reaction
			react_thumbs_up = "<space>r+", -- add/remove 👍 reaction
			react_thumbs_down = "<space>r-", -- add/remove 👎 reaction
			react_rocket = "<space>rr", -- add/remove 🚀 reaction
			react_laugh = "<space>rl", -- add/remove 😄 reaction
			react_confused = "<space>rc", -- add/remove 😕 reaction
		},
		submit_win = {
			approve_review = "<C-a>", -- approve review
			comment_review = "<C-m>", -- comment review
			request_changes = "<C-r>", -- request changes review
			close_review_tab = "<C-c>", -- close review tab
		},
		review_diff = {
			add_review_comment = "<space>ca", -- add a new review comment
			add_review_suggestion = "<space>sa", -- add a new review suggestion
			focus_files = "<leader>e", -- move focus to changed file panel
			toggle_files = "<leader>b", -- hide/show changed files panel
			next_thread = "]t", -- move to next thread
			prev_thread = "[t", -- move to previous thread
			select_next_entry = "]q", -- move to previous changed file
			select_prev_entry = "[q", -- move to next changed file
			close_review_tab = "<C-c>", -- close review tab
			toggle_viewed = "<leader><space>", -- toggle viewer viewed state
		},
		file_panel = {
			next_entry = "j", -- move to next changed file
			prev_entry = "k", -- move to previous changed file
			select_entry = "<cr>", -- show selected changed file diffs
			refresh_files = "R", -- refresh changed files panel
			focus_files = "<leader>e", -- move focus to changed file panel
			toggle_files = "<leader>b", -- hide/show changed files panel
			select_next_entry = "]q", -- move to previous changed file
			select_prev_entry = "[q", -- move to next changed file
			close_review_tab = "<C-c>", -- close review tab
			toggle_viewed = "<leader><space>", -- toggle viewer viewed state
		},
	},
})

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
                stategy = 'flat',
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
