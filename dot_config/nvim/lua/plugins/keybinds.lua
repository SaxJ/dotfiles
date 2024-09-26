return {
	"folke/which-key.nvim",
	event = "VeryLazy",
	opts = {
		preset = "modern",
	},
	keys = {
		{
			"<leader>?",
			function()
				require("which-key").show({ global = false })
			end,
			desc = "Buffer Local Keymaps (which-key)",
		},
		{
			"<leader>.",
			function()
				require("telescope.builtin").find_files({ cwd = vim.fn.expand("%:p:h") })
			end,
			desc = "Siblings",
		},
		{ "<leader><leader>", "<cmd>Telescope find_files<cr>", desc = "Find files" },
		{ "<leader>-", ":lua MiniFiles.open()<CR>", desc = "File Browser" },
		{ "<leader>/", "<cmd>Telescope live_grep<cr>", desc = "Project Search" },
		{ "<leader>p", group = "Project" },
		{ "<leader>pp", "<cmd>Telescope projects<cr>", desc = "Switch Project" },
		{ "<leader>ps", "<cmd>Telescope grep_string<cr>", desc = "Project Search" },
		{ "<leader>g", group = "Git" },
		{
			"<leader>gg",
			function()
				local cwd = vim.fn.getcwd()
				vim.cmd(string.format("FloatermNew (cd %s && lazygit)", cwd))
			end,
			desc = "Git status",
		},
		{ "<leader>r", group = "Remote" },
		{ "<leader>ru", ":RsyncUpFile<cr>", desc = "Upload File" },
		{ "<leader>rd", ":RsyncDownFile<cr>", desc = "Download File" },
		{ "<leader>o", group = "Opener" },
		{ "<leader>ot", ":terminal<cr>", desc = "Terminal" },
	},
}
