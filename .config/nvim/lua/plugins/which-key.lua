return {
	"folke/which-key.nvim",
	event = "VeryLazy",
	opts = {},
	config = function()
		local wk = require("which-key")
		wk.add({
			{
				mode = "n",
				-- General
				{ "<leader><leader>", ":Telescope find_files<cr>", desc = "Find File" },
				{ "<leader>/", ":Telescope live_grep<CR>", desc = "Search String" },
				{ "<leader>-", require("mini.files").open, desc = "File browser" },
				-- Files
				{ "<leader>f", group = "file" },
				{ "<leader>ff", ":Telescope find_files<cr>", desc = "Find File" },
				-- Buffers
				{ "<leader>b", group = "buffers" },
				{ "<leader>bb", ":Telescope buffers<cr>", desc = "Buffers" },
				-- Git
				{ "<leader>g", group = "git" },
				{ "<leader>gg", ":Git<CR>", desc = "Git" },
				{ "<leader>gd", ":Telescope git_files<CR>", desc = "Changed Files" },
				-- Project
				{ "<leader>p", group = "project" },
				{ "<leader>pp", ":Telescope neovim-project<CR>", desc = "Projects" },
				-- Remote
				{ "<leader>r", group = "remote" },
				{
					"<leader>ru",
					"!scp %:p ubuntu@minikube:/home/ubuntu/$(basename $PWD)/%:p:~:.<CR>",
					desc = "Push to remote",
				},
				{
					"<leader>rp",
					"!scp ubuntu@minikube:/home/ubuntu/$(basename $PWD)/%:p:~:. %:p<CR>",
					desc = "Push to remote",
				},
			},
			{
				mode = "t",
				{ "<Esc>", "<C-\\><C-n>" },
			},
		})
	end,
	keys = {
		{
			"<leader>?",
			function()
				require("which-key").show({ global = false })
			end,
			desc = "Buffer Local Keymaps (which-key)",
		},
	},
}
