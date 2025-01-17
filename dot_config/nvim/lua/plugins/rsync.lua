return {
	"OscarCreator/rsync.nvim",
	build = "make",
	dependencies = "nvim-lua/plenary.nvim",
	config = function()
		require("rsync").setup({
			fugitive_sync = false,
			sync_on_save = false,
			reload_file_after_sync = true,
			-- on_exit = function(code, _command) end,
			-- on_stderr = function(data, command) end,
			project_config_path = ".nvim/rsync.toml",
		})
	end,
}
