return {
	enabled = false,
	'natecraddock/workspaces.nvim',
	config = function ()
		require('workspaces').setup({
			auto_open = true,
			cd_type = 'global',
			hooks = {
				add = {},
				remove = {},
				rename = {},
				open_pre = {},
				open = { "Telescope find_files" }
			}
		})
	end
}
