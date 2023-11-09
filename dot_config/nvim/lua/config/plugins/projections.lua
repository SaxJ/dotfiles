return {
	'gnikdroy/projections.nvim',
	branch = 'pre_release',
	config = function ()
		require('projections').setup({
			workspaces = {
				{"~/Documents", {".git"}},
				{"~/.config", {".project"}}
			}
		})
	end
}
