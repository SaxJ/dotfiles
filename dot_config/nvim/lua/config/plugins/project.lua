return {
	'ahmedkhalf/project.nvim',
	config = function ()
		require("project_nvim").setup({
			patterns = { ".git", "_darcs", ".hg", ".bzr", ".svn", "Makefile", "package.json", ".project" },
			show_hidden = true,
			manual_mode = false,
			silent_chdir = false,
			scope_chdir = 'global'
		})
	end
}
