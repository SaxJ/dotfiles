return {
	'arnarg/todotxt.nvim',
	dependencies = {
		'MunifTanjim/nui.nvim',
	},
	config = function ()
		require('todotxt-nvim').setup({
			todo_file = '~/Dropbox/todo/todo.txt',
		})
	end
}
