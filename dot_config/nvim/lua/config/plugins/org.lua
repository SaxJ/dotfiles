return {
	'nvim-orgmode/orgmode',
	event = 'VeryLazy',
	dependencies = {
		'akinsho/org-bullets.nvim',
		'lukas-reineke/headlines.nvim',
		'dhruvasagar/vim-table-mode',
		'andreadev-it/orgmode-multi-key',
	},
	config = function ()
		require('orgmode').setup({
			org_agenda_files = {'~/Documents/wiki/**/*'},
			org_default_notes_file = '~/Documents/wiki/notes.org',
			org_capture_templates = {
				t = {
					description = "New Todo",
					template = '* TODO [#%^{A|B|C}] %? %t',
					target = '~/Documents/wiki/todo.org',
				},
				j = {
					description = "New Journal",
					datetree = {
						tree_type = 'day',
					},
					template = '** %<%I:%M %p>\n- %?',
					target = '~/Documents/wiki/journal.org',
				}
			}
		})
		require('org-bullets').setup({})
		-- require('headlines').setup({})
		require('orgmode-multi-key').setup({
			key = '<Tab>',
		})
	end
}
