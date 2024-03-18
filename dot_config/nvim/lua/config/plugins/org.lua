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
		require('orgmode').setup_ts_grammar()
		require('orgmode').setup({
			org_agenda_files = {'~/Documents/wiki/**/*'},
			org_default_notes_file = '~/Documents/wiki/notes.org',
			org_capture_templates = {
				j = {
					description = "New Journal",
					datetree = {
						tree_type = 'day',
					},
					template = '** %<%I:%M %p>\n- %?',
					target = os.date('~/Documents/wiki/journals/%Y_01_01.org'),
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
