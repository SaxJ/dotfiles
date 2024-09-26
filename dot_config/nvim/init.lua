local o = vim.opt

-- Editor options
o.number = true -- Print the line number in front of each line
o.relativenumber = true -- Show the line number relative to the line with the cursor in front of each line.
o.clipboard = "unnamedplus" -- uses the clipboard register for all operations except yank.
o.syntax = "on" -- When this option is set, the syntax with this name is loaded.
o.autoindent = true -- Copy indent from current line when starting a new line.
o.cursorline = true -- Highlight the screen line of the cursor with CursorLine.
o.expandtab = true -- In Insert mode: Use the appropriate number of spaces to insert a <Tab>.
o.shiftwidth = 2 -- Number of spaces to use for each step of (auto)indent.
o.tabstop = 2 -- Number of spaces that a <Tab> in the file counts for.
o.encoding = "UTF-8" -- Sets the character encoding used inside Vim.
o.ruler = true -- Show the line and column number of the cursor position, separated by a comma.
o.mouse = "a" -- Enable the use of the mouse. "a" you can use on all modes
o.title = true -- When on, the title of the window will be set to the value of 'titlestring'
o.hidden = true -- When on a buffer becomes hidden when it is |abandon|ed
o.ttimeoutlen = 0 -- The time in milliseconds that is waited for a key code or mapped key sequence to complete.
o.wildmenu = true -- When 'wildmenu' is on, command-line completion operates in an enhanced mode.
o.showcmd = true -- Show (partial) command in the last line of the screen. Set this option off if your terminal is slow.
o.showmatch = true -- When a bracket is inserted, briefly jump to the matching one.
o.inccommand = "split" -- When nonempty, shows the effects of :substitute, :smagic, :snomagic and user commands with the :command-preview flag as you type.
o.splitright = true
o.splitbelow = true -- When on, splitting a window will put the new window below the current one
o.termguicolors = true
o.wrap = false -- disable line wrapping

-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
	local lazyrepo = "https://github.com/folke/lazy.nvim.git"
	local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
	if vim.v.shell_error ~= 0 then
		vim.api.nvim_echo({
			{ "Failed to clone lazy.nvim:\n", "ErrorMsg" },
			{ out, "WarningMsg" },
			{ "\nPress any key to exit..." },
		}, true, {})
		vim.fn.getchar()
		os.exit(1)
	end
end
vim.opt.rtp:prepend(lazypath)

-- Make sure to setup `mapleader` and `maplocalleader` before
-- loading lazy.nvim so that mappings are correct.
-- This is also a good place to setup other settings (vim.opt)
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Setup lazy.nvim
require("lazy").setup({
	spec = {
		{ import = "plugins" },

		{ "voldikss/vim-floaterm" },
		-- optional for icons
		{ "nvim-tree/nvim-web-devicons" },
	},
	-- Configure any other settings here. See the documentation for more details.
	-- colorscheme that will be used when installing plugins.
	install = { colorscheme = { "habamax" } },
	-- automatically check for plugin updates
	checker = { enabled = true },
})

-- Attach LSP keybinds
vim.api.nvim_create_autocmd("LspAttach", {
	desc = "LSP Actions",
	callback = function(event)
		vim.keymap.set("n", "K", vim.lsp.buf.hover, { desc = "Hover", buffer = event.buf })
		vim.keymap.set("n", "gd", vim.lsp.buf.definition, { desc = "Goto definition", buffer = event.buf })
		vim.keymap.set("n", "gD", vim.lsp.buf.references, { desc = "Goto definition", buffer = event.buf })
		vim.keymap.set("n", "gi", vim.lsp.buf.implementation, { desc = "Goto implementation", buffer = event.buf })
		vim.keymap.set("n", "gr", vim.lsp.buf.references, { desc = "Goto references", buffer = event.buf })
		vim.keymap.set("n", "gs", vim.lsp.buf.signature_help, { desc = "Signature help", buffer = event.buf })
		vim.keymap.set("n", "<leader>cr", vim.lsp.buf.rename, { desc = "Code rename", buffer = event.buf })
		vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, { desc = "Code actions", buffer = event.buf })
	end,
})

vim.keymap.set("t", "<Esc>", "<C-\\><C-n>")
