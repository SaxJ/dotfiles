vim.keymap.set("n", "<C-c><C-c>", function()
	local file_name = vim.fn.expand("%:p")
	vim.cmd([[vsplit | terminal hurl ]] .. file_name)
end, { noremap = true, silent = true, buffer = true, desc = "Execute" })
