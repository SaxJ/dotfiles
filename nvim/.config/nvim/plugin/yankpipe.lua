vim.api.nvim_create_autocmd("BufEnter", {
	callback = function()
		if vim.b.paste_pipe == nil then
			vim.b.paste_pipe = vim.g.paste_pipe
		end
	end,
})

vim.paste = (function(overridden)
	return function(lines, phase)
		if vim.b.paste_pipe ~= nil then
			for _, func in ipairs(vim.b.paste_pipe) do
				lines = func(lines)
			end
		end
		return overridden(lines, phase)
	end
end)(vim.paste)
