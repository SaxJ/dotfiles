vim.api.nvim_create_user_command("PlayerctlToggle", function()
	vim.system({ "playerctl", "play-pause" })
end, { desc = "Toggle pause/play" })

vim.api.nvim_create_user_command("PlayerctlNext", function()
	vim.system({ "playerctl", "next" })
end, { desc = "Next Song" })

vim.api.nvim_create_user_command("PlayerctlPrevious", function()
	vim.system({ "playerctl", "previous" })
end, { desc = "Previous Song" })
-- vim: ts=2 sts=2 sw=2 et
