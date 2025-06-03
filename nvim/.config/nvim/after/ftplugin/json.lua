vim.keymap.set("n", "<localleader>q", function()
	require("jq").run()
end, { noremap = true, silent = true, buffer = true, desc = "JQ" })
