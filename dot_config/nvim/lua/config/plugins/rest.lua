return {
	"NTBBloodbath/rest.nvim",
	dependencies = { "nvim-lua/plenary.nvim" },
	config = function()
		require("rest-nvim").setup({
			-- Open request results in a horizontal split
			result_split_horizontal = false,
			-- Skip SSL verification, useful for unknown certificates
			skip_ssl_verification = true,
			-- Highlight request on run
			highlight = {
				enabled = true,
				timeout = 150,
			},
			-- Jump to request line on run
			jump_to_request = false,
		})
		vim.cmd("autocmd FileType http nmap <buffer> <Enter> <Plug>RestNvim")
	end,
}
