return {
	"nvim-treesitter/nvim-treesitter",
	build = ":TSUpdate",
	config = function()
		require("nvim-treesitter.configs").setup({
			ensure_installed = "all",
			highlight = {
				enable = true,
				additional_vim_regex_highlighting = { "org" },
			},
			indent = {
				enable = true,
			},
		})

		local parser_configs = require("nvim-treesitter.parsers").get_parser_configs()
		parser_configs.http = {
			install_info = {
				url = "https://github.com/NTBBloodbath/tree-sitter-http",
				files = { "src/parser.c" },
				branch = "main",
			},
		}
	end,
}
