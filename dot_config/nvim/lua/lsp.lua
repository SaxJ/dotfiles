require("neodev").setup()

local cmp = require("cmp")
local lspkind = require("lspkind")
local lspconfig = require("lspconfig")
local ht = require("haskell-tools")

local capabilities = require("cmp_nvim_lsp").default_capabilities()

local attach_keybinds = function(_, bufnr)
	-- Mappings.
	local bufopts = { noremap = true, silent = true, buffer = bufnr }

	vim.keymap.set("n", "gD", vim.lsp.buf.declaration, bufopts)
	vim.keymap.set("n", "gd", vim.lsp.buf.definition, bufopts)
	vim.keymap.set("n", "K", vim.lsp.buf.hover, bufopts)
	vim.keymap.set("n", "gi", vim.lsp.buf.implementation, bufopts)
	vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, bufopts)
	vim.keymap.set("n", "gr", ":Trouble lsp_references<CR>", bufopts)
end

-- Autocomplete mapping
cmp.setup({
	snippet = {
		expand = function(args)
			require("luasnip").lsp_expand(args.body)
		end,
	},
	mapping = {
		["<Tab>"] = cmp.mapping(cmp.mapping.select_next_item()),
		["<S-Tab>"] = cmp.mapping(cmp.mapping.select_prev_item()),
		["<C-Space>"] = cmp.mapping.complete({}),
		["<CR>"] = cmp.mapping.confirm({
			behavior = cmp.ConfirmBehavior.Replace,
			select = true,
		}),
	},
	sources = {
		{ name = "nvim_lsp" },
		{ name = "orgmode" },
		{ name = "luasnip" },
		-- { name = "buffer" },
	},
	formatting = {
		format = lspkind.cmp_format({
			mode = "symbol",
			maxwidth = 50,
		}),
	},
})

local servers = {
	"bashls",
	"dockerls",
	"intelephense",
	"jsonls",
	"csharp_ls",
	"sumneko_lua",
	"tsserver",
	"yamlls",
}

for _, lsp in ipairs(servers) do
	lspconfig[lsp].setup({
		on_attach = attach_keybinds,
		capabilities = capabilities,
	})
end

ht.setup({
	hls = {
		on_attach = function(client, bufnr)
			local bufopts = { noremap = true, silent = true, buffer = bufnr }

			vim.keymap.set("n", "<space>ca", vim.lsp.buf.code_action, bufopts)
			vim.keymap.set("n", "<space>hs", ht.hoogle.hoogle_signature, bufopts)
			vim.keymap.set("n", "<leader>rf", function()
				ht.repl.toggle(vim.api.nvim_buf_get_name(0))
			end, bufopts)

			attach_keybinds(client, bufnr)
		end,
	},
})
