local mason = require('mason')
mason.setup()
require("mason-lspconfig").setup({
  automatic_enable = false
})

local nvim_lsp = require("lspconfig")

local on_attach = function(client, bufnr)
  -- format on save
  if client.server_capabilities.documentFormattingProvider then
    vim.api.nvim_create_autocmd("BufWritePre", {
      group = vim.api.nvim_create_augroup("Format", { clear = true }),
      buffer = bufnr,
      callback = function()
        vim.lsp.buf.format()
      end,
    })
  end
end

require("lspconfig").lua_ls.setup({
  on_attach = on_attach,
  settings = {
    Lua = {
      runtime = {
        version = 'LuaJIT',
        path = vim.split(package.path, ';'),
      },
      diagnostics = {
        globals = { 'vim' },
      },
      workspace = {
        library = vim.api.nvim_get_runtime_file("", true),
        checkThirdParty = false,
      },
      telemetry = {
        enable = false,
      },
    },
  },
})
nvim_lsp["intelephense"].setup({
  settings = {
    intelephense = {
      environment = {
        phpVersion = "8.3.0"
      }
    }
  }
})
nvim_lsp["omnisharp"].setup({
  cmd = { "omnisharp" },
  on_attach = on_attach,
})
nvim_lsp["ts_ls"].setup({
  on_attach = on_attach,
})
nvim_lsp["cssls"].setup({
  on_attach = on_attach,
})
nvim_lsp["html"].setup({
  on_attach = on_attach,
})
nvim_lsp["jsonls"].setup({
  on_attach = on_attach,
})
nvim_lsp["pyright"].setup({
  on_attach = on_attach,
})
nvim_lsp["ocamllsp"].setup({
  cmd = { "opam", "exec", "--", "ocamllsp" },
  on_attach = on_attach,
})
nvim_lsp["templ"].setup({
  on_attach = on_attach,
})
nvim_lsp["gopls"].setup({
  on_attach = on_attach,
})
nvim_lsp["bashls"].setup({
  on_attach = on_attach,
})
nvim_lsp['hls'].setup({
  on_attach = on_attach
})
