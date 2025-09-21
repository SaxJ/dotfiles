local mason = require('mason')
mason.setup()
require("mason-lspconfig").setup({
  automatic_enable = false
})


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

vim.lsp.config('lua_ls', {
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
vim.lsp.enable('lua_ls')

vim.lsp.config("intelephense", {
  settings = {
    intelephense = {
      telemetry = {
        enabled = false,
      },
      environment = {
        phpVersion = "8.3.0"
      },
    }
  }
})
vim.lsp.enable('intelephense')

vim.lsp.config("omnisharp", {
  cmd = { "omnisharp" },
  on_attach = on_attach,
})
vim.lsp.enable('omnisharp')

vim.lsp.config("ts_ls", {
  on_attach = on_attach,
})
vim.lsp.enable('ts_ls')

vim.lsp.config("cssls", {
  on_attach = on_attach,
})
vim.lsp.enable('cssls')

vim.lsp.config("html", {
  on_attach = on_attach,
})
vim.lsp.enable('html')

vim.lsp.config("jsonls", {
  on_attach = on_attach,
})
vim.lsp.enable('jsonls')

vim.lsp.config("pyright", {
  on_attach = on_attach,
})
vim.lsp.enable('pyright')

vim.lsp.config("ocamllsp", {
  cmd = { "opam", "exec", "--", "ocamllsp" },
  on_attach = on_attach,
})
vim.lsp.enable('ocamllsp')

vim.lsp.config("templ", {
  on_attach = on_attach,
})
vim.lsp.enable('templ')

vim.lsp.config("gopls", {
  on_attach = on_attach,
})
vim.lsp.enable('gopls')

vim.lsp.config("bashls", {
  on_attach = on_attach,
})
vim.lsp.enable('bashls')

vim.lsp.config("hls", {
  on_attach = on_attach
})
vim.lsp.enable('hls')
