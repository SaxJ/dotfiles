local prettier = function()
  return {
    exe = "prettier",
    args = {"--stdin-filepath", vim.api.nvim_buf_get_name(0), "--single-quote"},
    stdin = true
  }
end
require("formatter").setup(
  {
    logging = false,
    filetype = {
      php = {
        function()
          return {
            exe = "php-cs-fixer",
            args = {"fix", vim.api.nvim_buf_get_name(0)},
            stdin = false
          }
        end
      },
      javascript = {prettier},
      typescript = {prettier},
      typescriptreact = {prettier},
      css = {prettier},
      scss = {prettier},
      rust = {
        -- Rustfmt
        function()
          return {
            exe = "rustfmt",
            args = {"--emit=stdout"},
            stdin = true
          }
        end
      },
      lua = {
        -- luafmt
        function()
          return {
            exe = "luafmt",
            args = {"--indent-count", 2, "--stdin"},
            stdin = true
          }
        end
      },
      cpp = {
        -- clang-format
        function()
          return {
            exe = "clang-format",
            args = {"--assume-filename", vim.api.nvim_buf_get_name(0)},
            stdin = true,
            cwd = vim.fn.expand("%:p:h") -- Run clang-format in cwd of the file.
          }
        end
      }
    }
  }
)

-- Format on save
vim.api.nvim_exec(
  [[
augroup FormatAutogroup
  autocmd!
  autocmd BufWritePost *.js,*.tsx,*.ts,*.jsx,*.rs,*.lua FormatWrite
augroup END
]],
  true
)