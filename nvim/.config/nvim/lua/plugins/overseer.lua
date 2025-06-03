return {
  'stevearc/overseer.nvim',
  config = function()
    require('overseer').setup({
      strategy = { 'toggleterm', use_shell = true },
    })
  end
}
