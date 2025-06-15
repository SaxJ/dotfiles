return {
  "ibhagwan/fzf-lua",
  -- optional for icon support
  -- dependencies = { "nvim-tree/nvim-web-devicons" },
  -- or if using mini.icons/mini.nvim
  dependencies = { "echasnovski/mini.icons" },
  opts = {
    grep = {
      rg_opts = [[--hidden --column -g "!.git" --line-number --no-heading --color=always --smart-case]]
    }
  }
}
