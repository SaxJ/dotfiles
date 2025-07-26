require('fzf-lua').setup({

    grep = {
      rg_opts = [[--hidden --column -g "!.git" --line-number --no-heading --color=always --smart-case]]
    }
  })
