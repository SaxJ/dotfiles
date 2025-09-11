require('blink.cmp').setup({
  keymap = {
    preset = 'none',

    -- If completion hasn't been triggered yet, insert the first suggestion; if it has, cycle to the next suggestion.
    ['<Tab>'] = {
      function(cmp)
        if cmp.snippet_active() then
          return cmp.snippet_forward()
        else
          return cmp.select_next()
        end
      end,
      'fallback',
    },
    -- Navigate to the previous suggestion or cancel completion if currently on the first one.
    ['<S-Tab>'] = { function(cmp)
      if cmp.snippet_active() then
        return cmp.snippet_backward()
      else
        return cmp.select_prev()
      end
    end },
    ['<C-Space>'] = { 'show' },
    ['<CR>'] = { 'accept', 'fallback' }
  },

  appearance = {
    nerd_font_variant = 'mono'
  },

  completion = {
    menu = { enabled = true, border = 'single' },
    list = {
      selection = { preselect = false }, cycle = { from_top = false },
    },
    documentation = {
      auto_show = true,
      window = {
        border = 'single'
      }
    },
  },
  signature = {
    enabled = true,
  },

  -- Default list of enabled providers defined so that you can extend it
  -- elsewhere in your config, without redefining it, due to `opts_extend`
  sources = {
    default = { 'lsp', 'path', 'snippets', 'buffer' },
  },

  -- (Default) Rust fuzzy matcher for typo resistance and significantly better performance
  -- You may use a lua implementation instead by using `implementation = "lua"` or fallback to the lua implementation,
  -- when the Rust fuzzy matcher is not available, by using `implementation = "prefer_rust"`
  --
  -- See the fuzzy documentation for more information
  fuzzy = { implementation = "prefer_rust" },
  cmdline = {
    enabled = false,
    keymap = { preset = 'inherit' },
    completion = { menu = { auto_show = true } },
  }
})
