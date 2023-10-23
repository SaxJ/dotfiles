return {
    "echasnovski/mini.nvim",
    config = function ()
        require('mini.ai').setup({})
        require('mini.align').setup({})
        require('mini.bracketed').setup({})
        require("mini.comment").setup({})
        require("mini.cursorword").setup({})
        require("mini.jump").setup({})
        require("mini.pairs").setup({})
    end
}
