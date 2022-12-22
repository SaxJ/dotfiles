return {
    "echasnovski/mini.nvim",
    config = function ()
        require("mini.comment").setup({})
        require("mini.cursorword").setup({})
        require("mini.jump").setup({})
        require("mini.pairs").setup({})
    end
}
