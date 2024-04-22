return {
    'akinsho/toggleterm.nvim',
    config = function ()
        require('toggleterm').setup({
            winbar = {
                enabled = true,
            },
        })
    end
}
