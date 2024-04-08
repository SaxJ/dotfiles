return {
    'akinsho/toggleterm.nvim',
    config = function ()
        require('toggleterm').setup({
            winbar = {
                enabled = true,
            },
            open_mapping = "<leader>ot",
        })
    end
}
