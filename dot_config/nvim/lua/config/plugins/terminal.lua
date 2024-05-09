return {
    'akinsho/toggleterm.nvim',
    version = "*",
    config = function()
        require('toggleterm').setup({
            open_mapping = '<leader>ot',
            insert_mappings = false,
        })
    end
}
