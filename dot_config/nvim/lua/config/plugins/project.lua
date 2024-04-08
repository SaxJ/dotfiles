return {
    'charludo/projectmgr.nvim',
    lazy = false,
    config = function()
        require('projectmgr').setup({
            session = { enabled = true, file = ".idea" }
        })
    end
}
