local module = {}

--- @param project string
function open_tab_if_not_existing(project)
	local tabids = vim.api.nvim_list_tabpages()
	local tabs = {}
	for _, tabid in ipairs(tabids) do
		local twd = vim.fn.getcwd(-1, tabid)
		local tabname = vim.fn.fnamemodify(twd, ":t")
		tabs[tabname] = tabid
	end
	local short_name = vim.fn.fnamemodify(project, ":t")

	if tabs[short_name] ~= nil then
		vim.api.nvim_set_current_tabpage(tabs[short_name])
	else
		vim.cmd.tabnew()
		vim.cmd.tcd(project)
	end
end

module.open_tab_if_not_existing = open_tab_if_not_existing

_G.Project = module
