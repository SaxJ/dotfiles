local module = {}

--- @param project string
local function open_tab_if_not_existing(project)
	local tabids = vim.api.nvim_list_tabpages()
	local tabs = {}
	for tabnr, tabid in ipairs(tabids) do
		local twd = vim.fn.getcwd(-1, tabnr)
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

function MyTabLine()
	local s = ""
	local current_tabpage = vim.api.nvim_get_current_tabpage()

	for tabnr, tabid in ipairs(vim.api.nvim_list_tabpages()) do
		local is_current = tabid == current_tabpage
		local twd = vim.fn.getcwd(-1, tabnr)
		local name = vim.fn.fnamemodify(twd, ":t")

		if is_current then
			s = s .. "%#TabLineSel#"
		else
			s = s .. "%#TabLine#"
		end

		s = s .. " " .. name .. " "
	end

	s = s .. "%#TabLineFill#"
	return s
end

_G.Project = module
