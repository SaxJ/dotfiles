vim.g.scp_projects = {
  ["megatron"] = "/home/ubuntu/megatron",
  ["hannibal"] = "/home/ubuntu/hannibal",
  ["unicron"] = "/home/ubuntu/unicron",
}

local get_project = function()
  local pwd = vim.uv.cwd() or ""
  return vim.fn.fnamemodify(pwd, ":t")
end

local scp_on_exit = function()
  vim.notify("SCP transfer complete", vim.log.levels.INFO)
end

local scp_upload = function()
  local project = get_project()
  local remote_path = vim.g.scp_projects[project]
  if remote_path == nil then
    vim.notify("Project not configured for SCP", vim.log.levels.ERROR)
    return
  end

  local local_relative_filename = vim.fn.expand("%:p:.")
  local scp_cmd = {
    "scp",
    local_relative_filename,
    string.format("ubuntu@minikube:%s/%s", remote_path, local_relative_filename),
  }

  vim.system(scp_cmd, nil, vim.schedule_wrap(scp_on_exit))
end

local scp_download = function()
  local project = get_project()
  local remote_path = vim.g.scp_projects[project]
  if remote_path == nil then
    vim.notify("Project not configured for SCP", vim.log.levels.ERROR)
    return
  end

  local local_relative_filename = vim.fn.expand("%:p:.")
  local scp_cmd = {
    "scp",
    string.format("ubuntu@minikube:%s/%s", remote_path, local_relative_filename),
    local_relative_filename,
  }

  vim.system(scp_cmd, nil, vim.schedule_wrap(scp_on_exit))
end

vim.api.nvim_create_user_command('ScpUpload', scp_upload, { desc = 'Upload file to corresponding project' })
vim.api.nvim_create_user_command('ScpDownload', scp_download, { desc = 'Upload file to corresponding project' })
