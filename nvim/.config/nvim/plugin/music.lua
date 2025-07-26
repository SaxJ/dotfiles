local play_headers = function()
  local header_query = vim.treesitter.query.parse("markdown", "((atx_heading) @header)")
  local root = vim.treesitter.get_parser():parse()[1]:root()
  local cmd = { "mpv" }
  for _, node, _, _ in header_query:iter_captures(root, 0, 0, -1) do
    local text = vim.treesitter.get_node_text(node, 0)
    local clean_text = string.gsub(text, "#+%s+", "")
    table.insert(cmd, "ytdl://ytsearch:" .. clean_text)
  end

  vim.system(cmd, { detach = true })
end
