-- Kubernetes
local function kubectl_pick_pod(cb)
	require("fzf-lua").fzf_exec("kubectl get pods --no-headers -o custom-columns=NAME:.metadata.name", {
		prompt = "Pod> ",
		actions = {
			["default"] = function(selected)
				cb(selected[1])
			end,
		},
	})
end

vim.keymap.set("n", "<leader>kp", function()
	kubectl_pick_pod(function(pod)
		vim.cmd(string.format("VTerm kubectl exec -it %s -- bash", pod))
	end)
end, { desc = "Pick pod (copy name)" })
-- vim: ts=2 sts=2 sw=2 et
