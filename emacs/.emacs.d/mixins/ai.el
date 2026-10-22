(use-package gptel
  :ensure t
  :config
  (setq gptel-model 'prism-ml/bonsai-27b
        gptel-backend (gptel-make-openai "LM Studio"
                        :stream t
                        :protocol "http"
                        :host "localhost:1234"
                        :models '(mistralai/devstral-small-2-2512 prism-ml/bonsai-27b))))

(use-package gptel-agent
  :ensure t
  :after gptel
  :config
  (gptel-agent-update))

(use-package agent-shell
  :ensure t)

(use-package agent-shell-manager
  :ensure t
  :vc (agent-shell-manager :url "https://github.com/jethrokuan/agent-shell-manager" :branch "main" :rev :newest))

(use-package agent-shell-notifications
  :ensure t
  :vc (agent-shell-notifications :url "https://github.com/zackattackz/agent-shell-notifications" :branch "main" :rev :newest
                                  :ignored-files ("agent-shell-notifications-knockknock.el" "agent-shell-notifications-tests.el"))
  :after agent-shell
  :config
  (add-hook 'agent-shell-mode-hook 'agent-shell-notifications-mode))
