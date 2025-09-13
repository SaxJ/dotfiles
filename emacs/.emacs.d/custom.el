;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ac893acecb0f1cf2b6ccea5c70ea97516c13c2b80c07f3292c21d6eb0cb45239"
     default))
 '(eglot-confirm-server-edits nil nil nil "Customized with use-package eglot")
 '(helm-minibuffer-history-key "M-p")
 '(ignored-local-variable-values '((checkdoc-allow-quoting-nil-and-t . t)))
 '(image-use-external-converter t)
 '(mu4e-search-results-limit -1)
 '(package-selected-packages
   '(aidermacs apheleia avy build cape casual chatgpt-shell chezmoi
               confluence-markup-mode corfu-terminal csproj-mode
               csv-mode denote-journal dired-preview dslide dune eat
               ef-themes eglot-fsharp elfeed-tube ellama elm-mode
               embark-consult epresent evil-collection evil-commentary
               evil-org evil-surround forge general git-timemachine
               gptel graphql-ts-mode graphviz-dot-mode haskell-mode
               helm-org hide-mode-line httprepl hurl-mode jira
               jiralib2 jq-mode js-doc json-mode kubernetes-evil
               lastfm magit-prime marginalia mingus mood-line mpdel
               mpdmacs mpris mu4easy multi-vterm nerd-icons-corfu nvm
               ordered-set orderless org-modern org-music org-ql
               org-roam org-tree-slide origami otpp pocket-reader
               pomo-cat popper pr-review prescient prodigy
               rainbow-mode simple-mpc soundcloud string-inflection
               sudo-edit templ-ts-mode terraform-mode tinee tramp
               tramp-term tuareg vc-use-package vertico vue-mode
               web-mode wgrep yaml-mode yasnippet yeetube yuck-mode
               zone-nyan zone-rainbow))
 '(package-vc-selected-packages
   '((mpris :url "https://code.tecosaur.net/tec/mpris.el" :branch
            "master")
     (confluence-markup-mode :url
                             "https://github.com/rmloveland/confluence-markup-mode"
                             :branch "master")
     (build :url "https://github.com/SaxJ/build.el" :branch "master")
     (org-music :url "https://github.com/debanjum/org-music" :branch
                "master")
     (org-ql :url "https://github.com/alphapapa/org-ql" :branch
             "master")
     (hurl-mode :url "https://github.com/JasZhe/hurl-mode")
     (vc-use-package :vc-backend Git :url
                     "https://github.com/slotThe/vc-use-package")))
 '(safe-local-variable-values
   '((ssh-deploy-async . 0)
     (ssh-deploy-root-remote
      . "/ssh:ubuntu@minikube:/home/ubuntu/megatron/")
     (gac-automatically-add-new-files-p . t)
     (rsync-local-path . "/home/saxonj/Documents/hannibal/")
     (rsync-remote-paths "minikube:/home/ubuntu/hannibal")
     (evil-shift-width . 2)
     (rsync-local-path . "/home/saxonj/Documents/unicron/")
     (rsync-remote-paths "minikube:/home/ubuntu/unicron")
     (evil-shift-width . 4)
     (rsync-excluded-dirs ".git" ".direnv" "node_modules" "vendor")
     (ssh-deploy-async . 1) (ssh-deploy-on-explicit-save . 0)
     (ssh-deploy-root-remote
      . "/sshfs:ubuntu@minikube:/home/ubuntu/megatron/")
     (ssh-deploy-root-local . "/home/saxonj/Documents/megatron/")
     (rsync-local-path . "/home/saxonj/Documents/megatron/")
     (rsync-remote-paths "minikube:/home/ubuntu/megatron")))
 '(tramp-connection-timeout 10))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
