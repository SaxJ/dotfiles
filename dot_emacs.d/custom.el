;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-confirm-server-edits nil nil nil "Customized with use-package eglot")
 '(helm-minibuffer-history-key "M-p")
 '(ignored-local-variable-values '((checkdoc-allow-quoting-nil-and-t . t)))
 '(mu4e-search-results-limit -1)
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((build :url "https://github.com/SaxJ/build.el" :branch "master")
     (org-music :url "https://github.com/debanjum/org-music" :branch
                "master")
     (org-ql :url "https://github.com/alphapapa/org-ql" :branch
             "master")
     (hurl-mode :url "https://github.com/Orange-OpenSource/hurl" :rev
                :newest :lisp-dir "contrib/emacs/")
     (otpp :url "https://github.com/abougouffa/one-tab-per-project"
           :rev :newest)
     (vc-use-package :vc-backend Git :url
                     "https://github.com/slotThe/vc-use-package")))
 '(safe-local-variable-values
   '((rsync-local-path . "/home/saxonj/Documents/hannibal/")
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
     (rsync-remote-paths "minikube:/home/ubuntu/megatron"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
