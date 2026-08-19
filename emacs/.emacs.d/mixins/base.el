;;; Emacs Bedrock
;;;
;;; Mixin: Base UI enhancements

;;; Usage: Append or require this file from init.el to enable various UI/UX
;;; enhancements.

;;; Contents:
;;;
;;;  - Motion aids
;;;  - Power-ups: Embark and Consult
;;;  - Minibuffer and completion

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Motion aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package avy
  :ensure t
  :demand t
  :bind (("C-c j" . avy-goto-line)
         ("s-j"   . avy-goto-char-timer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Power-ups: Embark and Consult
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Consult: Misc. enhanced commands
(use-package consult
  :ensure t
  ;; Other good things to bind: consult-ripgrep, consult-line-multi,
  ;; consult-history, consult-outline
  :bind (("C-x b" . consult-buffer) ; orig. switch-to-buffer
         ("M-y" . consult-yank-pop) ; orig. yank-pop
         ("C-s" . consult-line))
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<")
  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip --hidden --glob=!.git/ --glob=!.agent-shell/"))

(use-package embark
  :ensure t
  :demand t
  :after avy
  :bind (("C-c a" . embark-act))        ; bind this to an easy key to hit
  :init
  ;; Add the option to run embark when using avy
  (defun bedrock/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark))

(use-package embark-consult
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer and completion
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :ensure t
  :init
  ;; You'll want to make sure that e.g. fido-mode isn't enabled
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode)

  (defun saxon/giphy-annotate (cand)
    (let* ((thumb (get-text-property 0 'giphy-thumb cand)))
      (if (and thumb (file-exists-p thumb))
          (concat "  " (propertize "*" 'display (create-image thumb nil nil :max-height 120))))))
  
  (add-to-list 'marginalia-annotators '(giphy-gif saxon/giphy-annotate builtin none)))

(use-package cape
  :ensure t)

;; Popup completion-at-point
(use-package corfu
  :ensure t
  :after cape
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible)
  (setq corfu-auto t)

  (setq corfu-auto t
        corfu-popupinfo-delay '(0.5 . 0.1)
        corfu-popupinfo-hide t
        corfu-quit-no-match 'separator)

  :bind
  (:map corfu-map
        ([tab] . corfu-next)
        ("TAB" . corfu-next)
        ([backtab] . corfu-previous)
        ("S-TAB" . corfu-previous)))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package eshell
  :bind (("C-r" . consult-history)))

;; Orderless: powerful completion style
(use-package orderless
  :ensure t
  :after prescient
  :config
  (setq completion-styles '(orderless prescient)))

(use-package otpp
  :ensure t
  :after project
  :init
  (otpp-mode 1)
  ;; If you want to advice the commands in `otpp-override-commands`
  ;; to be run in the current's tab (so, current project's) root directory
  (otpp-override-mode 1))

(use-package prescient
  :ensure t)

(defun saxon/get-mpris-track-title ()
  "Get the title of the currently playing track"
  (unless (eq (mpris-get-metadata) 'no-player)
    (s-truncate 30 (s-trim (format "%s - %s" (mpris-track-attr 'title) (mpris-track-attr 'artist))))))

(defun saxon/clocking-status ()
  "Clearly show when not clocking time."
  (if (string= (car timeclock-last-event) "i")
      (propertize (nth 2 timeclock-last-event) 'face 'success)
    (propertize "Not Clocking" 'face 'error)))

(defun saxon/browse-url-mpv (url &rest args)
  (start-process "mpv" "*mpv*" "mpv" url))

(defun saxon/format-git-modeline ()
  "Clean up the modeline string"
  (when vc-mode
    (replace-regexp-in-string "^ Git[:-]" "\uf418 " vc-mode)))

(use-package emacs
  :ensure nil
  :config
  ;; Text expansion
  (global-set-key [remap dabbrev-expand] 'hippie-expand)

  ;; Mode line
  (setq global-mode-string '(
                             (:eval (mu4e--modeline-string))
                             " 🎵 " (:eval (saxon/get-mpris-track-title))
                             " 🕓 " display-time-string
                             (:eval mu4e-alert-mode-line)))

  (setq-default mode-line-format
                '((:propertize "" mode-line-modified mode-line-remote)
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  " "
                  mode-line-position
                  mode-line-format-right-align

                  (:eval (saxon/clocking-status))
                  " "
                  (:eval (saxon/format-git-modeline))
                  " "
                  mode-line-modes
                  mode-line-misc-info)

                project-mode-line nil
                mode-line-buffer-identification '(" %b")
                mode-line-position-line-format '(" %l:%c"))
  (setopt mode-line-modes-delimiters '("" . "")
          mode-line-collapse-minor-modes t)

  ;; Speeding up tramp
  (setq remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        remote-file-name-inhibit-auto-save-visited t)

  (setq browse-url-handlers '(("https:\\/\\/www\\.youtube." . saxon/browse-url-mpv))))

