;;; saxon-php.el --- PHP Config -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Setup completion packages.  Completion in this sense is more like
;; narrowing, allowing the user to find matches based on minimal
;; inputs and "complete" the commands, variables, etc from the
;; narrowed list of possible choices.

;;; Code:

(crafted-package-install-package 'php-mode)

(setq eglot-server-programs '((php-mode . ("intelephense" "--stdio"))))

(add-hook 'php-mode-hook #'eglot-ensure)

(provide 'saxon-php)
