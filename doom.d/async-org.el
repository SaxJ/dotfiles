;;; async-org.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Saxon Jensen
;;
;; Author: Saxon Jensen <https://github.com/SaxJ>
;; Maintainer: Saxon Jensen <saxon.jensen@gmail.com>
;; Created: November 13, 2021
;; Modified: November 13, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/SaxJ/async-org
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(require 'org)
(require 'ox)
(add-to-list 'load-path "~/.emacs.d/.local/straight/repos/org-mode/contrib/lisp/")
(require 'ox-koma-letter)
(require 'ox-org)

;;; async-org.el ends here
