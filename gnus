;;; .gnus --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Saxon Jensen
;;
;; Author: Saxon Jensen <https://github.com/SaxJ>
;; Maintainer: Saxon Jensen
;; Created: October 13, 2021
;; Modified: October 13, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/SaxJ/.gnus
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(setq user-mail-address "thing@gake.fake"
      user-full-name "Saxon Jensen")
(load-library "smtpmail")
(load-library "nnimap")
(load-library "starttls")
(require 'nnir)

(setq gnus-select-method '(nnimap "gmail-work"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-authinfo-file "~/.authinfo.gpg")
                                  (nnir-search-engine imap)
                                  (nnimap-stream ssl)))
(add-hook 'gnus-topic-mode-hook 'gnus-topic-mode)

;; (add-to-list 'gnus-secondary-select-methods '((nnimap "gmail-work"
;;                                                      (nnimap-address "imap.gmail.com")
;;                                                      (nnimap-server-post "imaps")
;;                                                      (nnimap-stream "ssl")
;;                                                      (nnimap-expiry-wait immediate))))

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(provide '.gnus)
;;; .gnus ends here
