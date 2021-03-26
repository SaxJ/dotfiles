;;; cli.el -*- lexical-binding: t; -*-

(add-hook! 'doom-sync-pre-hook
  (or (not (getenv "DIRENV_DIR"))
      doom-auto-accept
      (y-or-n-p "doom env update: Direnv detected! Continue?")
      (user-error "Aborted")))
