;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.
;;
;; WARNING: Disabling core packages listed in ~/.emacs.d/core/packages.el may
;; have nasty side-effects and is not recommended.


;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
                                        ;(unpin! t)

;; ...but to unpin a single package:
                                        ;(unpin! pinned-package)
;; Use it to unpin multiple packages
                                        ;(unpin! pinned-package another-pinned-package)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; Dumb Things
(package! slack)

;; Email
(package! bbdb)
(package! dianyou)
(package! gnus-select-account)
(package! gnus-summary-ext)
(package! gnus-desktop-notify)
(package! all-the-icons-gnus)
(package! nnhackernews)

;; Tooling
(package! string-inflection)
(package! dash-functional)
(package! ejira :recipe (:host github :repo "nyyManni/ejira"))
(package! telega)
(package! org-kanban)
(package! ob-mermaid)
(package! org-remark)
(package! todotxt)
(package! multi-vterm)


;; Filetypes
(package! submode :recipe (:host github :repo "nxtr/submode"))
(package! sosl-mode :recipe (:host github :repo "nxtr/sosl-mode"))
(package! soql-mode :recipe (:host github :repo "nxtr/soql-mode"))
(package! apex-mode :recipe (:host github :repo "nxtr/apex-mode"))
(package! shakespeare-mode)
(package! vimrc-mode)
(package! caddyfile-mode)
(package! mermaid-mode)

;; Unpin to non-broken packages
(unpin! lsp-mode)
(unpin! tree-sitter)
(unpin! tree-sitter-langs)

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))
