(setq vc-follow-symlinks t
      find-file-visit-truename t)

;; Git integration
(use-package magit
  :config
  (setq magit-git-environment (setenv-internal magit-git-environment "TZ" "UTC0" nil)
	magit-delete-by-moving-to-trash nil))

;; Language server support
(use-package eglot
  :hook ((python-ts-mode . eglot-ensure)
	 (rust-ts-mode . eglot-ensure)
	 (emacs-lisp-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs `(python-ts-mode . ("rass" "python"))))

;; Tree-sitter
(use-package treesit
  :ensure nil ;; emacs built-in
  :init
  ;; Recipes for tree-sitter grammars.
  ;; Target branch is specified to ensure ABI 14 compatibility of emacs 30
  (setq treesit-language-source-alist
	'((c . ("https://github.com/tree-sitter/tree-sitter-c" "v0.23.6"))
	  (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.23.6"))
	  (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.23.3"))
	  ))
  (setq major-mode-remap-alist
	'((c-mode . c-ts-mode)
	  (python-mode . python-ts-mode)
	  ))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  ;; Install missing tree-sitter grammars
  (dolist (lang (mapcar #'car treesit-language-source-alist))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))

;; LaTeX
(use-package auctex
  :config
  (setq TeX-view-program-selection '((output-pdf "xdg-open"))))

;; List common modes in case emacs-major-editing-modes debian package is not available/installed
(use-package markdown-mode
  :ensure t)
