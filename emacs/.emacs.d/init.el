;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;; Monospace fonts that support Claude Code well
(when (display-graphic-p)
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-12")
  (set-fontset-font t 'symbol "DejaVu Sans Mono" nil 'prepend))

(load-file (expand-file-name "+core.el" user-emacs-directory))
(require 'general)

(load-file (expand-file-name "+programming.el" user-emacs-directory))
(load-file (expand-file-name "+org.el" user-emacs-directory))
(load-file (expand-file-name "+llm.el" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
