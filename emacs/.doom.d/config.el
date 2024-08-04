;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Load Environment
;; Workaround https://github.com/hlissner/doom-emacs/issues/5760
(setenv "TZ" "America/New_York")
(if (file-exists-p "~/.doom.d/env")
    (doom-load-envvars-file "~/.doom.d/env"))
(eval-when-compile (require 'cl-lib))
(with-eval-after-load 'magit
  (cl-callf setenv-internal magit-git-environment "TZ" "UTC0" nil))

;; Overwrite bindings
(map! ;; Frame-local font resizing
      :n "C-="  #'doom/increase-font-size
      :n "C--"  #'doom/decrease-font-size)

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(setq evil-snipe-override-evil-repeat-keys nil)
(setq doom-localleader-key ",")

(setq user-full-name "Leonard Lausen"
      user-mail-address "leonard@lausen.nl")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(load! "+org.el")
(load! "+mail.el")

(add-hook 'python-mode-hook 'eglot-ensure)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-mode . ("~/.local/bin/ruff" "server"))))
