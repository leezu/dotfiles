(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nonfree" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")
      ))

;; Core settings
(setq use-package-always-ensure nil  ; set to t on OS that doesn't package elpa
      help-window-select t  ; switch to help buffer when opened
      )

; Builtin functionality
(use-package recentf
  :init
  (setq
    recentf-save-file "~/.cache/emacs/recentf"
    recentf-max-saved-items 10000
    recentf-max-menu-items 5000
    )
  (recentf-mode 1)
  (run-at-time nil (* 5 60) 'recentf-save-list)
  )

;; Evil mode (vim emulation)
(use-package evil
  :init (setq evil-want-keybinding nil
	      evil-want-C-u-scroll nil
	      )
  :config
  (evil-mode 1)
  (setq evil-emacs-state-modes (append evil-emacs-state-modes '(eat-mode magit-mode dired-mode info-mode)))
  (add-hook 'magit-blame-mode-hook
            (lambda ()
              (if magit-blame-mode
                  (evil-emacs-state)
                (evil-normal-state))))

  (evil-define-key 'normal org-mode-map
    (kbd "TAB") 'org-cycle
    (kbd "<tab>") 'org-cycle))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; General for leader key setup
(use-package general
  :ensure t  ; always download
  :after evil
  :config
  (general-create-definer my/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; Define leader key mappings (matching Doom conventions)
  (my/leader-keys
    "SPC" 'execute-extended-command

    ;; Files
    "f f" 'find-file
    "f d" 'delete-file
    "f p" 'project-find-file
    "f r" 'consult-recent-file
    "f s" 'save-buffer

    ;; Buffers
    "b b" 'consult-buffer
    "b k" 'kill-buffer
    "b d" 'kill-current-buffer
    "b ]" 'next-buffer
    "b [" 'previous-buffer

    ;; Windows
    "w w" 'other-window
    "w h" 'windmove-left
    "w j" 'windmove-down
    "w k" 'windmove-up
    "w l" 'windmove-right
    "w v" 'split-window-right
    "w s" 'split-window-below
    "w d" 'delete-window
    "w m" 'delete-other-windows

    ;; Git
    "g g" 'magit-status
    "g b" 'magit-blame

    ;; Org
    "o a" 'org-agenda
    "o c" 'org-capture

    ;; Search
    "s s" 'consult-line
    "s g" 'consult-grep
    "s p" 'project-search

    ;; Help
    "h f" 'describe-function
    "h v" 'describe-variable
    "h k" 'describe-key))


;; Completion
(use-package vertico
  :config (vertico-mode 1))
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
(use-package consult
  :bind (("C-s" . consult-line)))
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
(use-package corfu
  :custom
  (tab-always-indent 'complete)  ; emacs setting
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :init
  (global-corfu-mode))


;; UI improvements
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

;; Which-key for discovering keybindings
(use-package which-key
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.3))

;; File management
(setq make-backup-files nil)
(use-package dirvish
  :ensure t  ; download
  :config
  (dirvish-override-dired-mode))
