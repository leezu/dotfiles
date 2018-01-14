;;; packages.el --- my-mail layer packages file for Spacemacs.
;;
;; Copyright (c) 2017 Leonard Lausen
;;
;; Author:  <leonard@lausen.nl>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst my-mail-packages
  '(
    ;; Packages already in notmuch layer
    notmuch))

;; Configuration of packages already present in other layers
(defun my-mail/post-init-notmuch ()
  ;; setup the mail address and use name
  (setq user-mail-address "leonard@lausen.nl"
        user-full-name "Leonard Lausen")
  ;; add Cc and Bcc headers to the message buffer
  (setq message-default-mail-headers "Cc: \nBcc: \n")
  ;; postponed message is put in the following draft directory
  (setq message-directory "~/mail/gandi"
        message-auto-save-directory "~/mail/gandi/Drafts"
        message-kill-buffer-on-exit t)
  ;; Sending mail
  (setq send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "mail.gandi.net"
        smtpmail-smtp-service 587)
  ;; Tags
  (setq notmuch-archive-tags '("-inbox" "-autoinbox"))
  ;; Search
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-saved-searches '((:name "inbox"
                                        :query "tag:inbox"
                                        :count-query "tag:inbox and tag:unread"
                                        :key "i")
                                 (:name "autoinbox"
                                        :query "tag:autoinbox"
                                        :count-query "tag:autoinbox and tag:unread"
                                        :key "I")
                                 (:name "unread" :query "tag:unread" :key "u")
                                 (:name "flagged" :query "tag:flagged" :key "f")
                                 (:name "sent" :query "tag:sent" :key "t")
                                 (:name "drafts" :query "tag:draft" :key "d")
                                 (:name "all mail" :query "*" :key "a")))

  ;; Refresh mail - via https://kkatsuyuki.github.io/notmuch-conf/
  (defun notmuch-exec-offlineimap ()
    "execute offlineimap"
    (interactive)
    (set-process-sentinel
     (start-process-shell-command "offlineimap"
                                  "*offlineimap*"
                                  "offlineimap -o")
     '(lambda (process event)
        (notmuch-refresh-all-buffers)
        (let ((w (get-buffer-window "*offlineimap*")))
          (when w
            (with-selected-window w (recenter (window-end)))))))
    (popwin:display-buffer "*offlineimap*"))

  (add-to-list 'popwin:special-display-config
               '("*offlineimap*" :dedicated t :position bottom :stick t
                 :height 0.4 :noselect t))

  ;; Evilify the default notmuch modes
  (dolist (mode '(notmuch-hello-mode notmuch-search-mode notmuch-show-mode))
    (evil-set-initial-state mode 'evilified))

  ;; Remaps all conficting keybindings.
  ;; TODO Don't use eval-after-load (maybe)
  (with-eval-after-load 'notmuch
    (notmuch/switch-keys notmuch-common-keymap '(("j"  ";")))

    (notmuch/switch-keys notmuch-hello-mode-map '(("v" "_")))

    (notmuch/switch-keys notmuch-tree-mode-map '(("A" "O")
                                                 ("a" "o")
                                                 ("v" "a")
                                                 ("V" "A")
                                                 ("n" "J")
                                                 ("N" "H")
                                                 ("p" "K")
                                                 ("P" "L")
                                                 ("k" ":")
                                                 ("SPC" "C-L")
                                                 ("DEL" "C-H")))

    (notmuch/switch-keys notmuch-search-mode-map '(("SPC" "C-L")
                                                   ("DEL" "C-H")
                                                   ("n" "J")
                                                   ("p" "K")
                                                   ("l" "\"")
                                                   ("k" ":")
                                                   ("a" "o")))

    (notmuch/switch-keys notmuch-show-mode-map '(("SPC" "C-L")))

    (notmuch/add-key notmuch-tree-mode-map '(("d" spacemacs/notmuch-message-delete-down)
                                             ("D" spacemacs/notmuch-message-delete-up)
                                             ("M" compose-mail-other-frame)))

    (notmuch/add-key notmuch-show-mode-map '(("d" leezu-notmuch/show-delete-message)
                                             ("J" leezu-notmuch/show-next-message)
                                             ("K" leezu-notmuch/show-previous-message)
                                             ))

    (notmuch/add-key notmuch-search-mode-map '(("a" spacemacs/notmuch-search-archive-thread-down)
                                               ("A" spacemacs/notmuch-search-archive-thread-up)
                                               ("d" spacemacs/notmuch-message-delete-down)
                                               ("D" spacemacs/notmuch-message-delete-up)
                                               ("J" notmuch-jump-search)
                                               ("L" notmuch-search-filter)
                                               ("gg" notmuch-search-first-thread)
                                               ("gr" notmuch-refresh-this-buffer)
                                               ("gR" notmuch-refresh-all-buffers)
                                               ("G" notmuch-search-last-thread)
                                               ("M" compose-mail-other-frame))))

  ;; Dynamic evil-states based off of text property Treats text-boxes like
  ;; regular text files This is seperate from the fix, it's just another fix to
  ;; an evil/notmuch issue.
  (add-hook 'notmuch-hello-mode-hook
   (lambda ()
     (add-hook 'post-command-hook
      (lambda ()
        (let (
              ;; Everything else that isn't `editable' for notmuch-hello should
              ;; be read-only
              (editable (get-char-property (point) 'field))
              (is-evilified (string= evil-state "evilified"))
              (is-normal (string= evil-state "normal"))
              (is-insert (string= evil-state "insert")))
          (cond ((and is-evilified editable)
                 (evil-normal-state)
                 (define-key evil-motion-state-map (kbd "RET")
                 'widget-field-activate))
                ((and (or is-normal is-insert) (not editable))
                 (evil-evilified-state))))) nil t))))

;;; packages.el ends here
