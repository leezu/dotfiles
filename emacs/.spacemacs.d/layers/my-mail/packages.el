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
  '((helm-notmuch :requires helm)
    notmuch
    org))

(defun my-mail/init-helm-notmuch ()
  (use-package helm-notmuch
    :defer t
    :init (spacemacs/set-leader-keys "aNn" 'helm-notmuch)))

(defun my-mail/init-notmuch ()
  (use-package notmuch
    :defer t
    :commands notmuch
    :init
    (progn
      (spacemacs/declare-prefix "aN" "notmuch")
      (spacemacs/set-leader-keys
        "oN" 'notmuch
        "on" 'leezu/notmuch-inbox
        "oj" 'notmuch-jump-search
        "os" 'notmuch-search))
    :config
    (progn
      (dolist (prefix '(("ms" . "stash")
                        ("mp" . "part")
                        ("mP" . "patch")))
        (spacemacs/declare-prefix-for-mode 'notmuch-show-mode
          (car prefix) (cdr prefix)))

      ;; display
      (setq notmuch-wash-wrap-lines-length 80)
      ;; setup the mail address and use name
      (setq user-mail-address "leonard@lausen.nl"
            user-full-name "Leonard Lausen"
            message-sendmail-envelope-from 'header)
      ;; add Cc and Bcc headers to the message buffer
      (setq message-default-mail-headers "Cc: \nBcc: \n")
      ;; Directories
      (setq message-directory "~/mail/gandi"
            message-auto-save-directory "~/mail/gandi/Drafts"
            message-kill-buffer-on-exit t)
      ;; Sending mail
      (setq send-mail-function 'sendmail-send-it
            message-send-mail-function 'message-send-mail-with-sendmail
            sendmail-program "~/.local/bin/msmtpq"
            notmuch-fcc-dirs "gandi/Sent")
      ;; Tags
      (setq notmuch-archive-tags '("-inbox" "-autoinbox"))
      ;; Search
      (setq notmuch-search-oldest-first nil)
      (setq notmuch-saved-searches '((:name "inbox"
                                            :query "tag:inbox"
                                            :count-query "tag:inbox tag:unread"
                                            :key "i")
                                     (:name "autoinbox"
                                            :query "tag:autoinbox"
                                            :count-query "tag:autoinbox tag:unread"
                                            :key "I")
                                     (:name "lists"
                                            :query "tag:lists date:1M.."
                                            :count-query "tag:lists tag:unread date:1M.."
                                            :key "l")
                                     (:name "unread" :query "tag:unread" :key "u")
                                     (:name "flagged" :query "tag:flagged" :key "f")
                                     (:name "sent" :query "tag:sent" :key "t")
                                     (:name "drafts" :query "tag:draft" :key "d")
                                     (:name "all mail" :query "*" :key "a")))

      (evil-set-initial-state 'notmuch-tree-mode 'emacs)

      (evil-add-hjkl-bindings notmuch-search-mode-map 'emacs
        "d" 'leezu/notmuch-search-delete-thread
        "S" 'leezu/notmuch-search-spam-thread
        "J" 'notmuch-jump-search
        "K" 'notmuch-tag-jump
        "L" 'notmuch-search-filter)

      (evil-add-hjkl-bindings notmuch-show-mode-map 'emacs
        "d" 'leezu/notmuch-show-delete-message-then-next-or-exit
        "D" 'leezu/notmuch-show-delete-thread-then-next
        "S" 'leezu/notmuch-show-spam-thread-then-next
        "H" 'notmuch-show-toggle-visibility-headers
        "J" 'notmuch-jump-search
        "K" 'notmuch-tag-jump
        "L" 'notmuch-show-filter-thread)

      (evil-add-hjkl-bindings notmuch-tree-mode-map 'emacs
        "d" 'leezu/notmuch-tree-delete-message-then-next
        "D" 'leezu/notmuch-tree-delete-thread
        "S" 'leezu/notmuch-spam-delete-thread
        "J" 'notmuch-jump-search
        "K" 'notmuch-tag-jump))))

(defun my-mail/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (require 'org-notmuch)))

;;; packages.el ends here
