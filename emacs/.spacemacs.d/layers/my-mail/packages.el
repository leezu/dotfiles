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
  '(notmuch
    ol-notmuch
    org
    vdirel
    ))

(defun my-mail/init-ol-notmuch ()
  (use-package ol-notmuch
    :defer t))

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

      ;; html emails
      (setq mm-text-html-renderer 'shr
            shr-width 80
            shr-use-colors nil
            notmuch-multipart/alternative-discouraged
            '("text/plain" "text/html" "multipart/related"))

      ;; crypto
      (setq notmuch-crypto-process-mime nil)

      ;; display
      (setq notmuch-wash-wrap-lines-length 80)
      ;; setup the mail address and use name
      (setq user-mail-address "leonard@lausen.nl"
            user-full-name "Leonard Lausen"
            message-sendmail-envelope-from 'header)
      ;; add Cc and Bcc headers to the message buffer
      (setq message-default-mail-headers "Cc: \nBcc: \n")
      ;; Directories
      (setq message-directory "~/mail/uberspace"
            message-auto-save-directory "~/mail/uberspace/Drafts"
            message-kill-buffer-on-exit t)
      ;; Composing
      (eval-after-load "message"
        ;; Make sure message is loaded before attempting to overwrite defun
        '(defun message-make-date (&optional now)
           "Make a valid data header in UTC.
If NOW, use that time instead."
           (let ((system-time-locale "C"))
             (format-time-string "%a, %d %b %Y %T %z" now t))))
      ;; Sending mail
      (setq send-mail-function 'sendmail-send-it
            message-send-mail-function 'message-send-mail-with-sendmail
            sendmail-program "~/.local/bin/msmtpq"
            notmuch-fcc-dirs "uberspace/Sent")
      ;; Tags
      (setq notmuch-archive-tags '("-inbox" "-autoinbox"))
      ;; Search
      (setq notmuch-search-oldest-first nil)
      (setq notmuch-saved-searches '((:name "inbox"
                                            :query "tag:inbox"
                                            :count-query "tag:inbox tag:unread"
                                            :key "i")
                                     (:name "autoinbox"
                                            :query "tag:autoinbox and -tag:feeds and -tag:academic and -tag:chinese and -tag:news and -tag:software"
                                            :count-query "tag:autoinbox and tag:unread and -tag:feeds and -tag:academic and -tag:chinese and -tag:news and -tag:software"
                                            :key "I")
                                     (:name "academic"
                                            :query "tag:autoinbox and tag:academic"
                                            :count-query "tag:autoinbox and tag:academic and tag:unread"
                                            :key "a")
                                     (:name "chinese"
                                            :query "tag:autoinbox and tag:chinese"
                                            :count-query "tag:autoinbox and tag:chinese and tag:unread"
                                            :key "c")
                                     (:name "news"
                                            :query "tag:autoinbox and tag:news"
                                            :count-query "tag:autoinbox and tag:news and tag:unread"
                                            :key "n")
                                     (:name "feeds"
                                            :query "tag:autoinbox and tag:feeds"
                                            :count-query "tag:autoinbox and tag:feeds and tag:unread"
                                            :key "f")
                                     (:name "software"
                                            :query "tag:autoinbox and tag:software"
                                            :count-query "tag:autoinbox and tag:software and tag:unread"
                                            :key "s")
                                     (:name "lists"
                                            :query "tag:lists and tag:unread and date:1M.."
                                            :count-query "tag:lists and tag:unread and date:1M.."
                                            :key "l")
                                     (:name "unread" :query "tag:unread" :key "u")
                                     (:name "flagged" :query "tag:flagged" :key "F")
                                     (:name "sent" :query "tag:sent" :key "t")
                                     (:name "drafts" :query "tag:draft" :key "d")
                                     (:name "all mail" :query "*" :key "A")))

      (evil-set-initial-state 'notmuch-tree-mode 'emacs)

      (evil-add-hjkl-bindings notmuch-search-mode-map 'emacs
        "d" 'leezu/notmuch-search-delete-thread
        "S" 'leezu/notmuch-search-spam-thread
        "J" 'notmuch-jump-search
        "A" 'leezu/notmuch-search-kill-thread
        "K" 'leezu/notmuch-search-kill-thread
        "T" 'notmuch-tag-jump
        "L" 'notmuch-search-filter)

      (evil-add-hjkl-bindings notmuch-show-mode-map 'emacs
        "d" 'leezu/notmuch-show-delete-message-then-next-or-exit
        "D" 'leezu/notmuch-show-delete-thread-then-next
        "S" 'leezu/notmuch-show-spam-thread-then-next
        "H" 'notmuch-show-toggle-visibility-headers
        "J" 'notmuch-jump-search
        "K" 'leezu/notmuch-show-kill-thread-then-next
        "T" 'notmuch-tag-jump
        "L" 'notmuch-show-filter-thread)

      (evil-add-hjkl-bindings notmuch-tree-mode-map 'emacs
        "d" 'leezu/notmuch-tree-delete-message-then-next
        "D" 'leezu/notmuch-tree-delete-thread
        "S" 'leezu/notmuch-spam-delete-thread
        "J" 'notmuch-jump-search
        "K" 'leezu/notmuch-tree-kill-thread
        "T" 'notmuch-tag-jump))))

(defun my-mail/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (require 'ol-notmuch)))

(defun my-mail/init-vdirel ()
  (use-package vdirel
    :init (setq vdirel-repository "~/.contacts/personal")
    :defer t)
  )
;;; packages.el ends here
