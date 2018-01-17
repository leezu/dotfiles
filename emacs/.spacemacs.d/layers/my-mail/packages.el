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
  ;; display
  (setq notmuch-wash-wrap-lines-length 80)
  ;; setup the mail address and use name
  (setq user-mail-address "leonard@lausen.nl"
        user-full-name "Leonard Lausen")
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

  (setq evil-emacs-state-modes (append notmuch-show-mode evil-emacs-state-modes))

  (evil-add-hjkl-bindings notmuch-search-mode-map 'emacs
    "J" 'notmuch-jump-search
    "K" 'notmuch-tag-jump
    "L" 'notmuch-search-filter)

  (evil-add-hjkl-bindings notmuch-show-mode-map 'emacs
    "H" 'notmuch-show-toggle-visibility-headers
    "J" 'notmuch-jump-search
    "K" 'notmuch-tag-jump
    "L" 'notmuch-show-filter-thread)

  (evil-add-hjkl-bindings notmuch-show-mode-map 'emacs
    "J" 'notmuch-jump-search
    "K" 'notmuch-tag-jump)))

;;; packages.el ends here
