;;; ../.dotfiles/emacs/.doom.d/+mail.el -*- lexical-binding: t; -*-

;; Adjust evil-collection default keybindings
(defvar my-notmuch-kill-tags '("+killed" "-inbox" "-autoinbox"))
(setq +notmuch-delete-tags (list "+deleted" "-inbox" "-autoinbox")
      +notmuch-spam-tags (list "+spam" "-inbox" "-autoinbox"))
(defun my-notmuch/search-kill ()
  (interactive)
  (notmuch-search-add-tag my-notmuch-kill-tags)
  (notmuch-tree-next-message))
(defun my-notmuch/tree-kill ()
  (interactive)
  (notmuch-tree-add-tag my-notmuch-kill-tags)
  (notmuch-tree-next-message))
(defun my-notmuch/show-kill ()
  "Mark email for deletion in notmuch-show"
  (interactive)
  (notmuch-show-add-tag my-notmuch-kill-tags)
  (notmuch-show-next-thread-show))
(defun my/evil-notmuch (mode _mode-keymaps &rest _rest)
  (evil-define-key 'normal notmuch-show-mode-map
    "d" '+notmuch/show-delete
    "K" 'my-notmuch/show-kill)
  (evil-define-key 'normal notmuch-tree-mode-map
    "d" '+notmuch/tree-delete
    "S" '+notmuch/tree-spam
    "K" 'my-notmuch/tree-kill)
  (evil-define-key 'normal notmuch-search-mode-map
    "d" '+notmuch/search-delete
    "S" '+notmuch/search-spam
    "K" 'my-notmuch/search-kill))
(add-hook 'evil-collection-setup-hook 'my/evil-notmuch)


(after! notmuch
  (require 'notmuch-maildir-fcc)
  (setq +notmuch-sync-backend "muchsync"
        +notmuch-mail-folder "~/mail"
        notmuch-crypto-process-mime nil  ;; crypto
        notmuch-wash-wrap-lines-length 80  ;; display
        message-default-mail-headers "Cc: \nBcc: \n"
        message-sendmail-envelope-from 'header
        message-directory "~/mail/uberspace"
        message-auto-save-directory "~/mail/uberspace/Drafts"
        message-kill-buffer-on-exit t)

  ;; html
  (setq mm-text-html-renderer 'shr
        shr-width 80
        shr-use-colors nil
        notmuch-multipart/alternative-discouraged
        '("text/plain" "text/html" "multipart/related"))

  ;; Maximize windows
  (add-hook 'notmuch-show-hook 'delete-other-windows)
  (add-hook 'notmuch-search-mode-hook 'delete-other-windows)

  ;; hello
  ;; https://notmuchmail.org/emacstips/#index4h2
  (add-hook 'notmuch-hello-refresh-hook
            (lambda ()
              (if (and (eq (point) (point-min))
                       (search-forward "Saved searches:" nil t))
                  (progn
                    (forward-line)
                    (widget-forward 1))
                (if (eq (widget-type (widget-at)) 'editable-field)
                    (beginning-of-line)))))

  ;;
  ;; Composing
  (eval-after-load "message"
    ;; Make sure message is loaded before attempting to overwrite defun
    '(defun message-make-date (&optional now)
       "Make a valid data header in UTC.
If NOW, use that time instead."
       (let ((system-time-locale "C"))
         (format-time-string "%a, %d %b %Y %T %z" now t))))
  ;;
  ;; Sending mail
  (setq send-mail-function 'sendmail-send-it
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "~/.local/bin/msmtpq"
        notmuch-fcc-dirs "uberspace/Sent")

  ;; Tags
  (setq notmuch-archive-tags '("-inbox" "-autoinbox"))
  ;; Search
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
                                 (:name "all mail" :query "*" :key "A"))))
