;;; funcs.el --- my-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2017 Leonard Lausen
;;
;; Author:  <leonard@lausen.nl>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun notmuch/switch-keys (keymap keyslist)
  "Swaps OLDKEY with NEWKEY in KEYMAP without changing the key's
definition"
  (dolist (keys keyslist)
    (let ((oldkey (car keys))
          (newkey (car (cdr keys))))
      (define-key keymap (kbd newkey) (lookup-key keymap (kbd oldkey)))
      (define-key keymap (kbd oldkey) nil))))

(defun notmuch/add-key (keymap keybindlist)
  "Adds NEWKEY to KEYMAP to call FUNC"
  (dolist (keybind keybindlist)
    (let ((newkey (car keybind))
          (func (car (cdr keybind))))
      (define-key keymap (kbd newkey) func))))

(defun leezu-notmuch/show-delete-message ()
  "Deletes currently shown message"
  (interactive)
  (notmuch-show-tag '("+deleted" "-inbox" "-autoinbox" "-unread"))
  (unless (notmuch-show-next-open-message)
    (notmuch-show-next-thread t)))

(defun leezu-notmuch/show-previous-message ()
  "Show previous message, possibley in different thread."
  (interactive)
  (if (notmuch-show-goto-message-previous)
      (notmuch-show-message-adjust)
    (notmuch-show-next-thread t t)))

(defun leezu-notmuch/show-next-message ()
  "Show next message, possibley in different thread."
  (interactive)
  (if (notmuch-show-goto-message-next)
      (notmuch-show-message-adjust)
    (notmuch-show-next-thread t)))

(defun leezu-notmuch/message-delete (go-next)
  "Delete message and select GO-NEXT message."
  (notmuch-search-tag '("+deleted" "-inbox" "-unread"))
  (if (eq 'up go-next )
      (notmuch-search-previous-thread)
    (notmuch-search-next-thread)))
