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
  (notmuch-show-tag '("+deleted" "-inbox" "-unread"))
  (unless (notmuch-show-next-open-message)
    (notmuch-show-next-thread t)))
