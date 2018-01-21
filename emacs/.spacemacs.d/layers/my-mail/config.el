;;; config.el --- my-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2017 Leonard Lausen
;;
;; Author:  <leonard@lausen.nl>
;; URL: https://github.com/leezu/dotfiles
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(defvar notmuch-delete-tags '("+deleted" "-inbox")
  "Tags applied when deleting a message.")

(defvar notmuch-spam-tags '("+spam" "-inbox")
  "Tags applied when marking a message as spam.")

(defvar notmuch-kill-tags '("+killed" "-inbox")
  "Tags applied when marking a message as killed.")
