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

(defun spacemacs//notmuch-inbox-p (saved-search-property-item)
  "Returns non-nil if item is the inbox."
  (string-equal (plist-get saved-search-property-item :name) "inbox"))

(defun leezu/notmuch-inbox ()
  "Search inbox."
  (interactive)
  (notmuch-search
   (plist-get (nth 0 (-filter 'spacemacs//notmuch-inbox-p notmuch-saved-searches))
              :query)))
