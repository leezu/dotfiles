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

;; Variables
(defvar my-org-location "@anywhere" "Current location. Used to filter out tasks in the org-agenda tahta are out of reach.")

;; Org mode tags may not contain "-"
(defvar my-org-location-lists '(("@hongkong" "@asia" "@anywhere")
                                ("@seoul" "@korea" "@asia" "@anywhere")
                                ("@us" "@america" "@anywhere")
                                ("@freiburg" "@germany" "@europe" "@anywhere")))

(let (string-cl-union)
  (setq string-cl-union (lambda (list1 list2)
                          "cl-union  using string= as compare function"
                          (cl-union list1 list2 :test 'string=)))
  (defvar my-org-locations (cl-reduce string-cl-union my-org-location-lists))
  "List of all valid locations.")
