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


(defun leezu/weekday-p ()
  (let ((wday (nth 6
                   (decode-time))))
    (and (< wday 6)
         (> wday 0))))

(defun leezu/working-p ()
  (let ((hour (nth 2
                   (decode-time))))
    (and (leezu/weekday-p)
         (or (and (>= hour 10)
                  (<= hour 12))
             (and (>= hour 12)
                  (<= hour 19))))))

(defun leezu/org-auto-exclude-function (tag)
  (and (cond
        ((string= tag "@home")
         (leezu/working-p))
        ((string= tag "@office")
         (not (leezu/working-p)))
        ((or (string= tag "@errand")
             (string= tag "PHONE"))
         (let ((hour (nth 2
                          (decode-time))))
           (or (< hour 8)
               (> hour 21)))))
       (concat "-" tag)))
