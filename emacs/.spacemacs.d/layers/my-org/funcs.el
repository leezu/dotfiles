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
  "Return t if today is a weekday."
  (let ((wday (nth 6
                   (decode-time))))
    (and (< wday 6)
         (> wday 0))))

(defun leezu/working-p ()
  "Return t on weekdays during office hours."
  (let ((hour (nth 2
                   (decode-time))))
    (and (leezu/weekday-p)
         (or (and (>= hour 10)
                  (<= hour 12))
             (and (>= hour 12)
                  (<= hour 19))))))

(defun leezu-location-cdr ()
  (let (result)
    (dolist (location-list my-org-location-lists result)
      (setq result (cl-member my-org-location location-list :test 'string=)))
    result))

(defun leezu/location-available (location)
  "Return t if location is compatible, i.e. a superset of the current location."
  (cl-member location
             (leezu-location-cdr)
             :test 'string=))

(defun leezu/org-auto-exclude-function (tag)
  (and (cond
        ((cl-member tag my-org-locations :test 'string=) ; location tag
         (not (leezu/location-available tag)))
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