;;; packages.el
;;
;; Copyright (c) 2018 Leonard Lausen
;;
;; Author:  <leonard@lausen.nl>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst my-devel-packages '((magithub :requires magit)))

(defun my-devel/init-magithub ()
  (use-package magithub
    :after magit
    :ensure t
    :config (magithub-feature-autoinject t))
  (with-eval-after-load 'magithub ;; magithub limit filters
    (require 'parse-time)
    (defmacro magithub--time-number-of-days-since-string (iso8601)
      `(time-to-number-of-days (time-since (parse-iso8601-time-string (concat ,iso8601 "+00:00")))))
    (defun issue-filter-to-days (days type)
      `(lambda (issue)
         (let ((created_at (magithub--time-number-of-days-since-string (alist-get 'created_at issue)))
               (updated_at (magithub--time-number-of-days-since-string (alist-get 'updated_at issue))))
           (or (< created_at ,days)
               (< updated_at ,days)))))
    (defun magithub-filter-maybe (&optional limit)
      "Add filters to magithub only if number of issues is greter than LIMIT."
      (let ((max-issues (length (ignore-errors (magithub-issues))))
            (max-pull-requests (length (ignore-errors (magithub-pull-requests))))
            (limit (or limit 15)))
        (when (> max-issues limit)
          (add-to-list (make-local-variable 'magithub-issue-issue-filter-functions)
                       (issue-filter-to-days limit "issues")))
        (when (> max-pull-requests limit)
          (add-to-list (make-local-variable 'magithub-issue-pull-request-filter-functions)
                       (issue-filter-to-days limit "pull-requests")))))
    (add-to-list 'magit-status-mode-hook #'magithub-filter-maybe)))

;;; packages.el ends here
