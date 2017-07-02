;;; packages.el --- package-lint layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author:  <leonard@lausen.nl>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst package-lint-packages
  '(package-lint))

(defun package-lint/init-package-lint ()
  (use-package package-lint))

;;; packages.el ends here
