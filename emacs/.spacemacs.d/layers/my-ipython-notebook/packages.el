;;; packages.el --- my-ipython-notebook layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author:  <leonard@lausen.nl>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst my-ipython-notebook-packages '(ein))

;; Configuration of packages already present in other layers
(defun my-ipython-notebook/post-init-ein ()
  (setq ein:console-security-dir "~/.servers/dycpu2/.ipython/profile_default/security")
  (setq ein:console-args '("--ssh" "dycpu2"))

  ;; (setq ein:console-security-dir
  ;;       '((8891 . "~/.servers/dycpu2/.ipython/profile_default/security")
  ;;         (default . "~/.ipython/profile_default/security")))
  ;; (setq ein:console-args
  ;;       '((8891 . '("--ssh" "dycpu2"))
  ;;         (default . '("--profile" "default"))))
  )
