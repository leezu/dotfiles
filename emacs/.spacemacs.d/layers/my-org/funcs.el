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

;;; * Misc
(defun get-todays-journal-file-name ()
  "Gets the journal file name for today's date"
  (expand-file-name (format-time-string "%Y/%Y-%2m.org") org-journal-dir))

(defun get-todays-review-file-name ()
  "Gets the journal file name for today's date"
  (expand-file-name (format-time-string "%Y-review.org.gpg") org-journal-dir))

(defun load-todays-journal-file ()
  "Create an load a journal entry based on today's date"
  (interactive)
  (find-file (get-todays-journal-file-name)))
;;; * Agenda
(defun my/agenda-review (&args)
  (interactive)
  (let ((org-super-agenda-groups
         '((:name "Unscheduled Tasks"
                  :and (:todo ("TODO" "NEXT" "INPROGRESS")
                              :deadline nil
                              :scheduled nil))
           (:name "Refile" :tag "REFILE")
           (:name "Waiting" :tag "WAITING")
           (:discard (:anything t)))))
    (org-todo-list)))

(defun my/agenda-workflow (&args)
  (interactive)
  (let ((org-super-agenda-groups
         '((:name "Ready for Work" :todo "READY")
           (:name "Active Projects" :todo "ACTIVE")
           (:name "Waiting on External" :todo "WAIT")
           (:name "In Review" :todo "REVIEW")
           (:name "In Planning" :todo "PLAN")
           (:name "Project Backlog" :todo "BACKLOG")
           (:name "Completed Projects" :todo "COMPLETED")
           (:name "Cancelled Projects" :todo "CANC")
           (:discard (:anything t)))))
    (org-todo-list)))

;;; * Agenda bulk actions
(defun my/org-agenda-bulk-set-priority-c ()
  (interactive "P")
  (let* ((marker (or (org-get-at-bol 'org-hd-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker)))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char marker)
          (org-back-to-heading t)
          (org-priority ?C))))))

(defun my/org-agenda-bulk-set-priority-b ()
  (interactive "P")
  (let* ((marker (or (org-get-at-bol 'org-hd-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker)))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char marker)
          (org-back-to-heading t)
          (org-priority ?B))))))

(defun my/org-agenda-bulk-set-priority-a ()
  (interactive "P")
  (let* ((marker (or (org-get-at-bol 'org-hd-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker)))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char marker)
          (org-back-to-heading t)
          (org-priority ?A))))))
;;; * Clock
(defun my/check-for-clock-out-note()
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let ((tags (org-get-tags)))
      (and tags (message "tags: %s " tags)
           (when (member "clocknote" tags)
             (org-add-note))))))

;;; * Pomodoro
(defun my/toggle-music (action)
  (let ((command (concat "playerctl --player=spotify " action)))
    (shell-command command)))
