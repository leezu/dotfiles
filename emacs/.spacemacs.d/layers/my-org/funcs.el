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

(defun my/org-auto-exclude-function (tag)
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

;;; * Time zone handling
(defun my/org-global-props (&optional property buffer)
  "Get the plists of global org properties of current buffer."
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-match property (org-element-property :key el)) el)))))

(defun my/org-global-prop-value (key)
  "Get global org property KEY of current buffer."
  (org-element-property :value (car (my/org-global-props key))))

(defun my/org-check-timezone-property ()
  "Check if file TIMEZONE property matches emacs local timezone and warn otherwise"
  ( let ((file-timezone (my/org-global-prop-value "TIMEZONE")))
  (if (not (equal (current-time-zone nil file-timezone) (current-time-zone)))
      (display-warning
       '("my-org")
       (format "File time %s and emacs time %s zone mismatch %s. \
Consider calling my/org-change-timestamps-to-local-timezone \
while visiting the respective file \
and updating the TIMEZONE file property manually."
               (current-time-zone nil file-timezone)
               (current-time-zone)
               (buffer-name))))))

(defun my/org-change-timestamps-to-local-timezone ()
  "Compute offset between local and file timezone, update timestamps to match local."
  (interactive)
  (let ((minute-offset
         (/ (-
             (car (current-time-zone))
             (car (current-time-zone nil (my/org-global-prop-value "TIMEZONE"))))
            60)))
    (progn
      (message (format "Applying %s hours offset." (/ minute-offset (float 60))))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "[[<]" nil t)
          (when (org-at-timestamp-p 'lax)
            (org-timestamp-change minute-offset 'minute)))))))

;; via https://github.com/ekaschalk/.spacemacs.d/blob/58e266097efda77366fbd7ccafa0313ff5c491b4/layers/macros/local/macros/macros.el
(defun define-keys (keymap &rest pairs)
  "Define alternating key-def PAIRS for KEYMAP."
  (-each
      (-partition 2 pairs)
    (-lambda ((key def))
      (define-key keymap key def))))
