;;; packages.el --- my-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2017 Leonard Lausen
;;
;; Author:  <leonard@lausen.nl>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; list of packages
(defconst my-org-packages
  '(
    ;; Packages owned by org layer
    org
    org-agenda
    org-pomodoro

    ;; Packages owned by bibtex layer
    org-ref
    helm-bibtex

    ;; Owned packages
    outshine
    helm-navi
    interleave
    org-alert
    helm-org-rifle
    org-super-agenda
    git-auto-commit-mode
    ))

;;; Packages owned by other layers
;;;; org
(defun my-org/post-init-org ()
  (setq org-adapt-indentation nil  ;; Disable indentation in org mode
        org-catch-invisible "error"  ;; Cancel invisible edits
        org-enforce-todo-dependencies t
        org-agenda-window-setup 'current-window)

;;;;; Capture
  (setq org-capture-templates
        ;; note the backquote ` instead of normal quote '
        `(("P" "New project" entry (file+olp "~/org/organizer.org" "Projects")
           "* PLAN %?\nOPENED: %U\n%a\n\n%i")
          ("b" "New project (backlog)" entry (file+olp "~/org/organizer.org" "Projects")
           "* BACKLOG %?\nOPENED: %U\n%a\n\n%i")
          ("t" "Task" entry (file+olp "~/org/organizer.org" "Tasks")
           "* TODO [#B]%?\nOPENED: %U\n%a\n\n%i")
          ("s" "Clocked entry subtask" entry (clock)
           "* TODO [#B]%?\nOPENED: %U\n%a\n\n%i" :empty-lines 1)
          ("c" "Clocked entry checkbox" checkitem (clock) "[ ] %i")
          ("w" "Waiting for" entry (file+olp "~/org/organizer.org" "Tasks")
           "* WAITING %?\nOPENED: %U\n%a\n\n%i")
          ("s" "Someday" entry (file+olp "~/org/organizer.org" "Someday")
           "* SOMEDAY %?\nOPENED: %U\n%a\n\n%i")

          ;; notes
          ("n" "Note in current file" entry (file+olp+datetree "~/org/organizer.org")
           "* %?\nCREATED: %U\n%a\n\n%i" :prepend t)
          ("l" "Note in current file" entry (file+olp+datetree buffer-file-name)
           "* %?\nCREATED: %U\n%a\n\n%i" :prepend t)

          ;; snippets
          ("C" "Code Snippet" entry (file+olp+datetree "~/org/snippets.org")
           ;; Prompt for tag and language
           "* %?\t:%^{language}:\nCREATED: %U\n%a\n#+BEGIN_SRC %\\1\n%i\n#+END_SRC")

          ;; journal
          ("j" "Journal" entry (file+olp+datetree ,(get-todays-journal-file-name))
           "* %?\nEntered on %U\n%a\n")
          ("r" "Journal (review)" entry (file+olp+datetree ,(get-todays-review-file-name))
           "* %?\nEntered on %U\n%a\n" :clock-in t :clock-resume t)
          ("e" "Event" entry (file+olp+datetree ,(get-todays-journal-file-name))
           "* %?\nSCHEDULED: %t\n%a\n")

          ;; interruptions
	        ("i" "Interrupts")
          ("ii" "IM" entry (file+olp+datetree ,(get-todays-journal-file-name))
           "*** IM: %^{Sender name}\t:interrupt:instantmessage:\n%a\n\n%?" :clock-in :clock-resume :empty-lines 1)
	        ("iv" "Visitor" entry (file+olp+datetree ,(get-todays-journal-file-name))
           "*** Visit from %^{Visitor name}\t:interrupt:visitor:\n%a\n\n%?" :clock-in :clock-resume :empty-lines 1)
	        ("im" "Meeting" entry (file+olp+datetree ,(get-todays-journal-file-name))
           "*** Meeting: %^{Meeting description}\t:interrupt:meeting:\n%a\n\n%?" :clock-in :clock-resume)
          ))

;;;;; Refiling
  (setq org-refile-targets '((nil . (:maxlevel . 9))
                             (org-agenda-files . (:maxlevel . 9)))
        ;; Show full "path" of refile targets
        org-refile-use-outline-path 'file
        ;; Do not complete in steps
        org-outline-path-complete-in-steps nil
        ;; Allow refile to create parent tasks with confirmation
        org-refile-allow-creating-parent-nodes 'confirm)
  ;; Exclude completed tasks from refile targets (https://michael.englehorn.com/config.html)
  (defun bh/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  (setq org-refile-target-verify-function 'bh/verify-refile-target)

  ;; org-archive
  (setq org-archive-location "%s_archive::datetree/")

;;;;; Clock
  ;; persist the last used clock accross emacs sessions
  (setq org-clock-persist t
        org-clock-history-length 35
        org-clock-in-resume t
        ;; always resolve open clocks (even if a clock is running)
        org-clock-auto-clock-resolution t
        org-agenda-start-with-clockreport-mode t
        org-clock-report-include-clocking-taskt t)
  (setq org-agenda-clockreport-parameter-plist
        '(:link t :maxlevel 4 :stepskip0 t :fileskip0 t :compact t :narrow 80))
  (org-clock-persistence-insinuate)

  ;; Useful tweaks
  (setq org-log-done 'time)
  (setq org-log-redeadline 'time)
  (setq org-log-reschedule 'time)

  ;; Prompt for note on :clocknote tagged entries
  (add-hook 'org-clock-out-hook 'my/check-for-clock-out-note)

;;;;; Time zone handling
  (add-hook 'org-mode-hook 'my/org-check-timezone-property)

;;;;; Load extra functionality
  (with-eval-after-load 'org
    (require 'org-inlinetask))
  )

;;;; org-agenda
(defun my-org/post-init-org-agenda ()
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t

        org-agenda-files `("~/org/organizer.org"
                           "~/org/areas/"
                           "~/org/areas/places/"
                           ,(concat "~/org/journal/" (format-time-string "%Y") "/"))

        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "INPROGRESS" "|" "DONE(d!)" "CANCELLED(C@)")
                            (sequence "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)"
                                      "BACKLOG(b)" "|" "COMPLETED(c)" "CANC(k@)"))
        org-stuck-projects '("/!+PLAN|+READY|+ACTIVE|+REVIEW|+WAIT" nil nil "SCHEDULED:\\|DEADLINE:")

        org-log-into-drawer t
        org-tag-alist '(("@office" . ?o)
                        ("@home" . ?h)
                        ("@errand" . ?e))
        org-tags-exclude-from-inheritance '("PROJECT")
        org-default-notes-file "~/org/organizer.org"

        org-agenda-auto-exclude-function 'leezu/org-auto-exclude-function

        org-agenda-custom-commands
              '(("r" "Review" my/agenda-review)
                ("w" "Workflow" my/agenda-workflow)
                ("R" "Week in review" agenda ""
                 ;; agenda settings
                 ((org-agenda-span 'week)
                  (org-agenda-start-on-weekday 0) ;; start on Sunday
                  (org-agenda-overriding-header "Week in Review")
                  (org-agenda-start-with-log-mode t)
                  (org-agenda-log-mode-items '(clock state))
                  (org-agenda-archives-mode t) ; include archive files
                  ))))
  )

;;;; org-ref
(defun my-org/post-init-org-ref ()
  (setq org-ref-default-bibliography '("~/Dropbox/Papers/references.bib")
        org-ref-pdf-directory "~/Dropbox/Papers/"
        org-ref-bibliography-notes "~/Dropbox/Papers/notes.org")

  (spacemacs/set-leader-keys "ob" 'helm-bibtex-with-local-bibliography)

  ;; Template for paper notes
  (setq org-ref-note-title-format
        "** %y - %t
:PROPERTIES:
:Custom_ID: %k
:INTERLEAVE_PDF: ./%k.pdf
:END:
cite:%k
")

  ;; Tell org-ref to let helm-bibtex find notes for it
  (setq org-ref-notes-function
        (lambda (thekey)
          (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
            (bibtex-completion-edit-notes
             (list (car (org-ref-get-bibtex-key-and-file thekey))))))))

;;;; helm-bibtex
(defun my-org/post-init-helm-bibtex ()
  (setq bibtex-completion-bibliography '("~/Dropbox/Papers/references.bib")
        bibtex-completion-library-path '("~/Dropbox/Papers/")
        bibtex-completion-notes-path "~/Dropbox/Papers/notes.org")

  (setq bibtex-completion-notes-template-one-file
        (concat
         "* ${title} (${year})\n"
         ":PROPERTIES:\n"
         ":Custom_ID: ${=key=}\n"
         ":INTERLEAVE_PDF: ./${=key=}.pdf\n"
         ":END:\n"
         "cite:${=key=}\n"
         "%?")  ;; %? for org-capture
        )

  ;; Overwrite bibtex-completion-edit-notes to use org-capture
  (eval-after-load "helm-bibtex"
    '(defun bibtex-completion-edit-notes (keys)
       "Open the notes associated with the selected entry or create new via org-capture."
       (require 'org-capture)
       (dolist (key keys)
         (let* ((entry (bibtex-completion-get-entry key))
                (year (or (bibtex-completion-get-value "year" entry)
                          (car (split-string (bibtex-completion-get-value "date" entry "") "-"))))
                (entry (push (cons "year" year) entry))
                (buffer (generate-new-buffer-name "bibtex-notes")))
           (with-current-buffer (make-indirect-buffer  ;; TODO: buffer is never deleted
                                 (find-file-noselect bibtex-completion-notes-path)
                                 buffer t)  ;; clone t
             (widen)
             (outline-show-all)
             (goto-char (point-min))
             (if (re-search-forward (format bibtex-completion-notes-key-pattern (regexp-quote key)) nil t)
                                        ; Existing entry found:
                 (when (eq major-mode 'org-mode)
                   (org-narrow-to-subtree)
                   (goto-char (point-min))
                   (outline-show-all)
                   (switch-to-buffer-other-window buffer))
                                        ; Create a new entry:
               (let ((org-capture-templates
                      '(("bibtex" "helm-bibtex"
                         entry
                         (file+olp+datetree bibtex-completion-notes-path)
                         "%(s-format bibtex-completion-notes-template-one-file 'bibtex-completion-apa-get-value entry)"))))
                 (org-capture nil "bibtex")))))))))

;;;; org-pomodoro
(defun my-org/post-init-org-pomodoro ()
  (setq alert-default-style 'libnotify)
  )

;;; Owned packages

;;;; interleave
(defun my-org/init-interleave ()
  (use-package interleave)
  )

;;;; helm-org-rifle
(defun my-org/init-helm-org-rifle ()
  (use-package helm-org-rifle)
  (setq helm-org-rifle-show-path t)

  (spacemacs/set-leader-keys "aro" 'helm-org-rifle)
  (spacemacs/set-leader-keys "arb" 'helm-org-rifle-current-buffer)
  (spacemacs/set-leader-keys "ard" 'helm-org-rifle-directories)
  (spacemacs/set-leader-keys "arf" 'helm-org-rifle-files)
  (spacemacs/set-leader-keys "arr" 'helm-org-rifle-org-directory))

;;;; org-super-agenda
(defun my-org/init-org-super-agenda ()
  (use-package org-super-agenda
    :config (org-super-agenda-mode))

  (setq org-super-agenda-groups
        '(;; Each group has an implicit boolean OR operator between its selectors.
          (:name "Today" :time-grid t)
          (:name "In progress" :todo "INPROGRESS")
          (:name "Next" :todo "NEXT")
          (:name "Important" :and (:priority "A" :todo ("TODO" "NEXT" "INPROGRESS")))
          (:name "Projects"  :todo ("PLAN" "READY" "ACTIVE" "REVIEW"))
          (:name "Waiting Projects"  :todo "WAIT")
          (:name "Backlog Projects"  :todo "BACKLOG")
          (:and (:priority<= "B" :todo ("TODO" "NEXT" "INPROGRESS")))
          (:todo ("TODO" "NEXT" "INPROGRESS"))
          (:discard (:anything t))
          )))

;;;; git-auto-commit-mode
(defun my-org/init-git-auto-commit-mode ()
  (use-package git-auto-commit-mode
    :init (setq gac-automatically-push-p 1)))

;;;; outshine
(defun my-org/init-outshine ()
  (setq outshine-startup-folded-p t)

  (defun advise-outshine-narrow-start-pos ()
    (unless (outline-on-heading-p t)
      (outline-previous-visible-heading 1)))

  (use-package outshine
    :init
    (progn
      (spacemacs/set-leader-keys
        "nn" 'outshine-narrow-to-subtree
        "nw" 'widen)
      (define-keys outline-minor-mode-map
        (kbd "M-RET") 'outshine-insert-heading
        (kbd "<backtab>") 'outshine-cycle-buffer))

    :config
    (progn
      ;; Narrowing works within the headline rather than requiring to be on it
      (advice-add 'outshine-narrow-to-subtree :before
                  'advise-outshine-narrow-start-pos)

      (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
      (add-hook 'prog-mode-hook 'outline-minor-mode))))

;;;; helm-navi
(defun my-org/init-helm-navi ()
  (use-package helm-navi
    :init
    (progn
      (spacemacs/set-leader-keys
        "sj" 'helm-navi))))
