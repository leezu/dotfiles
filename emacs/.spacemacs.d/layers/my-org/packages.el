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

(defconst my-org-packages
  '(
    ;; Packages already in org layer
    org
    org-agenda
    org-pomodoro
    ;; Packages already in bibtex layer
    org-ref
    helm-bibtex
    ;; Extra packages
    interleave
    org-alert
    helm-org-rifle
    org-super-agenda
    git-auto-commit-mode
    ))

;; Configuration of packages already present in other layers
(defun my-org/post-init-org ()
  (setq org-adapt-indentation nil  ;; Disable indentation in org mode
        org-catch-invisible "error"  ;; Cancel invisible edits
        org-enforce-todo-dependencies t
        org-agenda-window-setup 'current-window)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Capture
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq org-capture-templates
        ;; note the backquote ` instead of normal quote '
        `(("P" "New project" entry (file+olp "~/org/organizer.org" "Projects")
           "* PLAN %?\nOPENED: %U\n%a\n\n%i")
          ("b" "New project (backlog)" entry (file+olp "~/org/organizer.org" "Projects")
           "* BACKLOG %?\nOPENED: %U\n%a\n\n%i")
          ("t" "Task" entry (file+olp "~/org/organizer.org" "Tasks")
           "* TODO [#A]%?\nOPENED: %U\n%a\n\n%i" :clock-in t :clock-resume t)
          ("s" "Clocked entry subtask" entry (clock)
           "* TODO [#A]%?\nOPENED: %U\n%a\n\n%i" :clock-in t :clock-resume t :empty-lines 1)
          ("c" "Clocked entry checkbox" checkitem (clock) "[ ] %i")
          ("w" "Waiting for" entry (file+olp "~/org/organizer.org" "Tasks")
           "* WAITING %?\nOPENED: %U\n%a\n\n%i")
          ("s" "Someday" entry (file+olp "~/org/organizer.org" "Someday")
           "* SOMEDAY %?\nOPENED: %U\n%a\n\n%i")

          ;; notes
          ("n" "Note" entry (file+olp "~/org/organizer.org" "Notes")
           "* %?\nCREATED: %U\n%a\n\n%i")

          ;; snippets
          ("c" "Code Snippet" entry (file "~/org/snippets.org")
           ;; Prompt for tag and language
           "* %?\t:%^{language}:\n#+BEGIN_SRC %\\1\n%i\n#+END_SRC")

          ;; journal
          ("j" "Journal" entry (file+olp+datetree ,(get-todays-journal-file-name))
           "* %?\nEntered on %U\n%a\n" :clock-in t :clock-resume t)

          ;; interruptions
	        ("i" "Interrupts")
          ("ii" "IM" entry (file+olp+datetree ,(get-todays-journal-file-name))
           "*** IM: %^{Sender name}\t:interrupt:instantmessage:\n%a\n\n%?" :clock-in :clock-resume :empty-lines 1)
	        ("iv" "Visitor" entry (file+olp+datetree ,(get-todays-journal-file-name))
           "*** Visit from %^{Visitor name}\t:interrupt:visitor:\n%a\n\n%?" :clock-in :clock-resume :empty-lines 1)
	        ("im" "Meeting" entry (file+olp+datetree ,(get-todays-journal-file-name))
           "*** Meeting: %^{Meeting description}\t:interrupt:meeting:\n%a\n\n%?" :clock-in :clock-resume)
          ))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Refiling
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Clock
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

  ;; Load extra functionality
  (with-eval-after-load 'org
    (require 'org-inlinetask))
  )

(defun my-org/post-init-org-agenda ()
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t

        org-agenda-files '("~/org/organizer.org"
                           "~/org/areas/"
                           "~/org/journal/2018/")

        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "INPROGRESS" "|" "DONE(d!)" "CANCELLED(C@)")
                            (sequence "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)"
                                      "BACKLOG(b)" "|" "COMPLETED(c)" "CANC(k@)"))
        org-stuck-projects '("/!+PLAN|+READY|+ACTIVE|+REVIEW" ("NEXT" "INPROGRESS") nil "")

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


(defun my-org/post-init-org-ref ()
  (setq org-ref-default-bibliography '("~/Dropbox/Papers/references.bib")
        org-ref-pdf-directory "~/Dropbox/Papers/"
        org-ref-bibliography-notes "~/Dropbox/Papers/notes.org")

  (spacemacs/set-leader-keys "ob" 'helm-bibtex)

  ;; Template for paper notes
  (setq org-ref-note-title-format
        "** %y - %t
:PROPERTIES:
:Custom_ID: %k
:INTERLEAVE_PDF: ./%k.pdf
:END:
cite:%k
")
  )

(defun my-org/post-init-helm-bibtex ()
  (setq bibtex-completion-bibliography '("~/Dropbox/Papers/references.bib")
        bibtex-completion-library-path '("~/Dropbox/Papers/")
        bibtex-completion-notes-path "~/Dropbox/Papers/notes.org")

  (setq bibtex-completion-notes-template-one-file
        (concat
         "** ${year} - ${title}\n"
         ":PROPERTIES:\n"
         ":Custom_ID: ${=key=}\n"
         ":INTERLEAVE_PDF: ./${=key=}.pdf\n"
         ":END:\n"
         "cite:${=key=}")
        )
  )

(defun my-org/post-init-org-pomodoro ()
  (setq alert-default-style 'libnotify)
  )

;; Configuration of newly introduced packages

(defun my-org/init-interleave ()
  (use-package interleave)
  )

(defun my-org/init-helm-org-rifle ()
  (use-package helm-org-rifle)
  (setq helm-org-rifle-show-path t)

  (spacemacs/set-leader-keys "aro" 'helm-org-rifle)
  (spacemacs/set-leader-keys "arb" 'helm-org-rifle-current-buffer)
  (spacemacs/set-leader-keys "ard" 'helm-org-rifle-directories)
  (spacemacs/set-leader-keys "arf" 'helm-org-rifle-files)
  (spacemacs/set-leader-keys "arr" 'helm-org-rifle-org-directory))

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

(defun my-org/init-git-auto-commit-mode ()
  (use-package git-auto-commit-mode
    :init (setq gac-automatically-push-p 1)))
;;; packages.el ends here
