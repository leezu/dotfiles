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
  (setq org-adapt-indentation nil)  ;; Disable indentation in org mode
  (setq org-catch-invisible "error")  ;; Cancel invisible edits

  ;; Agenda
  (setq org-enforce-todo-dependencies t
        org-agenda-dim-blocked-tasks 'invisible)

  ;; org-refile
  (setq org-refile-targets '((nil . (:maxlevel . 9))
                             (org-agenda-files . (:maxlevel . 9)))
        ;; Show full "path" of refile targets
        org-refile-use-outline-path 'file
        ;; Do not complete in steps
        org-outline-path-complete-in-steps nil
        ;; Allow refile to create parent tasks with confirmation
        org-refile-allow-creating-parent-nodes 'confirm
        ;; Cache refile targets
        org-refile-use-cache t)
  ;; Periodically invalidate org-refile cache
  (run-with-idle-timer 300 t (lambda ()
                               (org-refile-cache-clear)
                               (org-refile-get-targets)))
  ;; Exclude completed tasks from refile targets (https://michael.englehorn.com/config.html)
  (defun bh/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  (setq org-refile-target-verify-function 'bh/verify-refile-target)

  ;; org-archive
  (setq org-archive-location "%s_archive::datetree/")

  ;; org-clock
  ;; persist the last used clock accross emacs sessions
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  ;; Useful tweaks
  (setq org-log-done 'time)
  (setq org-log-redeadline 'time)
  (setq org-log-reschedule 'time)
  )

(defun my-org/post-init-org-agenda ()
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)

  (setq org-agenda-files '("~/org/refile.org"
                           "~/org/organizer.org"
                           "~/org/areas/"
                           "~/org/journal/2018/")
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "INPROGRESS" "|" "DONE(d!)")
                            (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "|" "COMPLETED(c)" "CANC(k@)"))
        org-log-into-drawer t
        org-tag-alist '(("@office" . ?o)
                        ("@home" . ?h)
                        ("@errand" . ?e))
        org-tags-exclude-from-inheritance '("PROJECT")
        org-default-notes-file "~/org/organizer.org"
        )

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
          ("n" "Note" entry (file+headline "~/org/organizer.org" "Notes")
           "* %?\nCREATED: %U\n%a\n\n%i")

          ;; snippets
          ("c" "Code Snippet" entry (file "~/org/snippets.org")
           ;; Prompt for tag and language
           "* %?\t:%^{language}:\n#+BEGIN_SRC %\\1\n%i\n#+END_SRC")

          ;; journal
          ("j" "Journal" entry (file+olp+datetree ,(get-todays-journal-file-name) "Journal")
           "* %?\nEntered on %U\n%a\n" :clock-in t :clock-resume t)

          ;; interruptions
	        ("i" "Interrupts")
          ("ii" "IM" entry (file+olp+datetree ,(get-todays-journal-file-name))
           "*** IM: %^{Sender name}\t:interrupt:instantmessage:\n%a\n\n%?" :clock-in :clock-resume :empty-lines 1)
	        ("iv" "Visitor" entry (file+datetree (get-todays-journal-file-name))
           "*** Visit from %^{Visitor name}\t:interrupt:visitor:\n%a\n\n%?" :clock-in :clock-resume :empty-lines 1)
	        ("im" "Meeting" entry (file+datetree (get-todays-journal-file-name))
           "*** Meeting: %^{Meeting description}\t:interrupt:meeting:\n%a\n\n%?" :clock-in :clock-resume)
          ))

  (setq org-agenda-custom-commands
        '(
          ;; Filtered agendas
          ("w" "Work"
           ((agenda "" ((org-agenda-tag-filter-preset '("+@office"))))))
          ("h" "Home"
           ((agenda "" ((org-agenda-tag-filter-preset '("+@home"))))))
          ("e" "Errand"
           ((agenda "" ((org-agenda-tag-filter-preset '("+@errand"))))))
          ;; Review
          ("rr" "Review"
           ((agenda "" ((org-super-agenda-groups
                         (butlast org-super-agenda-groups 1))))))
          ("rw" "Work"
           ((agenda "" ((org-agenda-tag-filter-preset '("+@office"))
                        (org-super-agenda-groups
                         (butlast org-super-agenda-groups 1))))))
          ("rw" "Home"
           ((agenda "" ((org-agenda-tag-filter-preset '("+@home"))
                        (org-super-agenda-groups
                         (butlast org-super-agenda-groups 1))))))
          ("rw" "Errand"
           ((agenda "" ((org-agenda-tag-filter-preset '("+@errand"))
                        (org-super-agenda-groups
                         (butlast org-super-agenda-groups 1))))))
          ("rg" "GTD review"
           ((tags-todo "-PROJECTS"
                       ((org-agenda-overriding-header "Assign to PROJECT")))
            (tags "PROJECT-MAYBE-DONE"
                  ((org-agenda-overriding-header "Projects")))
            (tags "PROJECT&MAYBE"
                  ((org-agenda-overriding-header "Maybe projects")))
            (stuck "" ())
            (todo "WAITING"
                  ((org-agenda-overriding-header "Waiting list")))
            ))
          ))

  (setq org-stuck-projects
        '("+LEVEL=2/!+PLAN|+READY|+ACTIVE|+REVIEW" ("NEXT" "INPROGRESS") nil ""))

  ;; Auto-exclude
  ;; See funcs.el
  (setq org-agenda-auto-exclude-function 'leezu/org-auto-exclude-function)
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
          (:name "Today"
                 :time-grid t
                 :order 1)
          (:name "In progress"
                 :todo "INPROGRESS"
                 :order 2)
          (:name "Next"
                 :todo "NEXT"
                 :order 3)
          (:name "Unscheduled"  ;; These are only displayed in the all TODOs agenda
                 :and (:todo t
                             :deadline nil
                             :scheduled nil)
                 :order 4)
          (:name "Important"
                 :priority "A"
                 :order 5)
          (:priority<= "B"
                       :order 6)
          (:name "Refile"
                 :tag "REFILE"
                 :order 7)
          (:name "Waiting"
                 :tag "WAITING"
                 :order 8)
          (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                 :order 9)
          (:discard (:anything t))  ;; discard must be the last entry due to butlast above
          )))

(defun my-org/init-git-auto-commit-mode ()
  (use-package git-auto-commit-mode
    :init (setq gac-automatically-push-p 1)))
;;; packages.el ends here
