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
    org-super-agenda
    git-auto-commit-mode
    ))

;; Configuration of packages already present in other layers
(defun my-org/post-init-org ()
  ;; Disable indentation in org mode
  (setq org-adapt-indentation nil)

  ;; org-refile
  (setq org-refile-targets '((nil . (:maxlevel . 9))
                             (org-agenda-files . (:maxlevel . 9)))
        ;; Show full "path" of refile targets
        org-refile-use-outline-path t
        ;; Do not complete in steps
        org-outline-path-complete-in-steps nil
        ;; Allow refile to create parent tasks with confirmation
        org-refile-allow-creating-parent-nodes 'confirm)

  ;; Useful tweaks
  (setq org-log-done (quote time))
  (setq org-log-redeadline (quote time))
  (setq org-log-reschedule (quote time))
  )

(defun my-org/post-init-org-agenda ()
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)

  (setq org-agenda-files '("~/org/refile.org"
                           "~/org/organizer.org"
                           "~/org/diary.org.gpg"
                           "~/org/journal.org.gpg"
                           "~/org/work-journal.org.gpg"
                           "~/org/deft/"
                           "~/Dropbox/Papers/notes.org"
                           "~/projects/leezu.github.io/posts")
        org-todo-keywords '((sequence "SOMEDAY" "TODO" "NEXT" "INPROGRESS" "|" "DONE" "CANCELLED")
                            (sequence "WAITING" "|" "DONE" "CANCELLED")
                            (sequence "HOLD" "|" "DONE" "CANCELLED"))
        org-log-into-drawer t
        org-tag-alist '(("@office" . ?o)
                        ("@home" . ?h)
                        ("@errand" . ?e))
        org-tags-exclude-from-inheritance '("PROJECT")
        org-default-notes-file "~/org/refile.org"
        )

  (setq org-capture-templates
        '(("t" "Task templates")
          ("tt" "Office task" entry (file+headline "~/org/refile.org" "Tasks")
           "* TODO [#A]%?\t:@office:\nOPENED: %U\n%a\n\n%i")
          ("th" "Home task" entry (file+headline "~/org/refile.org" "Tasks")
           "* TODO [#A]%?\t:@home:\nOPENED: %U\n%a\n\n%i")
          ("te" "Errand task" entry (file+headline "~/org/refile.org" "Tasks")
           "* TODO [#A]%?\t:@errand:\nOPENED: %U\n%a\n\n%i")
          ("w" "Waiting for" entry (file+headline "~/org/refile.org" "Tasks")
           "* WAITING %?\nOPENED: %U\n%a\n\n%i")
          ("s" "Someday" entry (file+headline "~/org/refile.org" "Tasks")
           "* SOMEDAY %?\nOPENED: %U\n%a\n\n%i")
          ("c" "Code Snippet" entry (file "~/org/snippets.org")
           ;; Prompt for tag and language
           "* %?\t:%^{language}:\n#+BEGIN_SRC %\\1\n%i\n#+END_SRC")
          ("n" "Note" entry (file+headline "~/org/refile.org" "Notes")
           "* %?\nCREATED: %U\n%a\n\n%i")
          ("d" "Diary" entry (file+olp+datetree "~/org/diary.org.gpg")
           "* %?\nEntered on %U\n%a\n")
          ("j" "Journal" entry (file+olp+datetree "~/org/journal.org.gpg")
           "* %?\nEntered on %U\n%a\n")))

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
        '("+PROJECT/-MAYBE-DONE" ("NEXT" "STARTED") nil "\\<IGNORE\\>"))

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

(defun my-org/init-org-super-agenda ()
  (use-package org-super-agenda
    :config (org-super-agenda-mode))

  (setq org-super-agenda-groups
        '(;; Each group has an implicit boolean OR operator between its selectors.
          (:name "Today"
                 :time-grid t
                 :todo "TODAY"
                 :order 1)
          (:name "Important"
                 :tag "bills"
                 :priority "A"
                 :order 2)
          (:priority<= "B"
                       :order 3)
          (:name "In progress"
                 :tag "INPROGRESS"
                 :order 4)
          (:name "Next"
                 :tag "NEXT"
                 :order 4)
          (:name "Refile"
                 :tag "REFILE"
                 :order 5)
          (:name "Waiting"
                 :tag "WAITING"
                 :order 6)
          (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                 :order 9)
          (:discard (:anything t))  ;; discard must be the last entry due to butlast above
          )))

(defun my-org/init-git-auto-commit-mode ()
  (use-package git-auto-commit-mode
    :init (setq gac-automatically-push-p 1)))
;;; packages.el ends here
