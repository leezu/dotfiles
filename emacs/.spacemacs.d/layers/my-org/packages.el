;;; packages.el --- my-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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
    ;; Packages already in bibtex layer
    org-ref
    helm-bibtex
    ;; Extra packages
    org-pdfview
    (org-habit :location built-in)
    interleave
    ))

(defun my-org/post-init-org ()
  ;; Disable indentation in org mode
  (setq org-adapt-indentation nil)
  ;; Auto-fill mode for org-mode
  (add-hook 'org-mode-hook
            (lambda ()
              ;; Never enable fill column indicator! It makes org-capture very slow.
              ;; (fci-mode t)
              ;; Turn off line numbering, it makes org so slow
              (linum-mode -1)
              ;; Set fill column to 79
              ;; (setq fill-column 79)
              ;; Enable automatic line wrapping at fill column
              (auto-fill-mode t)))
  ;; org-refile
  (setq org-refile-targets '((nil . (:maxlevel . 9))
                             (org-agenda-files . (:maxlevel . 9)))
        ;; Show full "path" of refile targets
        org-refile-use-outline-path t
        ;; Do not complete in steps
        org-outline-path-complete-in-steps nil
        ;; Allow refile to create parent tasks with confirmation
        org-refile-allow-creating-parent-nodes '(confirm))

  ;; Useful tweaks
  (setq org-log-done (quote time))
  (setq org-log-redeadline (quote time))
  (setq org-log-reschedule (quote time))

  ;; face settings
  ;; make org level all same size
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
   '(org-level-6 ((t (:inherit outline-6 :height 1.0))))
   '(org-level-7 ((t (:inherit outline-7 :height 1.0))))
   '(org-level-8 ((t (:inherit outline-8 :height 1.0))))
   )
  ;; make priority show different colors
  (setq org-priority-faces
        '((?A . (:foreground "red" :weight 'bold))
          (?B . (:foreground "yellow"))
          (?C . (:foreground "green"))))
  ;; make agend today's font the same as others
  (setq spacemacs-theme-org-agenda-height nil)
  ;; make org-agenda done tasks having the same font scale
  (custom-set-faces
   '(org-agenda-done ((t (:foreground "#86dc2f" :height 1.0)))))
  )

(defun my-org/init-org-habit ()
  ;; tracking habits
  (use-package org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-preceding-days 10
        org-habit-following-days 1
        org-habit-graph-column 40
        org-habit-show-done-always-green t)
  )

(defun my-org/post-init-org-agenda ()
  (setq org-agenda-files '("~/Dropbox/org/organizer.org"
                           "~/Dropbox/org/deft/"
                           "~/Dropbox/Papers/notes.org"
                           "~/projects/leezu.github.io/posts")
        org-todo-keywords '((sequence "SOMEDAY" "TODO" "NEXT" "STARTED" "|" "DONE" "CANCELLED")
                            (sequence "WAITING" "|" "DONE" "CANCELLED"))
        org-log-into-drawer t
        org-tag-alist '(("@research" . ?r)
                        ("@work" . ?w)
                        ("@home" . ?h))
        org-tags-exclude-from-inheritance '("PROJECT")
        org-default-notes-file "~/Dropbox/org/organizer.org"
        )

  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Dropbox/org/organizer.org" "Tasks")
           "* TODO %?\nOPENED: %U\n %i")
          ("w" "Waiting for" entry (file+headline "~/Dropbox/org/organizer.org" "Tasks")
           "* WAITING %?\nOPENED: %U\n %i")
          ("s" "Someday" entry (file+headline "~/Dropbox/org/organizer.org" "Tasks")
           "* SOMEDAY %?\nOPENED: %U\n %i")
          ("n" "Note" entry (file+headline "~/Dropbox/org/organizer.org" "Notes")
           "* %?\nCREATED: %U\n %i")
          ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
           "* %?\nEntered on %U\n")))

  ;; GTD Projects ( http://sachachua.com/blog/2008/01/projects-in-emacs-org/ )
  (setq org-agenda-custom-commands
        '(
          ;; The following ressources were helpful when creating the custom agenda commands
          ;; https://www.reddit.com/r/emacs/comments/2b9obs/org_users_what_did_it_take_you_a_long_time_to/

          ;; Weekly review
          ("r" "GTD review"
           ((tags "REFILE"
                  ((org-agenda-overriding-header "Tasks to Refile")))
            (tags-todo "-PROJECTS"
                       ((org-agenda-overriding-header "Assign to PROJECT")))
            (tags "PROJECT-MAYBE-DONE"
                  ((org-agenda-overriding-header "Projects")))
            (tags "PROJECT&MAYBE"
                  ((org-agenda-overriding-header "Maybe projects")))
            (stuck "" ())
            ;; (tags-todo "-SCHEDULED={.+}-DEADLINE={.+}"
            ;;            ((org-agenda-overriding-header "Unscheduled tasks")))
            ;; (todo "SOMEDAY"
            ;;       ((org-agenda-overriding-header "Someday list")))
            (todo "WAITING"
                  ((org-agenda-overriding-header "Waiting list")))
            (todo ""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
                   (org-agenda-prefix-format '((todo . " %i %-22(org-entry-get nil \"DEADLINE\") %-12:c %s")))
                   (org-agenda-sorting-strategy '(deadline-up))
                   (org-agenda-overriding-header "Upcoming deadlines")))))

          ;; Habits
          ("h" "Habits"
           ((tags-todo "STYLE=\"habit\"+SCHEDULED<=\"<today>\"")
            (org-agenda-overriding-header "Habits")
            (org-agenda-sorting-strategy
             '(priority-down time-down todo-state-down
                             effort-up category-keep))))
          ;; Block agenda
          ("c" "Simple agenda view"
           ((agenda ""
                    ((org-agenda-span 'day)))
            (todo "NEXT|STARTED"
                  ((org-agenda-overriding-header "Started tasks and tasks ready to be done next")))
            (todo ""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
                   (org-agenda-prefix-format '((todo . " %i %-22(org-entry-get nil \"DEADLINE\") %-12:c %s")))
                   (org-agenda-sorting-strategy '(deadline-up))
                   (org-agenda-overriding-header "Upcoming deadlines")))))
          ))

  (setq org-stuck-projects
        '("+PROJECT/-MAYBE-DONE" ("NEXT" "STARTED") nil "\\<IGNORE\\>"))
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

(defun my-org/init-org-pdfview ()
  (use-package org-pdfview)

  ;; Open pdfs with pdf-tools (when called from org-ref)
  (delete '("\\.pdf\\'" . default)
          org-file-apps)
  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . (lambda (file link)
                                 (org-pdfview-open file))))
  (add-to-list 'org-file-apps
               '("\\.pdf::\\([[:digit:]]+\\)\\'" . (lambda (file link)
                                                     (org-pdfview-open file))))
  )

(defun my-org/init-interleave ()
  (use-package interleave)
  )

;;; packages.el ends here
