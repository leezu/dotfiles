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
    org-pomodoro
    ;; Packages already in bibtex layer
    org-ref
    helm-bibtex
    ;; Extra packages
    org-pdfview
    interleave
    org-alert
    ))

;; Configuration of packages already present in other layers
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

(defun my-org/post-init-org-agenda ()
  (setq org-agenda-files '("~/org/refile.org"
                           "~/org/organizer.org"
                           "~/org/diary.org"
                           "~/org/journal.org"
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
        '(("t" "Todo" entry (file+headline "~/org/refile.org" "Tasks")
           "* TODO %?\nOPENED: %U\n %i")
          ("w" "Waiting for" entry (file+headline "~/org/refile.org" "Tasks")
           "* WAITING %?\nOPENED: %U\n %i")
          ("s" "Someday" entry (file+headline "~/org/refile.org" "Tasks")
           "* SOMEDAY %?\nOPENED: %U\n %i")
          ("c" "Code Snippet" entry (file "~/org/snippets.org")
           ;; Prompt for tag and language
           "* %?\t:%^{language}:\n#+BEGIN_SRC %\\1\n%i\n#+END_SRC")
          ("n" "Note" entry (file+headline "~/org/refile.org" "Notes")
           "* %?\nCREATED: %U\n %i")
          ("d" "Diary" entry (file+olp+datetree "~/org/diary.org")
           "* %?\nEntered on %U\n")
          ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n")))

  ;; GTD Projects ( http://sachachua.com/blog/2008/01/projects-in-emacs-org/ )
  (setq org-agenda-custom-commands
        '(
          ;; The following ressources were helpful when creating the custom
          ;; agenda commands
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
          ;; Block agenda
          (" " "Agenda"
           ((agenda ""
                    ((org-agenda-span 'day)))
            ;; All items with the "REFILE" tag, everything in refile.org
            ;; automatically gets that applied
            (tags "REFILE"
                  ((org-agenda-overriding-header "Tasks to Refile")))
            ;; All "INPROGRESS" todo items
            (todo "INPROGRESS"
                  ((org-agenda-overriding-header "Current work")))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Started tasks and tasks ready to be done next")))
            (tags "WAITING"
                  ((org-agenda-overriding-header "Waiting for something")))
            ;; All TODO items
            (todo "TODO"
                  ((org-agenda-overriding-header "Task list")
                   ;; sort by time, priority, and category
                   (org-agenda-sorting-strategy
                    '(time-up priority-down category-keep))))
            ;; Everything on hold
            (todo "HOLD"
                  ((org-agenda-overriding-header "On-hold")))
            ;; All headings with the "recurring" tag
            (tags "recurring/!"
                  ((org-agenda-overriding-header "Recurring")))
            ))
          ))

  (setq org-stuck-projects
        '("+PROJECT/-MAYBE-DONE" ("NEXT" "STARTED") nil "\\<IGNORE\\>"))

  ;; Auto-exclude
  (defun leezu/weekday-p ()
    (let ((wday (nth 6 (decode-time))))
      (and (< wday 6)
           (> wday 0))))

  (defun leezu/working-p ()
    (let ((hour (nth 2 (decode-time))))
      (and (leezu/weekday-p)
           (or (and (>= hour 10) (<= hour 12))
               (and (>= hour 12) (<= hour 19))))))

  (defun leezu/org-auto-exclude-function (tag)
    (and (cond
          ((string= tag "@home")
           (leezu/working-p))
          ((string= tag "@office")
           (not (leezu/working-p)))
          ((or (string= tag "@errand") (string= tag "PHONE"))
           (let ((hour (nth 2 (decode-time))))
             (or (< hour 8) (> hour 21)))))
         (concat "-" tag)))

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
