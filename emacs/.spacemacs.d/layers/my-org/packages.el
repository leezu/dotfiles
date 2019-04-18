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
    evil-collection
    ebib
    outshine
    helm-navi
    org-id
    interleave
    org-alert
    org-super-agenda
    (git-auto-commit-mode :fetcher github
                          :repo "leezu/git-auto-commit-mode"
                          :branch "leezu")
    ))

;;; Packages owned by other layers
;;;; org
(defun my-org/post-init-org ()
  (setq org-adapt-indentation nil  ;; Disable indentation in org mode
        org-catch-invisible "error"  ;; Cancel invisible edits
        org-enforce-todo-dependencies t
        org-agenda-window-setup 'current-window
        org-agenda-inhibit-startup t
        org-agenda-use-tag-inheritance nil
        org-preview-latex-default-process 'dvisvgm
        org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

;;;;; org-id
  (require 'org-id)
  (defun org-id-goto-narrow-indirect-buffer (id)
    "Switch to the buffer containing the entry with id ID.
Move the cursor to that entry in that buffer."
    (let ((m (org-id-find id 'marker)))
      (unless m
        (error "Cannot find entry with ID \"%s\""
               id))
      ;; TODO: buffer is never deleted
      (with-current-buffer (marker-buffer m)
        (widen)
        (outline-show-all)
        (goto-char m)
        (move-marker m nil)
        (let* ((heading (org-get-heading 'no-tags))
               (ibuf (org-get-indirect-buffer (current-buffer) heading)))
          (switch-to-buffer-other-window ibuf)
          (goto-char (org-find-entry-with-id id))  ;; This shouldn't be
                                                   ;; necessary but is..
          (org-narrow-to-subtree)))))
  ;; This makes sure that each captured entry gets a unique ID
  (add-hook 'org-capture-prepare-finalize-hook
            'org-id-get-create)
  (setq org-id-link-to-org-use-id 'create-if-interactive
        org-id-extra-files
        '("~/Papers/notes.org"))
  (defun my/zettel-id-targets (&optional overview)
    (sort (directory-files "/home/leonard/org/zettels"
                           t (if overview "O.*org" ".*org") t)
          'string>))
  (defun my/org-id-complete-link (&optional arg)
    "Create an id: link using completion"
    (concat "id:"
            (org-id-get-with-outline-path-completion '((my/zettel-id-targets . (:maxlevel . 1))))))
  (org-link-set-parameters "id" :complete 'my/org-id-complete-link)
;;;;; Zettelkasten
  (require 'helm-ag)
  (defun my/helm-zettelkasten-ag ()
    (interactive)
    (helm-ag--init-state)
    (let ((helm-ag--last-query "^\\* ")
          (helm-ag--default-directory "/home/leonard/org/zettels")
          (helm-ag--default-target "/home/leonard/org/zettels")
          )
      (helm-attrset 'search-this-file nil helm-ag-source)
      (helm-attrset 'name (helm-ag--helm-header helm-ag--default-directory) helm-ag-source)
      (helm :sources '(helm-ag-source) :buffer "*helm-ag*" :keymap helm-ag-map
            :history 'helm-ag--helm-history)))
  (spacemacs/set-leader-keys
    "oz" 'my/helm-zettelkasten-ag)

;;;;; Capture
  (defun new-zettel-file (&optional overview)
    (concat "/home/leonard/org/zettels/"
            (if overview "O-")
            (format-time-string "%Y%m%d%H%M%S")
            ".org"))
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
          ("S" "Clocked entry checkbox" checkitem (clock) "[ ] %i")
          ("w" "Waiting for" entry (file+olp "~/org/organizer.org" "Tasks")
           "* WAITING %?\nOPENED: %U\n%a\n\n%i")
          ("s" "Someday" entry (file+olp "~/org/organizer.org" "Someday")
           "* SOMEDAY %?\nOPENED: %U\n%a\n\n%i")

          ;; zettelkasten
          ("z" "Zettel"
           entry
           (file new-zettel-file)
           "* %?\nOPENED: %U\n\n%i\n")
          ("o" "Overview Zettel"
           entry
           (file (lambda () (new-zettel-file t)))
           "* %?\nOPENED: %U\n\n%i\n")

          ;; notes
          ("n" "Note (organizer.org)" entry (file+olp+datetree "~/org/organizer.org")
           "* %?\nCREATED: %U\n%a\n\n%i" :prepend t)
          ("l" "Note (current file)" entry (file+olp+datetree buffer-file-name)
           "* %?\nCREATED: %U\n%a\n\n%i" :prepend t)
          ("c" "Clocked entry note" entry (clock)
           "* %U %?\n%a\n\n%i" :empty-lines 1)

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
        org-agenda-sticky t

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
  (setq org-ref-default-bibliography '("~/Papers/references.bib")
        org-ref-pdf-directory "~/Papers/"
        org-ref-bibliography-notes "~/Papers/notes.org")

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
  (setq bibtex-completion-bibliography '("~/Papers/references.bib")
        bibtex-completion-library-path '("~/Papers/")
        bibtex-completion-notes-path "~/org/zettels"
        bibtex-completion-cite-default-command "autocite"
        bibtex-completion-cite-prompt-for-optional-arguments nil)

  (spacemacs/set-leader-keys "ob" 'helm-bibtex-with-local-bibliography)

  (setq bibtex-completion-notes-template-one-file
        (concat
         "* ${title} (${year})\n"
         ":PROPERTIES:\n"
         ":Custom_ID: ${=key=}\n"
         ":END:\n"
         "cite:${=key=}\n"
         "%?")
        bibtex-completion-notes-template-multiple-files
        (concat
         "* %?${title} (${year})\n"
         ":PROPERTIES:\n"
         ":Custom_ID: ${=key=}\n"
         ":ID: ${ids}\n" ;; TODO breaks if bibtex entry has multiple aliases besides UUID
         ":END:\n"
         "cite:${=key=}\n")
        )

  ;; Overwrite bibtex-completion-edit-notes to use org-capture
  (with-eval-after-load "helm-bibtex"
    (progn
      (require 'org-capture)
      (require 'org-id)
      (org-id-locations-load) ; TODO necessary to avoid org-id-locations being nil
      (defun bibtex-completion-edit-notes (keys)
        "Open the notes associated with the selected entry or create new via org-capture."
        (dolist (key keys)
          (let* ((entry (bibtex-completion-get-entry key))
                 (id (bibtex-completion-get-value "IDS" entry nil))
                 (year (or (bibtex-completion-get-value "year" entry)
                           (car (split-string (bibtex-completion-get-value "date" entry
                                                                           "")
                                              "-"))))
                 (entry (push (cons "year" year)
                              entry))
                 (buffer (generate-new-buffer-name "bibtex-notes")))
            (if (and bibtex-completion-notes-path
                     (f-directory? bibtex-completion-notes-path))
                (let* ((current-new-zettel-file (new-zettel-file))
                       (org-capture-templates '(("bibtex" "helm-bibtex"
                                                 entry
                                                 ;; Zettelkasten format
                                                 (file current-new-zettel-file)
                                                 "%(s-format bibtex-completion-notes-template-multiple-files 'bibtex-completion-apa-get-value entry)"
                                                 :unnarrowed t))))
                                        ; One notes file per publication based on org-id:
                  (if (and id
                           (hash-table-p org-id-locations)
                           (gethash id org-id-locations))
                                        ; id is not nil and exists
                      (with-helm-current-buffer (org-id-goto-narrow-indirect-buffer id))
                    (if id
                                        ; id is not nil, but no entry exists
                        (progn
                          (org-capture nil "bibtex")
                          (org-id-add-location id current-new-zettel-file))
                                        ; id is nil
                      (let ((id (org-id-new))
                            (cbuf (current-buffer)))
                        ;; associate id with bibtex entry
                        (save-excursion
                          (with-temp-buffer
                            (bibtex-completion-show-entry (list key))
                            (bibtex-make-field '("IDS" "org-id"
                                                 (lambda ()
                                                   id))
                                               t)
                            (save-buffer)))
                        (switch-to-buffer cbuf)
                        (let ((entry (bibtex-completion-get-entry key)))
                          ;; reload entry due to id addition
                          (org-capture nil "bibtex")
                          (org-id-add-location id current-new-zettel-file))))))
                                        ; One file for all notes:
              (with-current-buffer (make-indirect-buffer ;; TODO: buffer is never deleted (find-file-noselect bibtex-completion-notes-path)
                                    buffer t) ;; clone t
                (widen)
                (outline-show-all)
                (goto-char (point-min))
                (if (re-search-forward (format bibtex-completion-notes-key-pattern
                                               (regexp-quote key))
                                       nil
                                       t)
                                        ; Existing entry found:
                    (when (eq major-mode 'org-mode)
                      (org-narrow-to-subtree)
                      (goto-char (point-min))
                      (outline-show-all)
                      (switch-to-buffer-other-window buffer))
                                        ; Create a new entry:
                  (let ((org-capture-templates '(("bibtex" "helm-bibtex"
                                                  entry
                                                  (file+olp+datetree bibtex-completion-notes-path)
                                                  "%(s-format bibtex-completion-notes-template-one-file 'bibtex-completion-apa-get-value entry)"))))
                    (org-capture nil "bibtex"))))))))
      ;; Overwrite bibtex-completion-prepare-entry to handle org-id
      (defun bibtex-completion-prepare-entry (entry &optional fields do-not-find-pdf)
        (when entry ; entry may be nil, in which case just return nil
          (let* ((fields (when fields
                           (append fields
                                   (list "=type=" "=key=" "=has-pdf=" "=has-note="))))
                                        ; Check for PDF:
                 (entry (if (and (not do-not-find-pdf)
                                 (bibtex-completion-find-pdf entry))
                            (cons (cons "=has-pdf=" bibtex-completion-pdf-symbol) entry)
                          entry))
                 (entry-key (cdr (assoc "=key=" entry)))
                                        ; Check for notes:
                 (id (bibtex-completion-get-value "IDS" entry nil))
                 (entry (if (and id
                                 (hash-table-p org-id-locations)
                                 (gethash id org-id-locations))
                            (cons (cons "=has-note=" bibtex-completion-notes-symbol) entry)
                          entry))
                                        ; Remove unwanted fields:
                 (entry (if fields
                            (--filter (member-ignore-case (car it)
                                                          fields)
                                      entry)
                          entry)))
            ;; Normalize case of entry type:
            (setcdr (assoc "=type=" entry)
                    (downcase (cdr (assoc "=type=" entry))))
            ;; Remove duplicated fields:
            (bibtex-completion-remove-duplicated-fields
             entry))))
      ;; Disable file-watcher in bibtex-completion-init
      (defun bibtex-completion-init ()
  "Checks that the files and directories specified by the user
actually exist. Also sets `bibtex-completion-display-formats-internal'."

  ;; Remove current watch-descriptors for bibliography files:
  (mapc (lambda (watch-descriptor)
          (file-notify-rm-watch watch-descriptor))
        bibtex-completion-file-watch-descriptors)
  (setq bibtex-completion-file-watch-descriptors nil)

  ;; Check that all specified bibliography files exist and add file
  ;; watches for automatic reloading of the bibliography when a file
  ;; is changed:
  (mapc (lambda (file)
          (unless (f-file? file)
            (user-error "Bibliography file %s could not be found." file)))
            (bibtex-completion-normalize-bibliography))

  ;; Pre-calculate minimal widths needed by the format strings for
  ;; various entry types:
  (setq bibtex-completion-display-formats-internal
        (mapcar (lambda (format)
                  (let* ((format-string (cdr format))
                         (fields-width 0)
                         (string-width
                          (length
                           (s-format format-string
                                     (lambda (field)
                                       (setq fields-width
                                             (+ fields-width
                                                (string-to-number
                                                 (or (cadr (split-string field ":"))
                                                     ""))))
                                       "")))))
                    (-cons* (car format) format-string (+ fields-width string-width))))
                bibtex-completion-display-formats))))))

;;;; org-pomodoro
(defun my-org/post-init-org-pomodoro ()
  (setq alert-default-style 'libnotify)
  ;; TODO only "play" if music was "paused" in the same session
  (add-hook 'org-pomodoro-started-hook
            (apply-partially #'my/toggle-music "play"))
  (add-hook 'org-pomodoro-break-finished-hook
            (apply-partially #'my/toggle-music "play"))
  (add-hook 'org-pomodoro-finished-hook
            (apply-partially #'my/toggle-music "pause")))

;;; Owned packages

;;;; interleave
(defun my-org/init-interleave ()
  (use-package interleave)
  )

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
    :init (setq gac-automatically-push-p 1
                gac-automatically-fetch-p 1
                gac-automatically-pull-p 1)))

;;;; outshine
(defun my-org/init-outshine ()
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
      (add-hook 'LaTeX-mode-hook 'outshine-mode)
      (add-hook 'prog-mode-hook 'outshine-mode))))

;;;; helm-navi
(defun my-org/init-helm-navi ()
  (use-package helm-navi
    :init
    (progn
      (spacemacs/set-leader-keys
        "sj" 'helm-navi))))

;;;; evil-collection
(defun my-org/init-evil-collection ()
  (use-package evil-collection
    :ensure t
    :after evil
    ;; Hide warning that evil-want-keybinding is not nil. Consequently
    ;; evil-keybindings.el will be loaded, but as none of the conflicting parts
    ;; of evil-collection is activated, there will be no issue.
    ;; https://github.com/emacs-evil/evil/commit/7ff4a877f3c5cc8765ee81a910c25d70940b486f
    :init
    (add-to-list 'warning-suppress-types '(evil-collection))
    :config
    (evil-collection-init 'ebib)))

;;;; ebib
(defun my-org/init-ebib ()
  (use-package ebib
    :init
    (progn
      (spacemacs/set-leader-keys "oe" 'ebib)
      (setq ebib-bibtex-dialect 'biblatex
          ebib-notes-use-single-file "~/Papers/notes.org"))
    ))
