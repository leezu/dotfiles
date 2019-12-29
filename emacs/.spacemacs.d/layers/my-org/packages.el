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
    org-brain

    ;; Packages owned by bibtex layer
    org-ref
    helm-bibtex

    ;; Owned packages
    anki-editor
    evil-collection
    ebib
    outshine
    interleave
    org-super-agenda
    (git-auto-commit-mode :fetcher github
                          :repo "leezu/git-auto-commit-mode"
                          :branch "leezu")
    org-drill
    ))

;;; Packages owned by other layers
;;;; org
(defun my-org/post-init-org ()
  (setq org-adapt-indentation nil  ;; Disable indentation in org mode
        org-startup-folded 'showall
        org-startup-with-latex-preview t
        org-catch-invisible "error"  ;; Cancel invisible edits
        org-enforce-todo-dependencies t
        org-agenda-window-setup 'current-window
        org-agenda-inhibit-startup t
        org-agenda-use-tag-inheritance nil
        org-agenda-skip-scheduled-if-done t  ;; Don't show DONE tasks in time-grid
        org-preview-latex-default-process 'dvisvgm
        org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  ;; Latex
  (setq org-format-latex-header "\\documentclass{article}
\\usepackage[usenames]{color}
% add mathtools here, as org-latex-packages-alist is not used for inline latex fragments
\\usepackage{mathtools}
[PACKAGES]
[DEFAULT-PACKAGES]
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}")

;;;;; org-tempo
  ;; Org 9.2 comes with a new template expansion mechanism, combining
  ;; ~org-insert-structure-template~ bound to ~C-c C-,~. To activate previous
  ;; patterns, e.g. =<s=, it is necessary to require Org Tempo
  (require 'org-tempo)
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
        (org-show-all)
        (goto-char m)
        (move-marker m nil)
        (let* ((heading (org-get-heading 'no-tags))
               (ibuf (org-get-indirect-buffer (current-buffer) heading)))
          (switch-to-buffer-other-window ibuf)
          (with-current-buffer ibuf
            (goto-char (org-find-entry-with-id id))  ;; This shouldn't be
            (org-back-to-heading)                    ;; necessary but is..
            (org-narrow-to-subtree)
            (org-show-all)))))) ;; TODO org-show-all has no visible effect
  ;; This makes sure that each captured entry gets a unique ID
  (add-hook 'org-capture-prepare-finalize-hook
            'org-id-get-create)
  (setq org-id-link-to-org-use-id 'create-if-interactive
        org-id-extra-files
        (cons "~/Papers/notes.org" (directory-files "~/wiki" t ".*org" t)))
  (defun my/zettel-id-targets (&optional overview)
    (sort (directory-files "~/wiki/"
                           t (if overview "O.*org" ".*org") t)
          'string>))
  (defun my/org-id-complete-link (&optional arg)
    "Create an id: link using completion"
    (concat "id:"
            (org-id-get-with-outline-path-completion '((my/zettel-id-targets . (:maxlevel . 1))))))
  (org-link-set-parameters "id" :complete 'my/org-id-complete-link)
;;;;; Zettelkasten
  (require 'helm-ag)
  (defun my/helm-zettelkasten-ag (path)
    (interactive)
    (helm-ag--init-state)
    (let ((helm-ag--last-query "^\\* ")
          (helm-ag--default-directory path)
          (helm-ag--default-target path)
          )
      (helm-attrset 'search-this-file nil helm-ag-source)
      (helm-attrset 'name (helm-ag--helm-header helm-ag--default-directory) helm-ag-source)
      (helm :sources '(helm-ag-source) :buffer "*helm-ag*" :keymap helm-ag-map
            :history 'helm-ag--helm-history)))
  (defun my/helm-zettelkasten-ag-wiki ()
    (interactive)
    (require 'org-brain)
    (save-window-excursion
      (my/helm-zettelkasten-ag "~/wiki")
      (setq my/org-brain-current-entry (org-brain-entry-at-pt)))
    (org-brain-visualize my/org-brain-current-entry))
  (spacemacs/set-leader-keys
    "oz" 'my/helm-zettelkasten-ag-wiki)

;;;;; Capture
  (defun new-zettel-file (path &optional overview)
    (concat path
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
           "* WAIT %?\nOPENED: %U\n%a\n\n%i")
          ("s" "Someday" entry (file+olp "~/org/organizer.org" "Someday")
           "* SOMEDAY %?\nOPENED: %U\n%a\n\n%i")

          ;; zettelkasten
          ("z" "Zettel"
           entry
           (file (lambda () (new-zettel-file "~/wiki/")))
           "* %?\nOPENED: %U\n\n%i\n")
          ("o" "Overview Zettel"
           entry
           (file (lambda () (new-zettel-file "~/wiki/" t)))
           "* %?\nOPENED: %U\n\n%i\n")
          ("a" "Anki")
          ("aa" "Anki card (current file)" entry (file+olp buffer-file-name)
           "* Card\n:PROPERTIES:\n:ANKI_DECK: Wiki\n:ANKI_NOTE_TYPE: Basic\n:END:\n** Front\n\n** Back\n"
           :immediate-finish t)
          ("ac" "Anki cloze (current file)" entry (file+olp buffer-file-name)
           "* Cloze\n:PROPERTIES:\n:ANKI_DECK: Wiki\n:ANKI_NOTE_TYPE: Cloze\n:END:\n** Text\n"
           :immediate-finish t :jump-to-captured t)

          ;; notes
          ("n" "Note (organizer.org)" entry (file+olp+datetree "~/org/organizer.org")
           "* %?\nCREATED: %U\n%a\n\n%i" :prepend t)
          ("l" "Note (current file)" entry (file+olp+datetree buffer-file-name)
           "* %?\nCREATED: %U\n%a\n\n%i" :prepend t)
          ;; ("c" "Clocked entry note" entry (clock)
          ;;  "* %U %?\n%a\n\n%i" :empty-lines 1)

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

;;;;; Load extra functionality
  (with-eval-after-load 'org
    (require 'org-inlinetask))
  )

;;;; org-agenda
(defun my-org/post-init-org-agenda ()
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-files `("~/org/"
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
  (setq org-ref-default-bibliography '("~/wiki/references.bib")
        org-ref-pdf-directory "~/Papers/"
        org-ref-bibliography-notes "~/Papers/notes.org")

  ;; Template for paper notes
  (setq org-ref-note-title-format
        "** %y - %t
:PROPERTIES:
:BIBKEY: %k
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
  (setq bibtex-completion-bibliography '("~/wiki/references.bib")
        bibtex-completion-library-path '("~/library/")
        bibtex-completion-notes-path "~/wiki/"
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
                (let* ((current-new-zettel-file (new-zettel-file "~/wiki/"))
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
                            (save-buffer)
                            (bury-buffer)))
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
                 (id (bibtex-completion-get-value "IDS" entry nil))
                                        ; Check for PDF:
                 (entry (if (and id
                                 (not do-not-find-pdf)
                                 (bibtex-completion-find-pdf id))  ; use ID instead of key
                            (cons (cons "=has-pdf=" bibtex-completion-pdf-symbol) entry)
                          entry))
                 (entry-key (cdr (assoc "=key=" entry)))
                                        ; Check for notes:
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
      ;; Overwrite bibtex-completion-apa-get-value to preserve title capitalization
      (defun bibtex-completion-apa-get-value (field entry &optional default)
        ;; Virtual fields:
        (if (string= field "author-or-editor")
            (let ((value (bibtex-completion-get-value "author" entry)))
              (if value
                  (bibtex-completion-apa-format-authors value)
                (bibtex-completion-apa-format-editors
                 (bibtex-completion-get-value "editor" entry))))
          ;; Real fields:
          (let ((value (bibtex-completion-get-value field entry)))
            (if value
                (pcase field
                  ;; https://owl.english.purdue.edu/owl/resource/560/06/
                  ("author" (bibtex-completion-apa-format-authors value))
                  ("editor" (bibtex-completion-apa-format-editors value))
                  ;; For three or more authors, abbreviate to "Author et al"
                  ("author-abbrev" (bibtex-completion-apa-format-authors-abbrev value))
                  ("title" value)
                  ("booktitle" value)
                  ;; Maintain the punctuation and capitalization that is used by
                  ;; the journal in its title.
                  ("pages" (s-join "–" (s-split "[^0-9]+" value t)))
                  ("doi" (s-concat " http://dx.doi.org/" value))
                  ("year" (or value
                              (car (split-string (bibtex-completion-get-value "date" entry "") "-"))))
                  (_ value))
              ""))))
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
                bibtex-completion-display-formats)))
      ;; Overwrite bibtex-completion-open-pdf to open based on IDS
      (defun bibtex-completion-open-pdf (keys &optional fallback-action)
        (dolist (key keys)
          (let* ((entry (bibtex-completion-get-entry key))
                 (id (bibtex-completion-get-value "IDS" entry nil))
                 (pdf (if id (bibtex-completion-find-pdf id bibtex-completion-find-additional-pdfs) nil)))
            (cond
             ((> (length pdf) 1)
              (let* ((pdf (f-uniquify-alist pdf))
                     (choice (completing-read "File to open: " (mapcar 'cdr pdf) nil t))
                     (file (car (rassoc choice pdf))))
                (funcall bibtex-completion-pdf-open-function file)))
             (pdf
              (funcall bibtex-completion-pdf-open-function (car pdf)))
             (fallback-action
              (funcall fallback-action (list key)))
             (t
              (message "No PDF(s) found for this entry: %s"
                       key)))))))))

;;;; org-pomodoro
(defun my-org/post-init-org-pomodoro ()
  (setq alert-default-style 'mode-line)
  ;; TODO only "play" if music was "paused" in the same session
  (add-hook 'org-pomodoro-started-hook
            (apply-partially #'my/toggle-music "play"))
  (add-hook 'org-pomodoro-break-finished-hook
            (apply-partially #'my/toggle-music "play"))
  (add-hook 'org-pomodoro-finished-hook
            (apply-partially #'my/toggle-music "pause")))

;;;; org-brain
(defun my-org/post-init-org-brain ()
  (setq org-brain-path "~/wiki/"
        org-brain-scan-directories-recursively nil
        org-brain-visualize-default-choices 'all
        org-brain-show-text t
        org-brain-show-resources t
        org-brain-title-max-length 50
        org-brain-include-file-entries nil
        org-brain-file-entries-use-title nil)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "v" 'org-brain-visualize-entry-at-pt)

  (with-eval-after-load 'org-brain
    ;; Overwrite org-brain-choose-entries to avoid parsing all files but rather
    ;; select a single file based on helm-ag and parse only that one
    (defun org-brain-choose-entries (prompt entries &optional predicate require-match initial-input hist def inherit-input-method)
      (unless org-id-locations (org-id-locations-load))
      (save-window-excursion
        (my/helm-zettelkasten-ag "~/wiki")
        (list (org-brain-entry-from-id (org-id-get)))
        ))

    ;; Overwrite org-brain-siblings to avoid displaying all siblings in
    ;; org-brain-visualize. Instead, claim that there are no siblings.
    (defun org-brain-siblings (entry)
      (delete-dups
       (mapcar
        (lambda (parent) (cons parent nil))
        (org-brain-parents entry))))))

;;; Owned packages
(defun my-org/init-anki-editor ()
  (use-package anki-editor-mode
    :hook org-mode
    :init (progn (spacemacs/set-leader-keys-for-major-mode
                   'org-mode "iN" 'anki-editor-insert-note "ea"
                   'anki-editor-push-notes)
                 (setq anki-editor-use-math-jax t)))
  (defun my-anki-editor-push-all ()
    (anki-editor-push-notes nil
                            nil
                            (split-string (shell-command-to-string "find ~/wiki -type f -name '*.org' -maxdepth 1 | xargs grep -l 'ANKI'")))))

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
          (:and (:priority<= "B" :todo ("TODO" "NEXT" "INPROGRESS")))
          (:todo ("TODO" "NEXT" "INPROGRESS"))
          (:name "Backlog Projects"  :todo "BACKLOG")
          (:discard (:anything t))
          )))

;;;; org-drill
(defun my-org/init-org-drill ()
  (use-package org-drill
    :defer t
    :ensure org-plus-contrib
    :commands (org-drill)
    :config (setq org-drill-auto-pronounce nil)))
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

  ;; via https://github.com/ekaschalk/.spacemacs.d/blob/58e266097efda77366fbd7ccafa0313ff5c491b4/layers/macros/local/macros/macros.el
  (defun define-keys (keymap &rest pairs)
    "Define alternating key-def PAIRS for KEYMAP."
    (-each
        (-partition 2 pairs)
      (-lambda ((key def))
        (define-key keymap key def))))

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
