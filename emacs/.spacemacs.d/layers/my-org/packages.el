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
    org-roam

    ;; Packages owned by bibtex layer
    org-ref
    biblio
    helm-bibtex

    ;; Owned packages
    anki-editor
    gscholar-bibtex
    outshine
    interleave
    org-super-agenda
    org-sidebar
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
        org-catch-invisible "error"  ;; Cancel invisible edits
        org-enforce-todo-dependencies t
        org-agenda-window-setup 'current-window
        org-agenda-inhibit-startup t
        org-agenda-use-tag-inheritance nil
        org-agenda-skip-scheduled-if-done t)  ;; Don't show DONE tasks in time-grid
  ;; Latex
  (setq org-startup-with-latex-preview t
        org-latex-compiler "xelatex"
        org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
        org-preview-latex-default-process 'dvisvgm
        org-preview-latex-process-alist  ;; overwrite to use xelatex instead of latex
        '((dvisvgm :programs ("xelatex" "dvisvgm")
                   :description "xdv > svg"
                   :message "you need to install the programs: xelatex and dvisvgm."
                   :image-input-type "xdv"
                   :image-output-type "svg"
                   :image-size-adjust (1.7 . 1.5)
                   :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                   :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))
          (imagemagick :programs("xelatex" "convert")
                       :description "pdf > png"
                       :message "you need to install the programs: xelatex and imagemagick."
                       :image-input-type "pdf"
                       :image-output-type "png"
                       :image-size-adjust (1.0 . 1.0)
                       :latex-compiler ("xelatex -interaction nonstopmode -output-directory %o %f")
                       :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O")))
        org-latex-default-packages-alist '((""     "fontenc"   t)
                                           (""     "fixltx2e"  nil)
                                           (""     "wrapfig"   nil)
                                           (""     "soul"      t)
                                           (""     "textcomp"  t)
                                           (""     "marvosym"  t)
                                           (""     "wasysym"   t)
                                           (""     "latexsym"  t)
                                           (""     "amssymb"   t)
                                           (""     "hyperref"  nil)
                                           (""     "unicode-math"  t0)
                                           "\\setmainfont{Linux Libertine O}"
                                           "\\setmathfont{STIX2Math.otf}"))

;;;;; org-tempo
  ;; Org 9.2 comes with a new template expansion mechanism, combining
  ;; ~org-insert-structure-template~ bound to ~C-c C-,~. To activate previous
  ;; patterns, e.g. =<s=, it is necessary to require Org Tempo
  (require 'org-tempo)
;;;;; org-id
  (require 'org-id)
  ;; This makes sure that each captured entry gets a unique ID
  (add-hook 'org-capture-prepare-finalize-hook
            'org-id-get-create)
  (setq org-id-link-to-org-use-id 'create-if-interactive
        org-id-extra-files
        (append (cons "~/Papers/notes.org" (directory-files "~/wiki" t ".*org" t)) (directory-files "~/wiki/refs" t ".*org" t)))
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
          ("S" "Clocked entry checkbox" checkitem (clock) "[ ] %i")
          ("w" "Waiting for" entry (file+olp "~/org/organizer.org" "Tasks")
           "* WAIT %?\nOPENED: %U\n%a\n\n%i")
          ("s" "Someday" entry (file+olp "~/org/organizer.org" "Someday")
           "* SOMEDAY %?\nOPENED: %U\n%a\n\n%i")

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

        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "INPROGRESS" "VERYIMPORTANTTASK(i)" "|" "DONE(d!)" "CANCELLED(C@)")
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
  (spacemacs/set-leader-keys-for-major-mode 'bibtex-mode "c" 'org-ref-clean-bibtex-entry)

  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator ""
        bibtex-autokey-year-title-separator ""
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length nil
        bibtex-autokey-titleword-ignore '("A" "An" "On" "The" "Eine?" "Der" "Die" "Das" "And" "and" "To" "to" ".*[^[:upper:][:lower:]0-9].*"))

  (setq org-ref-default-bibliography '("~/wiki/references.bib")
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex)

  ;; Tell org-ref to let helm-bibtex find notes for it
  (setq org-ref-notes-function
        (lambda (thekey)
          (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
            (bibtex-completion-edit-notes
             (list (car (org-ref-get-bibtex-key-and-file thekey))))))))

;;;; org-roam
(defun my-org/post-init-org-roam ()
  (setq org-roam-v2-ack t
        org-roam-capture-templates
        '(("d" "default" plain "%?"
          :target (file+head "%<%Y%m%d%H%M%S>.org"
                             "#+TITLE: ${title}\nOPENED: %U\n%i\n")
          :unnarrowed t))))
;;;; helm-bibtex
(defun my-org/post-init-helm-bibtex ()
  (setq bibtex-completion-bibliography '("~/wiki/references.bib")
        bibtex-completion-library-path '("~/library/")
        bibtex-completion-notes-path "~/wiki/"
        bibtex-completion-cite-default-command "autocite"
        bibtex-completion-cite-prompt-for-optional-arguments nil)

  (setq bibtex-completion-fallback-options
  '(("Google Scholar                            (biblio-gscholar.el)"
     . (lambda (search-expression) (biblio-lookup #'biblio-gscholar-backend search-expression)))
    ("arXiv                                     (biblio.el)"
     . (lambda (search-expression) (biblio-lookup #'biblio-arxiv-backend search-expression)))
    ("DBLP (computer science bibliography)      (biblio.el)"
     . (lambda (search-expression) (biblio--lookup-1 #'biblio-dblp-backend search-expression)))
    ("CrossRef                                  (biblio.el)"
     . (lambda (search-expression) (biblio-lookup #'biblio-crossref-backend search-expression)))
    ("IEEE                                      (biblio.el)"
     . (lambda (search-expression) (biblio--lookup-1 #'biblio-ieee-backend search-expression)))))

  (spacemacs/set-leader-keys "ob" 'helm-bibtex-with-local-bibliography)

  (setq bibtex-completion-notes-template-one-file
        (concat
         "* ${title} (${year})\n"
         ":PROPERTIES:\n"
         ":ROAM_KEY: cite:${=key=}\n"
         ":END:\n"
         "%?")
        bibtex-completion-notes-template-multiple-files
        (concat
         ":PROPERTIES:\n"
         ":ID: ${ids}\n" ;; TODO breaks if bibtex entry has multiple aliases besides UUID
         ":ROAM_KEY: cite:${=key=}\n"
         ":END:\n"
         "#+TITLE: ${title} (${year})\n"
         "%?\n")
        )

  ;; Add bibtex-completion "add pdf" action to org-ref
  (with-eval-after-load "org-ref"
    (add-to-list 'org-ref-helm-user-candidates
                 '("Add PDF to library" . (lambda () (bibtex-completion-add-pdf-to-library (list (car (org-ref-get-bibtex-key-and-file))))))
                 t))

  ;; Overwrite bibtex-completion-edit-notes to use org-capture
  (with-eval-after-load "helm-bibtex"
    (progn
      (require 'org-capture)
      (require 'org-id)
      (require 'org-roam)
      (org-id-locations-load) ; TODO necessary to avoid org-id-locations being nil
      (cl-defmethod my/slug (title) ; Adapted from org-roam-node-slug
        "Return the slug of title."
        (let ((slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                                 768 ; U+0300 COMBINING GRAVE ACCENT
                                 769 ; U+0301 COMBINING ACUTE ACCENT
                                 770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                                 771 ; U+0303 COMBINING TILDE
                                 772 ; U+0304 COMBINING MACRON
                                 774 ; U+0306 COMBINING BREVE
                                 775 ; U+0307 COMBINING DOT ABOVE
                                 776 ; U+0308 COMBINING DIAERESIS
                                 777 ; U+0309 COMBINING HOOK ABOVE
                                 778 ; U+030A COMBINING RING ABOVE
                                 780 ; U+030C COMBINING CARON
                                 795 ; U+031B COMBINING HORN
                                 803 ; U+0323 COMBINING DOT BELOW
                                 804 ; U+0324 COMBINING DIAERESIS BELOW
                                 805 ; U+0325 COMBINING RING BELOW
                                 807 ; U+0327 COMBINING CEDILLA
                                 813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                                 814 ; U+032E COMBINING BREVE BELOW
                                 816 ; U+0330 COMBINING TILDE BELOW
                                 817 ; U+0331 COMBINING MACRON BELOW
                                 )))
          (cl-flet* ((nonspacing-mark-p (char)
                                        (memq char slug-trim-chars))
                     (strip-nonspacing-marks (s)
                                             (ucs-normalize-NFC-string
                                              (apply #'string (seq-remove #'nonspacing-mark-p
                                                                          (ucs-normalize-NFD-string s)))))
                     (cl-replace (title pair)
                                 (replace-regexp-in-string (car pair) (cdr pair) title)))
            (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_") ;; convert anything not alphanumeric
                            ("__*" . "_")                   ;; remove sequential underscores
                            ("^_" . "")                     ;; remove starting underscore
                            ("_$" . "")))                   ;; remove ending underscore
                   (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
              (downcase slug)))))
      (defun bibtex-completion-edit-notes (keys)
        "Open the notes associated with the selected entry or create new via org-capture."
        (dolist (key keys)
          (let* ((entry (bibtex-completion-get-entry key))
                 (id (bibtex-completion-get-value "IDS" entry nil))
                 (title (bibtex-completion-get-value "title" entry))
                 (author (car (s-split "," (bibtex-completion-shorten-authors (bibtex-completion-get-value "author" entry)))))
                 (year (or (bibtex-completion-get-value "year" entry)
                           (car (split-string (bibtex-completion-get-value "date" entry
                                                                           "")
                                              "-"))))
                 (entry (push (cons "year" year)
                              entry))
                 (buffer (generate-new-buffer-name "bibtex-notes")))
            (if (and bibtex-completion-notes-path
                     (f-directory? bibtex-completion-notes-path))
                (let* ((current-new-file (concat "~/wiki/refs/" year "-" (my/slug author) "-" (my/slug title) ".org"))
                       (org-capture-templates '(("bibtex" "helm-bibtex"
                                                 plain
                                                 (file current-new-file)
                                                 "%(s-format bibtex-completion-notes-template-multiple-files 'bibtex-completion-apa-get-value entry)"
                                                 :unnarrowed t))))
                                        ; One notes file per publication based on org-id:
                  (if (and id
                           (hash-table-p org-id-locations)
                           (gethash id org-id-locations))
                                        ; id is not nil and exists
                      (with-helm-current-buffer (org-id-goto id))
                    (if id
                                        ; id is not nil, but no entry exists
                        (progn
                          (org-capture nil "bibtex")
                          (org-id-add-location id current-new-file))
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
                          (org-id-add-location id current-new-file))))))
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
                 (entry (if (and (not do-not-find-pdf) (bibtex-completion-find-pdf entry))
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
                bibtex-completion-display-formats))))))

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

;;;; biblio
(defun my-org/post-init-biblio ()
  (use-package biblio-gscholar
    :load-path "lisp/"
    :config
    (define-key biblio-selection-mode-map "A" 'my/biblio-selection-insert-end-of-bibfile))

  (spacemacs/set-leader-keys "og" 'biblio-gscholar-lookup)

  ;; biblio.el action to append to references file
  (defun my/biblio--selection-insert-at-end-of-bibfile-callback (bibtex entry)
    "Add BIBTEX (from ENTRY) to end of a user-specified bibtex file."
    (with-current-buffer (find-file-noselect "~/wiki/references.bib")
      (goto-char (point-max))
      (insert (concat bibtex "\n\n"))
      (org-ref-clean-bibtex-entry)
      (save-buffer))
    (quit-window)
    (message "Inserted bibtex entry for %S."
	           (biblio--prepare-title (biblio-alist-get 'title entry))))
  (defun my/biblio-selection-insert-end-of-bibfile ()
    "Insert BibTeX of current entry at the end of user-specified bibtex file."
    (interactive)
    (biblio--selection-forward-bibtex #'my/biblio--selection-insert-at-end-of-bibfile-callback))

  ;; Overwrite biblio-url-retrieve to adapt timeout
  (defun biblio-url-retrieve (url callback)
    "Wrapper around `url-queue-retrieve'.
URL and CALLBACK; see `url-queue-retrieve'"
    (message "Fetching %s" url)
    (if biblio-synchronous
        (with-current-buffer (url-retrieve-synchronously url)
          (funcall callback nil))
      (setq url-queue-timeout 10)
      (url-queue-retrieve url callback))))

;;;; interleave
(defun my-org/init-interleave ()
  (use-package interleave)
  )

;;;; gscholar-bibtex
(defun my-org/init-gscholar-bibtex ()
  (use-package gscholar-bibtex
    :defer t))
;;;; org-super-agenda
(defun my-org/init-org-super-agenda ()
  (use-package org-super-agenda
    :config (org-super-agenda-mode))

  (setq org-super-agenda-groups
        '(;; Each group has an implicit boolean OR operator between its selectors.
          (:name "Very Important Task" :todo "VERYIMPORTANTTASK")
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

;;;; org-sidebar
(defun my-org/init-org-sidebar ()
  (use-package org-super-agenda)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ms" 'org-sidebar-toggle)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "mt" 'org-sidebar-tree-toggle))

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
