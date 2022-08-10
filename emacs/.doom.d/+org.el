;;; ../.dotfiles/emacs/.doom.d/+org.el -*- lexical-binding: t; -*-


(map! :map org-capture-mode-map
      :localleader
      (:prefix ("r" . "refile")
       "a" #'org-capture-refile)  ;; Overwrite #'org-refile in org-capture-mode
      )

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq
   ;; org-super-agenda-header-map (copy-keymap evil-org-agenda-mode-map)
   ;; ; TODO define default super-agenda groups
   org-super-agenda-groups
   '(;; Each group has an implicit boolean OR operator between its selectors.
     (:name "Today" :time-grid t)
     (:name "Important" :and (:priority "A" :todo ("TODO" "TASK")))
     (:name "Others" :todo ("TASK"))
     (:and (:priority<= "B" :todo ("TODO")))
     (:discard (:anything t))
     ))

  :config
  (org-super-agenda-mode))

(use-package! git-auto-commit-mode
  :init (setq gac-automatically-push-p 1
              gac-automatically-fetch-p 1
              gac-automatically-pull-p 1))

(map! :leader
      ;; Swap doom "o a" and  "o A"
      :desc "Org agenda" "o a"  #'org-agenda
      ;; Custom org-capture shortcut
      :desc "Org Capture" "o c" #'org-capture)
(after! org
  (require 'org-secretary)
  (setq org-directory "~/org/"
        org-roam-directory "~/wiki/"
        citar-notes-paths '("~/wiki/")
        org-cite-global-bibliography '("~/wiki/references.bib")
        org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
        )

  ;; org-secretary
  (defun org-sec-who-view (par)
    "Builds agenda for a given user.  Queried. "
    (let ((who (completing-read "Build todo for user/tag: " org-sec-with-history
                                nil nil "" '(org-sec-with-history 0))))
      (org-sec-with-view "TODO dowith" who)
      (org-sec-assigned-with-view "TASK with" who)
      (org-sec-stuck-with-view "STUCK with" who)))


  ;; org-id
  ;; Ensure that each captured entry gets a unique ID
  (add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)
  (setq org-id-link-to-org-use-id 'create-if-interactive
        org-id-extra-files (append (directory-files "~/wiki" t ".*org" t) (directory-files "~/wiki/refs" t ".*org" t)))

  ;; Capture
  (setq org-capture-templates
        ;; note the backquote ` instead of normal quote '
        `(("p" "New project" entry (file+olp "~/org/organizer.org" "Projects")
           "* TODO %? :prj:\nOPENED: %U\n%a\n\n%i")
          ("b" "Backlog" entry (file+olp "~/org/organizer.org" "Tasks")
           "* BACKLOG [#B]%?\nOPENED: %U\n%a\n\n%i")
          ("t" "Todo" entry (file+olp "~/org/organizer.org" "Tasks")
           "* TODO [#B]%?\nOPENED: %U\n%a\n\n%i")
          ("o" "Task" entry (file+olp "~/org/organizer.org" "Tasks")
           "* TASK [#B]%?\nOPENED: %U\n%a\n\n%i")
          ))

  ;; Refile
  (setq org-refile-targets '((nil . (:maxlevel . 9))
                             (org-agenda-files . (:maxlevel . 9)))
        ;; Allow refile to create parent tasks with confirmation
        org-refile-allow-creating-parent-nodes 'confirm
        )
  ;; Exclude completed tasks from refile targets (https://michael.englehorn.com/config.html)
  (defun bh/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  (setq org-refile-target-verify-function 'bh/verify-refile-target)
  )

(after! org-agenda
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-files `("~/org/"
                           ,(concat "~/org/journal/" (format-time-string "%Y") "/"))

        org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)")
                            (sequence "TASK(f)" "|" "DONE(d)")
                            (sequence "BACKLOG(m)" "|" "CANCELLED(c)"))
        org-tags-exclude-from-inheritance '("prj")
        org-stuck-projects '("+prj/-BACKLOG-DONE" ("TODO" "TASK") ())

        org-log-into-drawer t
        org-default-notes-file "~/org/organizer.org"

        org-agenda-custom-commands
        '(
          ("h" "Todos" tags-todo
           "-doat={.+}-dowith={.+}/!-TASK"
           ((org-agenda-todo-ignore-scheduled t)))
          ("H" "All todos" tags-todo "/!-TASK-BACKLOG"
           ((org-agenda-todo-ignore-scheduled nil)))
          ("A" "Todos with doat or dowith" tags-todo
           "+doat={.+}|dowith={.+}/!-TASK"
           ((org-agenda-todo-ignore-scheduled nil)))
          ("j" "TODO dowith and TASK with"
           ((org-sec-with-view "TODO dowith")
            (org-sec-where-view "TODO doat")
            (org-sec-assigned-with-view "TASK with")
            (org-sec-stuck-with-view "STUCK with")))
          ("J" "Interactive TODO dowith and TASK with"
           ((org-sec-who-view "TODO dowith")))
          ("R" "Week in review" agenda ""  ;; agenda settings
           ((org-agenda-span 'week)
            (org-agenda-start-on-weekday 0) ;; start on Sunday
            (org-agenda-overriding-header "Week in Review")
            (org-agenda-start-with-log-mode t)
            (org-agenda-log-mode-items '(clock state))
            (org-agenda-archives-mode t) ; include archive files
            )))
        ))

(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>.org"
                              "#+TITLE: ${title}\nOPENED: %U\n%i\n")
           :unnarrowed t))
        ))

;; Import as this is a deprecated command upstream in citar due to
;; reliance on 'bibtex-completion'
(require 'citar)  ; TODO defer require
(defun my-citar-pdf-handling (keys-entries)
  "Open or add PDF associated with the KEYS-ENTRIES to library."
  (interactive (list (citar-select-refs :rebuild-cache current-prefix-arg)))
  (bibtex-completion-open-pdf (citar--extract-keys keys-entries)
                              'bibtex-completion-add-pdf-to-library))

(use-package! bibtex-completion)
(use-package! gscholar-bibtex)
(use-package biblio-gscholar
  :load-path "lisp/")
(after! biblio
  (define-key biblio-selection-mode-map "A" 'my/biblio-selection-insert-end-of-bibfile)
  (setq biblio-bibtex-use-autokey t
        biblio-crossref-user-email-address user-mail-address))
(map! :leader :nv "o b" nil)
(map! :leader :desc "Bibliography"
      (:prefix ("o b" . "bibliography")
       :desc "Open note" "n" #'citar-open-notes
       :desc "Google scholar" "g" #'biblio-gscholar-lookup
       :desc "Biblio lookup" "b" #'biblio-lookup
       :desc "Add pdf to library" "p" #'my-citar-pdf-handling))
(after! citar
  (setq citar-bibliography '("~/wiki/references.bib" "~/wiki/references-stable.bib")
        citar-open-note-function 'my-citar-org-open-notes
        citar-file-note-org-include '(org-id org-roam-ref)
        citar-at-point-function 'embark-act
        bibtex-dialect 'biblatex
        )

  (require 'org-id)
  (require 'org-roam)
  (require 'bibtex-completion)

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
                                         (string-glyph-compose
                                          (apply #'string (seq-remove #'nonspacing-mark-p
                                                                      (string-glyph-decompose s)))))
                 (cl-replace (title pair)
                             (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_") ;; convert anything not alphanumeric
                        ("__*" . "_")                   ;; remove sequential underscores
                        ("^_" . "")                     ;; remove starting underscore
                        ("_$" . "")))                   ;; remove ending underscore
               (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
          (downcase slug)))))

  (defun my-citar-org-open-notes (key entry)
    "Open a note file from KEY and ENTRY."
    (if (not org-id-locations) (org-id-locations-load))  ; Ensure org-id-locations is not nil
    (if-let* ((entry (bibtex-completion-get-entry key))
              (id (bibtex-completion-get-value "IDS" entry nil)))
        (org-id-goto id)  ;; Support legacy entries with org-id UUID stored in bibtex entry IDS field
      (if-let* ((hash (gethash key org-id-locations)))
          (org-id-goto key)  ;; New approach: bibtex key is reused as org-id
        (let* ((title (bibtex-completion-get-value "title" entry))
               (author (car (s-split "," (bibtex-completion-shorten-authors (bibtex-completion-get-value "author" entry)))))
               (year (or (bibtex-completion-get-value "year" entry)
                         (car (split-string (bibtex-completion-get-value "date" entry
                                                                         "")
                                            "-"))))
               (file (concat "~/wiki/refs/" year "-" (my/slug author) "-" (my/slug title) ".org"))
               (template (citar-get-template 'note))
               (note-meta
                (when template
                  (citar--format-entry-no-widths
                   entry
                   template)))
               (org-id (when (member 'org-id citar-file-note-org-include)
                         (concat "\n:ID:   " key)))
               (org-roam-key (when (member 'org-roam-ref citar-file-note-org-include)
                               (concat "\n:ROAM_REFS: @" key)))
               (prop-drawer (or org-id org-roam-key))
               (content
                (concat (when prop-drawer ":PROPERTIES:")
                        org-roam-key org-id
                        (when prop-drawer "\n:END:\n")
                        note-meta "\n")))


          (funcall citar-file-open-function file)
          ;; This just overrides other template insertion.
          (erase-buffer)
          (when template (insert content)
                (org-id-add-location key file))))))
  )

(after! bibtex
  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator ""
        bibtex-autokey-year-title-separator ""
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length nil
        bibtex-autokey-titleword-ignore '("A" "An" "On" "The" "Eine?" "Der" "Die" "Das" "And" "and" "To" "to" ".*[^[:upper:][:lower:]0-9].*"))
  )

(after! biblio
  ;; biblio.el action to append to references file
  (defun my/biblio--selection-insert-at-end-of-bibfile-callback (bibtex entry)
    "Add BIBTEX (from ENTRY) to end of a user-specified bibtex file."
    (with-current-buffer (find-file-noselect "~/wiki/references.bib")
      (goto-char (point-max))
      (insert (concat bibtex "\n\n"))
      (save-buffer))
    (quit-window)
    (message "Inserted bibtex entry for %S."
	           (biblio--prepare-title (biblio-alist-get 'title entry))))
  (defun my/biblio-selection-insert-end-of-bibfile ()
    "Insert BibTeX of current entry at the end of user-specified bibtex file."
    (interactive)
    (biblio--selection-forward-bibtex #'my/biblio--selection-insert-at-end-of-bibfile-callback))

  ;; Overwrite biblio-url-retrieve to adapt timeout
  ;; https://github.com/cpitclaudel/biblio.el/issues/39
  (defun biblio-url-retrieve (url callback)
    "Wrapper around `url-queue-retrieve'.
URL and CALLBACK; see `url-queue-retrieve'"
    (message "Fetching %s" url)
    (if biblio-synchronous
        (with-current-buffer (url-retrieve-synchronously url)
          (funcall callback nil))
      (setq url-queue-timeout 10)
      (url-queue-retrieve url callback))))

(after! bibtex-completion
  (setq
   bibtex-completion-bibliography '("~/wiki/references.bib")
   bibtex-completion-library-path '("~/library/")
   bibtex-completion-notes-path "~/wiki/"
   bibtex-completion-cite-default-command "autocite"
   bibtex-completion-cite-prompt-for-optional-arguments nil
   ))


(defun bibtex-clean-entry-reformat-reference-keys ()
  (interactive)
  (bibtex-clean-entry t))

;; Overwrite bibtex-clean-entry keybinding with bibtex-clean-entry-reformat-reference-keys
(map! :map bibtex-mode-map
      "C-c C-c" #'bibtex-clean-entry-reformat-reference-keys
      "C-c C-C" #'bibtex-clean-entry
      :localleader
      "c" #'bibtex-clean-entry-reformat-reference-keys
      "C" #'bibtex-clean-entry
      )

;; https://github.com/jkitchin/org-ref/blob/f058835db31c5f7266b9686c9da84ecf4ebc4314/org-ref-isbn.el
(defun isbn-to-bibtex (isbn)
  "Get bibtex entry for ISBN and insert it into ~/wiki/references.bib.
Nothing happens if an entry with the generated key already exists
in the file. Data comes from worldcat."
  (interactive
   (list
    (read-string
     "ISBN: "
     ;; now set initial input
     (cond
      ;; If region is active and it starts with a number, we use it
      ((and  (region-active-p)
             (s-match "^[0-9]" (buffer-substring (region-beginning) (region-end))))
       (buffer-substring (region-beginning) (region-end)))
      ;; if first entry in kill ring starts with a number assume it is an isbn
      ;; and use it as the guess
      ((stringp (car kill-ring))
       (when (s-match "^[0-9]" (car kill-ring))
	 (car kill-ring)))
      ;; type or paste it in
      (t
       nil)))))

  (let* ((url (format "https://www.ottobib.com/isbn/%s/bibtex" isbn))
	 (entry))
    (with-current-buffer (url-retrieve-synchronously url t t)
      (goto-char (point-min))
      (when (re-search-forward "@[a-zA-Z]+{.+\\(\n\s+[^\n]+\\)+}$" nil t)
	(setq entry (match-string 0))))

    (if (not entry)
	(message "Nothing found.")
      (find-file "~/wiki/references.bib")
      (goto-char (point-max))
      (insert (with-temp-buffer
		(insert (concat entry "\n}"))
		(goto-char (point-min))
		;; [2020-06-06 Sat] I got a report that ottobib returns entries
		;; with ,, in the first line. here if we find one, I eliminate
		;; one of them.
		(when (re-search-forward ",," nil t)
		  (delete-char -1))
		(bibtex-clean-entry)
		(bibtex-fill-entry)
                (s-trim (buffer-string))))
      (save-buffer))))
