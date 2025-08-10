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
  (add-hook! 'org-capture-after-finalize-hook (org-element-cache-reset t))

  (require 'org-secretary)
  (setq org-directory "~/org/"
        org-roam-directory "~/wiki/"
        org-cite-global-bibliography '("~/wiki/references.bib" "~/wiki/references-stable.bib")
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
        org-id-extra-files (append (directory-files "~/wiki" t ".*org" t) (directory-files "~/wiki/references" t ".*org" t)))

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
  ; https://github.com/org-roam/org-roam/issues/2506#issuecomment-2723312590
  (defun patch/org-roam-capture--setup-target-location ()
    "Initialize the buffer, and goto the location of the new capture.
     Return the ID of the location."
    (let (p new-file-p)
      (pcase (org-roam-capture--get-target)
        (`(file ,path)
         (setq path (org-roam-capture--target-truepath path)
               new-file-p (org-roam-capture--new-file-p path))
         (when new-file-p (org-roam-capture--put :new-file path))
         (set-buffer (org-capture-target-buffer path))
         (widen)
         (setq p (goto-char (point-min))))
        (`(file+olp ,path ,olp)
         (setq path (org-roam-capture--target-truepath path)
               new-file-p (org-roam-capture--new-file-p path))
         (when new-file-p (org-roam-capture--put :new-file path))
         (set-buffer (org-capture-target-buffer path))
         (setq p (point-min))
         (let ((m (org-roam-capture-find-or-create-olp olp)))
           (goto-char m))
         (widen))
        (`(file+head ,path ,head)
         (setq path (org-roam-capture--target-truepath path)
               new-file-p (org-roam-capture--new-file-p path))
         (set-buffer (org-capture-target-buffer path))
         (when new-file-p
           (org-roam-capture--put :new-file path)
           (insert (org-roam-capture--fill-template head 'ensure-newline)))
         (widen)
         (setq p (goto-char (point-min))))
        (`(file+head+olp ,path ,head ,olp)
         (setq path (org-roam-capture--target-truepath path)
               new-file-p (org-roam-capture--new-file-p path))
         (set-buffer (org-capture-target-buffer path))
         (widen)
         (when new-file-p
           (org-roam-capture--put :new-file path)
           (insert (org-roam-capture--fill-template head 'ensure-newline)))
         (setq p (point-min))
         (let ((m (org-roam-capture-find-or-create-olp olp)))
           (goto-char m)))
        (`(file+datetree ,path ,tree-type)
         (setq path (org-roam-capture--target-truepath path))
         (require 'org-datetree)
         (widen)
         (set-buffer (org-capture-target-buffer path))
         (unless (file-exists-p path)
           (org-roam-capture--put :new-file path))
         (funcall
          (pcase tree-type
            (`week #'org-datetree-find-iso-week-create)
            (`month #'org-datetree-find-month-create)
            (_ #'org-datetree-find-date-create))
          (calendar-gregorian-from-absolute
           (cond
            (org-overriding-default-time
             ;; Use the overriding default time.
             (time-to-days org-overriding-default-time))
            ((org-capture-get :time-prompt)
             ;; Prompt for date.  Bind `org-end-time-was-given' so
             ;; that `org-read-date-analyze' handles the time range
             ;; case and returns `prompt-time' with the start value.
             (let* (;; (org-time-was-given nil)
                    (org-end-time-was-given nil)
                    (prompt-time (org-read-date
                                  nil t nil "Date for tree entry:")))
               (org-capture-put
                :default-time
                (if (or org-time-was-given
                        (= (time-to-days prompt-time) (org-today)))
                    prompt-time
                  ;; Use 00:00 when no time is given for another
                  ;; date than today?
                  (apply #'encode-time 0 0
                         org-extend-today-until
                         (cl-cdddr (decode-time prompt-time)))))
               (time-to-days prompt-time)))
            ((org-capture-get :default-time)
             (time-to-days (org-capture-get :default-time)))
            (t
             ;; Current date, possibly corrected for late night
             ;; workers.
             (org-today)))))
         (setq p (point)))
        (`(node ,title-or-id)
         ;; first try to get ID, then try to get title/alias
         (let ((node (or (org-roam-node-from-id title-or-id)
                         (org-roam-node-from-title-or-alias title-or-id)
                         (user-error "No node with title or id \"%s\"" title-or-id))))
           (set-buffer (org-capture-target-buffer (org-roam-node-file node)))
           (goto-char (org-roam-node-point node))
           (setq p (org-roam-node-point node)))))
      ;; Setup `org-id' for the current capture target and return it back to the
      ;; caller.
      (save-excursion
        (goto-char p)
        (if-let ((id (org-entry-get p "ID")))
            (setf (org-roam-node-id org-roam-capture--node) id)
          (org-entry-put p "ID" (org-roam-node-id org-roam-capture--node)))
        (prog1
            (org-id-get)
          (run-hooks 'org-roam-capture-new-node-hook)))))
  (advice-add 'org-roam-capture--setup-target-location :override #'patch/org-roam-capture--setup-target-location)

  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>.org"
                              "#+TITLE: ${title}\nOPENED: %U\n%i\n")
           :unnarrowed t))
        ))

(use-package! bibtex-completion)
(use-package! gscholar-bibtex)
(use-package biblio-gscholar
  :load-path "~/.doom.d/lisp/")
(after! biblio
  (define-key biblio-selection-mode-map "A" 'my/biblio-selection-insert-end-of-bibfile)
  (setq biblio-bibtex-use-autokey t
        biblio-crossref-user-email-address user-mail-address))
(map! :leader :nv "o b" nil)
(map! :leader :desc "Bibliography"
      (:prefix ("o b" . "bibliography")
       :desc "Citar" "c" #'citar-open
       :desc "Open note" "n" #'citar-open-notes
       :desc "Open file" "f" #'citar-open-files
       :desc "Add file" "F" #'citar-add-file-to-library
       :desc "Google scholar" "g" #'biblio-gscholar-lookup
       :desc "Biblio lookup" "b" #'biblio-lookup))
(after! citar
  (require 'citar-org-roam)
  (citar-org-roam-mode)
  (setq citar-bibliography '("~/wiki/references.bib" "~/wiki/references-stable.bib")
        citar-at-point-function 'embark-act
        citar-library-paths '("~/library/")
        citar-notes-paths '("~/wiki/")
        citar-org-roam-note-title-template "Notes on \"${title}\""
        citar-file-open-functions '(("html" . citar-file-open-external)
                                    ("pdf" . citar-file-open-external)
                                    (t . find-file))
        bibtex-dialect 'biblatex))

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
   bibtex-completion-bibliography '("~/wiki/references.bib" "~/wiki/references-stable.bib")
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

;; https://github.com/jkitchin/org-ref/commit/8108750c9d94908fa01ac20b0daec53f7df85303
(defun isbn-to-bibtex-open-library (isbn)
  "Retrieve bibtex entry for a book with ISBN using openlibrary.org.
API: https://openlibrary.org/developers/api
"
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

  (let* ((url (format "https://openlibrary.org/isbn/%s.json" isbn))
	 (json (with-current-buffer (url-retrieve-synchronously url)
		 (json-read-from-string (string-trim (buffer-substring url-http-end-of-headers (point-max))))))
	 (title (cdr (assoc 'title json)))
	 (publisher (s-join ", " (cdr (assoc 'publishers json))))
	 (year (cdr (assoc 'publish_date  json)))
	 ;; this is a list of urls
	 (author-urls (cdr (assoc 'authors json)))
	 (authors (s-join " and "
			  (cl-loop for aurl across author-urls
				   collect
				   (with-current-buffer (url-retrieve-synchronously
							 (format "https://openlibrary.org%s.json"
								 (cdr (assoc 'key  aurl))))
				     (cdr (assoc 'personal_name
						 (json-read-from-string
						  (string-trim (buffer-substring url-http-end-of-headers (point-max))))))))))
	 (burl (format "https://openlibrary.org/%s" (cdr (assoc 'key json))))
	 (bibtex (format "@Book{,
  author = 	 {%s},
  title = 	 {%s},
  publisher = 	 {%s},
  year = 	 {%s},
  url = {%s}
}"
			 authors
			 title
			 publisher
			 year
			 burl)))

    (with-current-buffer (find-file-noselect "~/wiki/references.bib")
      (goto-char (point-max))
      (insert "\n\n")
      (insert bibtex)
      (org-ref-clean-bibtex-entry)
      (save-buffer))))
