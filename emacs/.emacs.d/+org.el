;; Org mode enhancements
(use-package org
  :config
  (setq org-directory "~/org/")

  ; keybindings
  (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)

  ;; Open files in same frame
  (setf (alist-get 'file org-link-frame-setup) 'find-file)

  ;; org-id
  ; Ensure that each captured entry gets a unique ID
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
        ; Allow refile to create parent tasks with confirmation
        org-refile-allow-creating-parent-nodes 'confirm
        )
  ; Exclude completed tasks from refile targets (https://michael.englehorn.com/config.html)
  (defun bh/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  (setq org-refile-target-verify-function 'bh/verify-refile-target)

  ;; agenda
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
	)
  )

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/wiki"))

  :config
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

  (setq org-roam-dailies-directory "local/dailies"
        find-file-visit-truename nil  ;; Support ~/wiki/local symlink
        org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>.org"
                              "#+TITLE: ${title}\nOPENED: %U\n%i\n")
           :unnarrowed t)
          ; TODO: "file+head+datetree" is not available, so if the file for
          ; datetree does not yet exist, the #+TITLE will not be prefilled. Need
          ; to create new node first and then trigger the 1:1 entry
          ("o" "1:1" plain "%?"
           :target (file+datetree "local/%<%Y%m%d%H%M%S>.org" "day")
           :unnarrowed t)
          ("O" "1:1" plain "%?"
           :target (file+datetree "local/%<%Y%m%d%H%M%S>.org" "day")
           :unnarrowed t :time-prompt t)
          ("l" "local" plain "%?"
           :target (file+head "local/%<%Y%m%d%H%M%S>.org"
                              "#+TITLE: ${title}\nOPENED: %U\n%i\n")
           :unnarrowed t)))

  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  (my/leader-keys
    "n r l" 'org-roam-buffer-toggle
    "n r f" 'org-roam-node-find
    "n r g" 'org-roam-graph
    "n r i" 'org-roam-node-insert
    "n r c" 'org-roam-capture
    "n r j" 'org-roam-dailies-capture-today))


;(require 'citar-org-roam)
;(citar-org-roam-mode)
(use-package citar
  :config
  (setq  org-cite-global-bibliography '("~/wiki/references.bib" "~/wiki/references-stable.bib")
	 org-cite-insert-processor 'citar
	 org-cite-follow-processor 'citar
	 org-cite-activate-processor 'citar
	 citar-bibliography org-cite-global-bibliography
	 citar-library-paths '("~/library/")
	 citar-notes-paths '("~/wiki/")
	 citar-org-roam-note-title-template "Notes on \"${title}\""
	 citar-file-open-functions '(("html" . citar-file-open-external)
				     ("pdf" . find-file)
				     (t . find-file))
	 bibtex-dialect 'biblatex)
  (my/leader-keys
    "o b c" 'citar-open
    "o b n" 'citar-open-notes
    "o b f" 'citar-open-files
    "o b F" 'citar-add-file-to-library
    ))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (setq pdf-view-use-scaling t
      pdf-view-use-imagemagick nil)
  )

(use-package visual-fill-column
  :ensure t
  :hook (org-mode . visual-fill-column-mode)
  :config
  (setq-default visual-fill-column-width 100
		visual-fill-column-center-text t))

(use-package writeroom-mode
  :ensure t)
