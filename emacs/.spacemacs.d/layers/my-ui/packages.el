(defconst my-ui-packages '(spaceline))

(defun my-ui/post-init-spaceline ()
  (setq spaceline-minor-modes-p nil
        spaceline-hud-p nil
        spaceline-purpose-p nil
        spaceline-buffer-position-p nil
        spaceline-buffer-encoding-abbrev-p nil
        spaceline-buffer-size-p nil
        spaceline-org-clock-p t
        spaceline-org-pomodoro-p t))
