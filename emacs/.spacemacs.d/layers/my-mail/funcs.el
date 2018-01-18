;;; funcs.el --- my-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2017 Leonard Lausen
;;
;; Author:  <leonard@lausen.nl>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun leezu//notmuch-inbox-p (saved-search-property-item)
  "Returns non-nil if item is the inbox."
  (string-equal (plist-get saved-search-property-item :name) "inbox"))

(defun leezu/notmuch-inbox ()
  "Search inbox."
  (interactive)
  (notmuch-search
   (plist-get (nth 0 (-filter 'leezu//notmuch-inbox-p notmuch-saved-searches))
              :query)))


;;
;; Add delete shortcuts
;;

;; Show mode
(defun leezu/notmuch-search-delete-thread (&optional undelete beg end)
  "Delete the currently selected thread or region.

Delete each message in the currently selected thread by applying
the tag changes in `notmuch-delete-tags' to each (remove the
\"inbox\" tag by default). If a prefix argument is given, the
messages will be \"undeleted\" (i.e. the tag changes in
`notmuch-delete-tags' will be reversed).

This function advances the next thread when finished."
  (interactive (cons current-prefix-arg (notmuch-search-interactive-region)))
  (when notmuch-delete-tags
    (notmuch-search-tag
     (notmuch-tag-change-list notmuch-delete-tags undelete) beg end))
  (when (eq beg end)
    (notmuch-search-next-thread)))

;; Tree mode

(defun leezu/notmuch-tree-delete-thread (&optional undelete)
  "Delete each message in thread.

Delete each message currently shown by applying the tag changes
in `notmuch-delete-tags' to each. If a prefix argument is given,
the messages will be \"undeleted\", i.e. the tag changes in
`notmuch-delete-tags' will be reversed.

Note: This command is safe from any race condition of new messages
being delivered to the same thread. It does not delete the
entire thread, but only the messages shown in the current
buffer."
  (interactive "P")
  (when notmuch-delete-tags
    (notmuch-tree-tag-thread
     (notmuch-tag-change-list notmuch-delete-tags undelete))))

(defun leezu/notmuch-tree-delete-message-then-next (&optional undelete)
  "Delete the current message and move to next matching message."
  (interactive "P")
  (leezu/notmuch-tree-delete-message undelete)
  (notmuch-tree-next-matching-message))

(defun leezu/notmuch-tree-delete-message (&optional undelete)
  "Delete the current message.

Delete the current message by applying the tag changes in
`notmuch-delete-tags' to it. If a prefix argument is given, the
message will be \"undeleted\", i.e. the tag changes in
`notmuch-delete-tags' will be reversed."
  (interactive "P")
  (when notmuch-delete-tags
    (notmuch-tree-tag (notmuch-tag-change-list notmuch-delete-tags undelete))))

;; Show mode

(put 'leezu/notmuch-show-delete-thread 'notmuch-prefix-doc
     "Un-delete each message in thread.")
(defun leezu/notmuch-show-delete-thread (&optional undelete)
  "Delete each message in thread.

Delete each message currently shown by applying the tag changes
in `notmuch-delete-tags' to each. If a prefix argument is given,
the messages will be \"undeleted\", i.e. the tag changes in
`notmuch-delete-tags' will be reversed.

Note: This command is safe from any race condition of new messages
being delivered to the same thread. It does not delete the
entire thread, but only the messages shown in the current
buffer."
  (interactive "P")
  (when notmuch-delete-tags
    (notmuch-show-tag-all
     (notmuch-tag-change-list notmuch-delete-tags undelete))))

(defun leezu/notmuch-show-delete-thread-then-next ()
  "Delete all messages in the current buffer, then show next thread from search."
  (interactive)
  (leezu/notmuch-show-delete-thread)
  (notmuch-show-next-thread t))

(defun leezu/notmuch-show-delete-thread-then-exit ()
  "Delete all messages in the current buffer, then exit back to search results."
  (interactive)
  (leezu/notmuch-show-delete-thread)
  (notmuch-show-next-thread))

(put 'leezu/notmuch-show-delete-message 'notmuch-prefix-doc
     "Un-delete the current message.")
(defun leezu/notmuch-show-delete-message (&optional undelete)
  "Delete the current message.

Delete the current message by applying the tag changes in
`notmuch-delete-tags' to it. If a prefix argument is given, the
message will be \"undeleted\", i.e. the tag changes in
`notmuch-delete-tags' will be reversed."
  (interactive "P")
  (when notmuch-delete-tags
    (apply 'notmuch-show-tag-message
	   (notmuch-tag-change-list notmuch-delete-tags undelete))))

(defun leezu/notmuch-show-delete-message-then-next-or-exit ()
  "Delete the current message, then show the next open message in the current thread.

If at the last open message in the current thread, then exit back
to search results."
  (interactive)
  (leezu/notmuch-show-delete-message)
  (notmuch-show-next-open-message t))

(defun leezu/notmuch-show-delete-message-then-next-or-next-thread ()
  "Delete the current message, then show the next open message in the current thread.

If at the last open message in the current thread, then show next
thread from search."
  (interactive)
  (leezu/notmuch-show-delete-message)
  (unless (notmuch-show-next-open-message)
    (notmuch-show-next-thread t)))

;;
;; Add spam shortcuts
;;

;; Show mode
(defun leezu/notmuch-search-spam-thread (&optional undelete beg end)
  "Spam the currently selected thread or region.

Spam each message in the currently selected thread by applying
the tag changes in `notmuch-spam-tags' to each (remove the
\"inbox\" tag by default). If a prefix argument is given, the
messages will be \"unspamd\" (i.e. the tag changes in
`notmuch-spam-tags' will be reversed).

This function advances the next thread when finished."
  (interactive (cons current-prefix-arg (notmuch-search-interactive-region)))
  (when notmuch-spam-tags
    (notmuch-search-tag
     (notmuch-tag-change-list notmuch-spam-tags undelete) beg end))
  (when (eq beg end)
    (notmuch-search-next-thread)))

;; Tree mode

(defun leezu/notmuch-tree-spam-thread (&optional undelete)
  "Spam each message in thread.

Spam each message currently shown by applying the tag changes
in `notmuch-spam-tags' to each. If a prefix argument is given,
the messages will be \"unspamd\", i.e. the tag changes in
`notmuch-spam-tags' will be reversed.

Note: This command is safe from any race condition of new messages
being delivered to the same thread. It does not spam the
entire thread, but only the messages shown in the current
buffer."
  (interactive "P")
  (when notmuch-spam-tags
    (notmuch-tree-tag-thread
     (notmuch-tag-change-list notmuch-spam-tags undelete))))

(defun leezu/notmuch-tree-spam-message-then-next (&optional undelete)
  "Spam the current message and move to next matching message."
  (interactive "P")
  (leezu/notmuch-tree-spam-message undelete)
  (notmuch-tree-next-matching-message))

(defun leezu/notmuch-tree-spam-message (&optional undelete)
  "Spam the current message.

Spam the current message by applying the tag changes in
`notmuch-spam-tags' to it. If a prefix argument is given, the
message will be \"unspamd\", i.e. the tag changes in
`notmuch-spam-tags' will be reversed."
  (interactive "P")
  (when notmuch-spam-tags
    (notmuch-tree-tag (notmuch-tag-change-list notmuch-spam-tags undelete))))

;; Show mode

(put 'leezu/notmuch-show-spam-thread 'notmuch-prefix-doc
     "Un-spam each message in thread.")
(defun leezu/notmuch-show-spam-thread (&optional undelete)
  "Spam each message in thread.

Spam each message currently shown by applying the tag changes
in `notmuch-spam-tags' to each. If a prefix argument is given,
the messages will be \"unspamd\", i.e. the tag changes in
`notmuch-spam-tags' will be reversed.

Note: This command is safe from any race condition of new messages
being delivered to the same thread. It does not spam the
entire thread, but only the messages shown in the current
buffer."
  (interactive "P")
  (when notmuch-spam-tags
    (notmuch-show-tag-all
     (notmuch-tag-change-list notmuch-spam-tags undelete))))

(defun leezu/notmuch-show-spam-thread-then-next ()
  "Spam all messages in the current buffer, then show next thread from search."
  (interactive)
  (leezu/notmuch-show-spam-thread)
  (notmuch-show-next-thread t))

(defun leezu/notmuch-show-spam-thread-then-exit ()
  "Spam all messages in the current buffer, then exit back to search results."
  (interactive)
  (leezu/notmuch-show-spam-thread)
  (notmuch-show-next-thread))

(put 'leezu/notmuch-show-spam-message 'notmuch-prefix-doc
     "Un-spam the current message.")
(defun leezu/notmuch-show-spam-message (&optional undelete)
  "Spam the current message.

Spam the current message by applying the tag changes in
`notmuch-spam-tags' to it. If a prefix argument is given, the
message will be \"unspamd\", i.e. the tag changes in
`notmuch-spam-tags' will be reversed."
  (interactive "P")
  (when notmuch-spam-tags
    (apply 'notmuch-show-tag-message
	   (notmuch-tag-change-list notmuch-spam-tags undelete))))

(defun leezu/notmuch-show-spam-message-then-next-or-exit ()
  "Spam the current message, then show the next open message in the current thread.

If at the last open message in the current thread, then exit back
to search results."
  (interactive)
  (leezu/notmuch-show-spam-message)
  (notmuch-show-next-open-message t))

(defun leezu/notmuch-show-spam-message-then-next-or-next-thread ()
  "Spam the current message, then show the next open message in the current thread.

If at the last open message in the current thread, then show next
thread from search."
  (interactive)
  (leezu/notmuch-show-spam-message)
  (unless (notmuch-show-next-open-message)
    (notmuch-show-next-thread t)))
