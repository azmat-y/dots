
(defvar az/temp-org-file "/tmp/temp-notes.org"
  "Path to the temporary Org file.")

(defun az/open-temp-org-file ()
  "Open a temporary Org file in /tmp in a new frame. Save it to ~/Org/ if needed."
  (interactive "B")
  (select-frame (make-frame)) ;; Create and switch to a new frame
  (find-file az/temp-org-file)
  (org-mode)
  (message "Opened temporary Org file in new frame. Use `M-x az/save-temp-org-file` to keep it."))

(defun az/save-temp-org-file (filename)
  "Prompt for a filename and save the temporary Org file to ~/Org/."
  (interactive
   (list (read-string "Save as (in ~/Org/): "
                      (format-time-string "notes-%Y%m%d-%H%M%S.org"))))
  (let ((new-path (expand-file-name filename "~/Org/")))
    (copy-file az/temp-org-file new-path t)
    (message "Saved to: %s" new-path)))

(defun az/org-create-backlog-heading-and-link ()
  "Create a new top-level heading with a unique ID, and insert a link
to it in the current Org table cell (Backlog column)."
  (interactive)
  (unless (org-at-table-p)
    (error "Not inside an Org table"))

  ;; Prompt for title
  (let* ((title (read-string "Task title: "))
         (id (org-id-new))
         (headline (format "* BACKLOG %s\n:PROPERTIES:\n:ID: %s\n:END:\n" title id))
         (link (format "[[id:%s][%s]]" id title)))

    ;; Insert heading at end of file
    (save-excursion
      (goto-char (point-max))
      (insert "\n" headline))

    ;; Replace current cell with link
    (org-table-blank-field)
    (insert link)
    (org-table-align)))

(provide 'utility)
