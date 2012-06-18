
(defvar *jumper-update-mode-to-def-regex-list* ())
(setq *jumper-update-mode-to-def-regex-list*
      '(
	(python-mode . "^[	]*\\(def\\|class\\) \\([_A-Za-z][_A-Za-z1-9]*\\)(.*")
	))


(defvar *jumper-update-line-extra-info-regex* "")
(setq *jumper-update-line-extra-info-regex* "\\(	.+	[0-9]+\\)$")


(defun jumper-update-add-lines-and-file-names ()
  (let ((line-number 1))
    (while (not (eobp))
      (end-of-line)
      (insert (format "	%s	%s" file-name line-number))
      (setq line-number (1+ line-number))
      (forward-line))))


(defun jumper-update-defs-from-current-buffer ()
  (interactive)
  (let* ((file-name (buffer-file-name))
	 (buffer-with-defs (current-buffer))
	 (def-regex (cdr (assoc major-mode *jumper-update-mode-to-def-regex-list*)))
	 (line-regex (concat def-regex *jumper-update-line-extra-info-regex*))
	 (jumper-file (jumper-find-jumper-file)))
    (when (and def-regex jumper-file)
      (with-temp-buffer
	(insert-buffer-substring buffer-with-defs)
	(beginning-of-buffer)
	(save-excursion (jumper-update-add-lines-and-file-names))
	(save-excursion (keep-lines line-regex))
	(message (buffer-substring (point-min) (point-max)))
	(while (re-search-forward line-regex nil t)
	  (replace-match "\\2\\3" nil nil))
	(message (buffer-substring (point-min) (point-max)))
	(jumper-update-defs-in-jumper-file
	 jumper-file
	 file-name
	 (buffer-substring (point-min) (point-max)))
	))))


(defun jumper-update-defs-in-jumper-file (jumper-file file-name defs)
  (set-buffer (find-file-noselect jumper-file))
  (beginning-of-buffer)
  (flush-lines file-name)
  (end-of-buffer)
  (insert defs)
  (save-buffer)
  )


(provide 'jumper-update)
