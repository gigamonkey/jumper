;;; jumper-update.el -- commands for updating jumper defs.
;;;
;;; Copyright (c) 2012, Peter Seibel, Felix Geller
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;     * Redistributions of source code must retain the above
;;;       copyright notice, this list of conditions and the following
;;;       disclaimer.
;;;
;;;     * Redistributions in binary form must reproduce the above
;;;       copyright notice, this list of conditions and the following
;;;       disclaimer in the documentation and/or other materials
;;;       provided with the distribution.
;;;
;;;     * Neither the name of Peter Seibel nor the names of its
;;;       contributors may be used to endorse or promote products
;;;       derived from this software without specific prior written
;;;       permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
;;; TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
;;; THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;
;;; Usage:
;;;
;;; Use `jumper-update-defs-from-current-buffer' to update your jumper
;;; file with defs found in the current buffer. For example:
;;;
;;;     (add-hook 'before-save-hook 'jumper-update-defs-from-current-buffer)
;;;
;;; This way your defs are updated automatically whenever you save a file.
;;;
;;; The `*jumper-update-mode-to-def-regex-list*' holds a mapping from
;;; major-mode names to lists of pairs of a regular expression and the
;;; number of the def's group in this regular expression. It is used
;;; to identify defs in the current buffer and should be extended as
;;; desired.
;;;


(defvar *jumper-update-debug* nil)
(setq *jumper-update-debug* nil)


(defvar *jumper-update-mode-to-def-regex-list* ())
(setq *jumper-update-mode-to-def-regex-list*
      '(
        (python-mode . (("^[[:blank:]]*\\(def\\|class\\) \\([_A-Za-z][_A-Za-z1-9]*\\)(.*$" . 2)))
        (js2-mode . (("^[[:blank:]]*Ext.define(.+\\.\\([_A-Za-z][^'\"]*\\).*$" . 1)
                     ("^[[:blank:]]*\\([_A-Za-z][_A-Za-z1-9]*\\): function.*$" . 1)))
        (makefile-mode . (("^\\([[:alnum:]{}()_]+\\)::?.*$" . 1)))
        ))


(defvar *jumper-update-excludes* nil)
(setq *jumper-update-excludes* '("/ssh:" "/tmp" "/sudo:" "/scpc:"))


(defun jumper-update-include-p (file-name)
  (loop for exclude in *jumper-update-excludes* never (string-match exclude file-name)))


(defun jumper-update-log (str)
  (when *jumper-update-debug*
    (message (replace-regexp-in-string "%" "%%" str))))


(defun jumper-update-delete-line ()
  (jumper-update-log (format "Deleting line [%s]" (jumper-update-current-line)))
  (delete-region (point-at-bol) (point-at-eol)))


(defun jumper-update-defify-line (replace-str file-name line-number)
  (jumper-update-log "Replacing a found def and adding location info.")
  (replace-match replace-str nil nil)
  (goto-char (point-at-eol))
  (insert (format "\t%s\t%s" file-name line-number)))


(defun jumper-update-current-line ()
  (buffer-substring (point-at-bol) (point-at-eol)))


(defun jumper-update-defify-or-delete-line (def-regexs file-name line-number)
  (if def-regexs
      (let ((regex (caar def-regexs))
            (replace-str (format "\\%s" (cdar def-regexs))))
        (jumper-update-log (format "Looking for [%s]" regex))
        (if (re-search-forward regex (point-at-eol) t)
            (jumper-update-defify-line replace-str file-name line-number)
          (jumper-update-defify-or-delete-line (cdr def-regexs) file-name line-number)))
    (jumper-update-delete-line)))


(defun jumper-update-strip-buffer-to-defs (def-regexs file-name)
  (let ((line-number 0))
    (goto-char (point-min))
    (while (not (eobp))
      (setq line-number (1+ line-number))
      (goto-char (point-at-bol))
      (jumper-update-log (format "Looking at line [%s]" (jumper-update-current-line)))
      (jumper-update-defify-or-delete-line def-regexs file-name line-number)
      (forward-line 1))
    (goto-char (point-min))
    (flush-lines "^[[:blank:]]*$")))


(defun jumper-update-defs-from-current-buffer ()
  (interactive)
  (let ((file-name (file-truename (buffer-file-name))))
    (when (jumper-update-include-p file-name)
      (let ((buffer-with-defs (current-buffer))
            (def-regexs (cdr (assoc major-mode *jumper-update-mode-to-def-regex-list*)))
            (jumper-file (jumper-find-jumper-file)))
        (when (and def-regexs jumper-file)
          (with-temp-buffer
            (insert-buffer-substring buffer-with-defs)
            (goto-char (point-min))
            (jumper-update-strip-buffer-to-defs def-regexs file-name)
            (jumper-update-log (format "Found defs: \n[%s]" (buffer-substring (point-min) (point-max))))
            (jumper-update-defs-in-jumper-file
             jumper-file
             file-name
             (buffer-substring (point-min) (point-max)))
            ))))))


(defun jumper-update-defs-in-jumper-file (jumper-file file-name defs)
  (set-buffer (find-file-noselect jumper-file))
  (goto-char (point-min))
  (flush-lines file-name)
  (goto-char (point-max))
  (insert defs)
  (save-buffer))


(provide 'jumper-update)
