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

(defvar *jumper-update-debug* nil)
(setq *jumper-update-debug* nil)


(defvar *jumper-update-mode-to-def-regex-list* ())
(setq *jumper-update-mode-to-def-regex-list*
      '(
	(python-mode . "^[	]*\\(def\\|class\\) \\([_A-Za-z][_A-Za-z1-9]*\\)(.*")
	))


(defun jumper-update-log (str)
  (when *jumper-update-debug*
      (message str)))


(defun jumper-update-strip-buffer-to-defs (def-regex)
  (let ((line-number 0))
    (while (not (eobp))
      (setq line-number (1+ line-number))
      (beginning-of-line)
      (if (re-search-forward def-regex (point-at-eol) t)
	  (progn
	    (replace-match "\\2" nil nil)
	    (end-of-line)
	    (insert (format "	%s	%s" file-name line-number))
	    (forward-line))
	(let ((kill-whole-line t))
	  (kill-line)))
      )))


(defun jumper-update-defs-from-current-buffer ()
  (interactive)
  (let* ((file-name (file-truename (buffer-file-name)))
	 (buffer-with-defs (current-buffer))
	 (def-regex (cdr (assoc major-mode *jumper-update-mode-to-def-regex-list*)))
	 (jumper-file (jumper-find-jumper-file)))
    (when (and def-regex jumper-file)
      (with-temp-buffer
	(insert-buffer-substring buffer-with-defs)
	(beginning-of-buffer)
	(jumper-update-strip-buffer-to-defs def-regex)
	(jumper-update-log (format "Found defs: \n%s" (buffer-substring (point-min) (point-max))))
	(jumper-update-defs-in-jumper-file
	 jumper-file
	 file-name
	 (buffer-substring (point-min) (point-max)))
	))))


(defun jumper-update-defs-in-jumper-file (jumper-file file-name defs)
    (set-buffer (find-file-noselect jumper-file))
    (goto-char (point-min))
    (flush-lines file-name)
    (goto-char (point-max))
    (insert defs)
    (save-buffer))


(provide 'jumper-update)
