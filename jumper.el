;;; jumper.el -- commands for jumping to source files.
;;;
;;; Copyright (c) 2012, Peter Seibel
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

;;; TODO: do something smart when there are multiple definitions for a
;;; name.


;; To get the minor mode for whatever kinds of files you've generated
;; JUMPER files for do something like:
;;
;;   (add-hook 'ruby-mode-hook 'jumper-mode)
;;
;; in your .emacs.

(require 'cl)
(require 'etags)

(defvar *jumper-default-jumper-file* "JUMPER")

(defvar *jumper-patterns* ())

(setq *jumper-patterns*
  '("\\(.+\\)"
    "[[:blank:]]*\\([^ ]+\\)"
    "[[:blank:]]*\\([^:]+\\):\\([[:digit:]]+\\)"
    "at \\([^ ]+\\) line \\([[:digit:]]+\\)[,.]" ;; Perl error message
))

(defun jumper-jump-to-symbol ()
  "Jump to the definition of the symbol at the point."
  (interactive)
  (jumper-jump-to-def (symbol-name (symbol-at-point))))

(defun jumper-jump-to-file ()
  "Jump to the file we find named somewhere on the line by
matching our *jumper-patterns* against the whole line."
  (interactive)
  (let ((line (jumper-line-as-string))
        (found nil))
    (dolist (pattern *jumper-patterns*)
      (when (string-match pattern line)
        (let ((name (expand-file-name (match-string 1 line)))
              (line-num (match-string 2 line)))
          (when (file-exists-p name)
            (setq found t)
            (message "Found %s %s with %s" name line-num pattern)
            (jumper-jump-to name (if line-num (string-to-number line-num) nil))))))
    (unless found
      (message "No file on line."))))


(defun jumper-jump-to (file &optional line)
  "Jump to a particular file and, optionally, a particular line."
  (cond
   ((file-exists-p file)
    (find-file file)
    (when line (goto-line line)))
   (t (message "No file: %s" file))))

(defun jumper-find-def-in-file (file name)
  (save-current-buffer
    (set-buffer (find-file-noselect file))
    (goto-char 0)
    (let ((case-fold-search nil))
      (search-forward-regexp (concat "^" name "\t")))
    (destructuring-bind (name source-file line)
        (split-string (jumper-line-as-string) "\t")
      (list
       (expand-file-name source-file (file-name-directory file))
       (string-to-number line)))))

(defun jumper-line-as-string ()
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (end-of-line)
      (buffer-substring-no-properties start (point)))))

(defun jumper-jump-to-def (name)
  (destructuring-bind (file line) (jumper-find-def-in-file (jumper-find-jumper-file) name)
    (jumper-push-definition-stack)
    (jumper-jump-to file line)))

(defun jumper-up-dir (d)
  (file-name-directory (directory-file-name d)))

(defun jumper-find-jumper-file ()
  (jumper-%find-jumper-file (buffer-file-name)))

(defun jumper-%find-jumper-file (d)
  (let ((filename (expand-file-name *jumper-default-jumper-file* d)))
    (cond
     ((jumper-is-file-p filename) filename)
     ((string-equal d "/") nil)
     (t (jumper-%find-jumper-file (jumper-up-dir d))))))

(defun jumper-is-file-p (f)
  (and (file-exists-p f)
       (not (file-exists-p (file-name-as-directory f)))))

;;; Next two functions based on code from from SLIME

(defun jumper-push-definition-stack ()
  "Add point to find-tag-marker-ring."
  (cond
   ((featurep 'xemacs) (push-tag-mark))
   (t (ring-insert find-tag-marker-ring (point-marker)))))

(defun jumper-pop-definition-stack ()
  "Pop the edit-definition stack and goto the location."
  (interactive)
  (cond
   ((featurep 'xemacs) (pop-tag-mark nil))
   (t (pop-tag-mark))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define a minor mode that can be used whenever we have JUMPER files
;;; built.

(define-minor-mode jumper-mode
  "Jump to definitions."
  nil
  :lighter " jmp"
  :global nil
  :keymap
  (list
   (cons (kbd "M-.") 'jumper-jump-to-symbol)
   (cons (kbd "M-,") 'jumper-pop-definition-stack)))

(provide 'jumper)