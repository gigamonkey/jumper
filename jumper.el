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

(defvar *jumper-default-jumper-file* "JUMPER")

(defvar *jumper-patterns*
  '(".+"
    "[^ ]+"
    "[^:]+"))

(defun jumper-file-name-on-line ()
  (interactive)
  (let ((eol nil))
    (save-excursion
      (end-of-line)
      (setf eol (point)))
    (dolist (pattern *jumper-patterns*)
      ;;(message "Checking pattern %s" pattern)
      (save-excursion
        (beginning-of-line)
        (when (search-forward-regexp pattern eol t)
          (let ((match (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
            ;;(message "Pattern %s matched: %s" pattern match)
            (let ((name (expand-file-name match)))
              (when (file-exists-p name)
                (return name)))))))))

;(defun jumper-ack-file ()
;  (interactive)
;  (let ((eol nil))
;    (save-excursion
;      (end-of-line)
;      (setf eol (point)))
;    (save-excursion
;      (beginning-of-line)
;      (when (search-forward-regexp "(\\d+):" eol t)
;        (let ((match (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
;          ;;(message "Pattern %s matched: %s" pattern match)
;         (let ((name (expand-file-name match)))
;             (when (file-exists-p name)
;                (return name)))))))))

(defun jump-to-file ()
  (interactive)
  (let ((file (jumper-file-name-on-line)))
    (if file
        (find-file file)
      (message "No file on line."))))

(defvar *jumper-def-files* ())

(defun jumper-add-def-file (file)
  (push file *jumper-def-files*))

(defun jumper-jump-to (file &optional line)
  "Jump to a particular file and, optionally, a particular line."
  (when (file-exists-p file)
    (find-file file)
    (goto-line (or line 1))))

(defun jumper-find-def-in-file (file name)
  (save-current-buffer
    (set-buffer (find-file-noselect file))
    (goto-char 0)
    (let ((case-fold-search nil))
      (search-forward-regexp (concat "^" name "\t")))
    (jumper-line-as-string)))

(defun jumper-def-location (file name)
  (destructuring-bind (name source-file line)
      (split-string (jumper-find-def-in-file file name) "\t")
    (list (expand-file-name source-file (file-name-directory file)) (string-to-number line))))

(defun jumper-line-as-string ()
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (end-of-line)
      (buffer-substring-no-properties start (point)))))

(defun jumper-jump-to-def (name)
  (destructuring-bind (file line) (jumper-def-location (jumper-find-jumper-file) name)
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

(defun jumper-jump-to-symbol ()
  (interactive)
  (jumper-jump-to-def (symbol-name (symbol-at-point))))

;;; Next two functions borrowed from SLIME

(defun jumper-push-definition-stack ()
  "Add point to find-tag-marker-ring."
  (require 'etags)
  (cond
   ((featurep 'xemacs) (push-tag-mark))
   (t (ring-insert find-tag-marker-ring (point-marker)))))

(defun jumper-pop-find-definition-stack ()
  "Pop the edit-definition stack and goto the location."
  (interactive)
  (cond
   ((featurep 'xemacs) (pop-tag-mark nil))
   (t (pop-tag-mark))))

(define-minor-mode jumper-mode
  "Jump to definitions."
  nil
  :lighter " jmp"
  :global nil
  :keymap
  (list
   (cons (kbd "M-.") 'jumper-jump-to-symbol)
   (cons (kbd "M-,") 'jumper-pop-find-definition-stack)))
