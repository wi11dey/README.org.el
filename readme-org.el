;;; readme-org.el --- README.org from your Elisp -*- lexical-binding: t -*-

;; Author: Will Dey
;; Maintainer: Will Dey
;; Version: 1.0.0
;; Package-Requires: ()
;; Homepage: https://github.com/wi11dey/README.org.el
;; Keywords: keywords

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;; Generate README:
;;; Commentary:

;;;; About
;; Keep your documentation with your code: readme-org.el generates a completely-customizable Org document from the *Commentary* section of your Elisp code. As an example, the file you're reading right now was generated with readme-org.

;; Org is a powerful plain-text . Both [[https://github.com][Github]] and [[https://gitlab.com][GitLab]] automatically render and preview README.org files when viewing projects.

;; The full power of Org can be used in comments, 

;; (see [[Global Mode]])

;;;; Installation
;; Add to your `load-path'

;;;; Usage
;; 
;;;;; Global Mode

;;;; Customization
;; `readme-org-filename'

;; `readme-org-title-format'

;; `readme-org-activation-cookie'

;;;; See also
;; - [[https://github.com/thomas11/md-readme][md-readme]] - To generate Markdown READMEs instead of Org

;;; Code:

(defgroup readme-org nil
  "Generate README.org from Elisp files."
  :group 'lisp
  :tag "README.org")

(defun readme-org-filename-safe-p (filename)
  "Return non-nil if FILENAME is a safe local value a file can set for `readme-org-filename'.

File paths are unsafe since they could potentially allow the file to overwrite files outside of its directory with the editing user's permissions. Therefore, only relative file names within the same directory are considered safe."
  (and (not (file-name-absolute-p filename))
       (equal (file-name-directory (expand-file-name filename default-directory))
	      (file-name-as-directory default-directory))))

(defcustom readme-org-filename "README.org"
  "Default file to write the generated README.org to."
  :group 'readme-org
  :type 'file
  :safe #'readme-org-filename-safe-p
  :tag "README.org Default Filename")

(defcustom readme-org-title-format "#+title: \\1
/\\2/"
  "Replacement string describing how to format the title in Org. Follows `replace-match' syntax, so a \\1 in the format will be replaced with the filename and a \\2 will be replaced with the description.

For example, a format like \"#+title: \\1, \\2\" will transform a header line like

    ;;; FILENAME.EL --- DESCRIPTION -*- file-local variables -*-

to 

    #+title: FILENAME.EL, DESCRIPTION"
  :group 'readme-org
  :type 'string
  :safe #'stringp
  :tag "README.org Title Format")

(defcustom readme-org-activation-cookie ";; Generate README:\\s-*\\(?1:.+\\)?
;;; Commentary:"
  "Regexp to search for when `readme-org-mode' on to determine which files to generate a README for. If a group is captured by the regexp by a \\(\\) pair, it will be used as the README filename, as long as it passes `readme-org-filename-safe-p'."
  :group 'readme-org
  :type 'string
  :tag "README.org Activation Cookie")

(defsubst readme-org--get-filename (&optional readme-filename)
  (substring-no-properties (or readme-filename
			       readme-org-filename)))

(defun readme-org-maybe-generate ()
  "Generate a README for this file iff `readme-org-activation-cookie' is found in the file."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (save-match-data
	(when (re-search-forward readme-org-activation-cookie nil t)
	  (readme-org-generate (and (match-string 1)
				    (if (readme-org-filename-safe-p (match-string 1))
					(match-string 1)
				      (display-warning 'readme-org
						       (format-message "%S is an unsafe file name to write to (set by `%s'). %S will be used instead."
								       (match-string 1)
								       buffer-file-name
								       (readme-org--get-filename))
						       :warning)
				      nil))))))))

(define-minor-mode readme-org-mode
  "Global mode to generate a README for all files containing `readme-org-activation-cookie' on save.

With a prefix argument ARG, enable README.org mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is ‘toggle’."
  :group 'readme-org
  :lighter " README.org"
  :global t
  ;;;; Teardown
  (remove-hook 'after-save-hook #'readme-org-maybe-generate)
  (when readme-org-mode
    ;;;; Construction
    (add-hook 'after-save-hook #'readme-org-maybe-generate)))

(defun readme-org--convert-keyword (keyword &optional org-keyword start)
  (unless org-keyword
    (setq org-keyword (downcase keyword)))
  (goto-char (point-min))
  (while (looking-at "#+")
    (forward-line))
  (let ((append-start (point))
	value)
    (when (re-search-forward (format "^;; %s: "
				     (regexp-quote keyword))
			     nil
			     t)
      (setq value (buffer-substring-no-properties (point) (line-end-position)))
      (delete-region (line-beginning-position) (line-end-position))
      (goto-char append-start)
      (insert "#+" org-keyword ": " value "\n"))))

(defun readme-org-generate (&optional readme-filename)
  "Generate an Org-formatted README for the current buffer. If README-FILENAME is non-nil it is used as the filename to save the README in. Otherwise, the value of `readme-org-filename' is used."
  (interactive (list (read-file-name "Readme file: "
                                     nil
				     nil
				     nil
				     (readme-org--get-filename))))
  (save-excursion
    (let (header-end
	  org-end
	  (original-buffer (current-buffer))
	  (title-format readme-org-title-format) ; Capture the potentially buffer-local `readme-org-title-format' before switching buffers.
	  )
      (save-restriction
	(widen)
	(goto-char (point-min))
	(save-match-data
	  (while (or (looking-at "\n")
		     (looking-at ";;"))
	    (forward-line))
	  (setq header-end (point))
	  (with-temp-file (readme-org--get-filename readme-filename)
	    (insert-buffer-substring-no-properties original-buffer
						   1
						   header-end)
	    (setq org-end (point-min-marker))
	    (set-marker-insertion-type org-end t)
	    (goto-char org-end)
	    ;;;; Title
	    (when (looking-at ";;; \\(?1:.*?\\) --- \\(?2:.*?\\)\\(?:\\s-*-\\*-.*-\\*-\\s-*\\|$\\)")
              (replace-match title-format
			     :fixedcase)
	      (insert ?\n)
	      (set-marker org-end (point)))
	    ;;;; Author
	    (readme-org--convert-keyword "Author")
	    ;;;; Commentary
	    (when (re-search-forward "^;;; Commentary:")
	      (forward-line)
	      (delete-region org-end (point))
	      (while (not (looking-at "^;;; Code:"))
		(when (looking-at "\\(?1:;;+\\)\\s-?")
		  (if (>= (length (match-string 0)) 4)
		      (replace-match (concat (make-string (- (length (match-string 1)) 3) ?*)
					     " ")
				     :fixedcase
				     :literal)
		    (replace-match ""
				   :fixedcase
				   :literal)))
		(forward-line))
	      (set-marker org-end (point)))
	    ;;;; Clean
	    (delete-region org-end (point-max))
	    (goto-char (point-max))
	    (when (re-search-backward "." nil t)
	      (forward-line)
	      (delete-region (point) (point-max)))
	    ;;;; Monospace text
	    (goto-char (point-min))
	    (while (re-search-forward "`\\(?1:.*?\\)'" nil :noerror)
	      (replace-match "~\\1~"
			     :fixedcase)))
	  (message "Generated %s from %s"
		   (readme-org--get-filename readme-filename)
		   buffer-file-name))))))

(provide 'readme-org)

;;; readme-org.el ends here
