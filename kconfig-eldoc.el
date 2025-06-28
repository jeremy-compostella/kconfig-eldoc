;;; kconfig-eldoc.el --- Kconfig eldoc interface. -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Created: June 2025
;; Keywords: extensions eldoc
;; Homepage: https://github.com/jeremy-compostella/kconfig-eldoc
;; Package-Version: 1.0
;; Package-Requires: ((emacs "29.4") (magit "3.3.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides `eldoc' integration for Kconfig symbols in C code.
;; It allows Emacs to display the value of Kconfig symbols (defined in the
;; .config file of a project) in the minibuffer as you type, providing
;; immediate feedback on the configuration options.
;;
;; The `kconfig-eldoc' function retrieves the value of the Kconfig symbol
;; at point from the .config file and displays it using `eldoc'.
;; The `kconfig-c-mode-hook' function adds `kconfig-eldoc' to the
;; `eldoc-documentation-functions` hook in `c-mode`, so that it is
;; automatically invoked when `eldoc` is active in C buffers.
;;
;; This package requires `magit' to locate the root of the Git repository
;; and find the .config file.

(require 'magit)

(defcustom kconfig-eldoc-supported-modes
  '(c-mode makefile-mode asm-mode kconfig-mode cscope-mode)
  "List of major modes where `kconfig-eldoc-mode' is active."
  :type '(repeat symbol))

(defun kconfig-at-point ()
  "Return the Kconfig symbol at point, or nil.

This function extracts the symbol under the cursor. It returns
the symbol only if it doesn't already have a face property; this
helps avoid identifying parts of code which shouldn't be Kconfig
symbols."
  (when-let ((symbol (thing-at-point 'symbol)))
    (unless (get-text-property 0 'face symbol)
      symbol)))

(defun kconfig-eldoc-config-file ()
  "Return the path to the \"kconfig\" file.

This function attempts to locate the .config file at the root of
the current Git repository.  It uses `magit-toplevel' to find the
repository root and checks if the .config file exists there.
Returns the full path to the file if found; otherwise, returns
nil."
  (when-let ((top (magit-toplevel)))
    (let ((file (concat top ".config")))
      (when (file-exists-p file)
	file))))

(defvar kconfig-eldoc-buffer-cache nil
  "Cache for the Kconfig buffers.")

(defun kconfig-eldoc-get-buffer ()
  "Retrieve the buffer associated with the Kconfig file located in DIR.

This function checks the cache `kconfig-eldoc-buffer-cache' for
an existing buffer corresponding to the directory DIR. If found,
it returns the cached buffer. If not, it attempts to locate the
Kconfig file using `kconfig-eldoc-config-file', opens it, and
caches the buffer for future use. "
  (if-let ((dir default-directory)
	   (item (find dir kconfig-eldoc-buffer-cache
		       :test (lambda (x y)
			       (when (string-prefix-p (car y) x)
				 (or (not (cdr y))
				     (buffer-live-p (cdr y))))))))
      (cdr item)
    (when-let ((top-level (magit-toplevel)))
      (setf dir top-level))
    (let ((file (kconfig-eldoc-config-file))
	  (elem (cons dir nil)))
      (when file
	(let* ((buffer (find-file-noselect file)))
	  (setcdr elem buffer)))
      (add-to-list 'kconfig-eldoc-buffer-cache elem)
      (cdr elem))))

(defun kconfig-eldoc (callback &rest _ignored)
  "Provide eldoc information for Kconfig symbols found in C code."
  (interactive)
  (when-let ((kconfig (kconfig-at-point)))
    (when-let ((buffer (kconfig-eldoc-get-buffer)))
      (with-current-buffer buffer
	(goto-char (point-min))
	(unless (string-prefix-p "CONFIG_" kconfig)
	  (setf kconfig (concat "CONFIG_" kconfig)))
	(when (re-search-forward (concat kconfig "[ =]") nil t)
	  (let ((value (buffer-substring (point) (line-end-position))))
	    (if callback
		(funcall callback value :thing kconfig
			 :face 'font-lock-variable-name-face)
	      value)))))))

(defun kconfig-eldoc-c-mode-hook ()
  (add-hook 'eldoc-documentation-functions #'kconfig-eldoc nil t))

(define-minor-mode kconfig-eldoc-mode
  "Toggle Kconfig eldoc support.

When enabled, this minor mode provides eldoc information for
Kconfig symbols in C, Makefiles, and Assembly code. It adds
`kconfig-eldoc' to the `eldoc-documentation-functions' hook in
the relevant modes."
  :init-value nil
  :global t
  (cl-flet ((action (var hook local)
	      (if kconfig-eldoc-mode
		  (add-hook var hook nil local)
		(remove-hook var hook local))))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(when (memq major-mode kconfig-eldoc-supported-modes)
	  (action 'eldoc-documentation-functions #'kconfig-eldoc t))))
    (dolist (mode kconfig-eldoc-supported-modes)
      (let* ((hook-name (concat (symbol-name mode) "-hook"))
	     (hook (intern hook-name)))
	(action hook #'kconfig-eldoc-c-mode-hook nil)))))

(provide 'kconfig-eldoc)
