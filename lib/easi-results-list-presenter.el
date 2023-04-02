;;; easi-results-list-presenter.el --- EASI results presenter based on `tabulated-list-mode'  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Hugo Heagren

;; Author: Hugo Heagren <hugo@undertown>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This modules provides a results presenter for EASI results, based
;; on `tabulated-list-mode'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'tabulated-list)
(require 'easi)
(require 'easi-presenter)
(require 'easi-result)

(defgroup easi-results-list nil
  "Group for EASI results presenter based on `tabulated-list-mode'."
  :group 'easi)

(defcustom easi-results-list-fields nil
  "List of fields for `easi-results-list-presenter' to display.

Each element is of the form (FIELD WIDTH SORT).

FIELD is a string, the name of the field from each result to
include.

WIDTH is an integer, the maximum width to print.

If SORT is non-nil, this field can be sorted. It is either
t (indicating to sort alphabetically on strings), or a predicate."
  :group 'easi-results-list
  :type '(repeat (list
		  (string :tag "Field")
		  (integer :tag "Width")
		  (choice :tag "Sort"
		   (const :tag "Sort on strings" t)
		   function))))

(defun easi-results-list--convert-field-list (list)
  "Make easi-results-list field tabulated-list compatible.

If the first element of LIST is a list, return a list of the
first non-list item in LIST, then the cdr of LIST. (i.e. for
'(((foo bar)) baz qux)) return (foo baz qux). Otherwise return LIST.

If this function is mapped over `easi-results-list-fields', the
result is an acceptable value for `tabulated-list-format'."
  (if (listp (car list))
      (cons (car (flatten-list list)) (cdr list))
    list))

(define-derived-mode easi-results-list-mode tabulated-list-mode "easi-results-list-mode"
  "Major mode for viewing EASI results in a tabulated list. Derived
from `tabulated-list-mode'."
  :group 'easi-results-list
  ;; NOTE There is a convention to start new major modes with
  ;; `kill-all-local-variables'. We don't do this here because EASI
  ;; relies heavily on local variables for storing data (and they may
  ;; already be set).
  (setq major-mode 'easi-results-list-mode)
  (setq tabulated-list-format
	(seq--into-vector
	 (mapcar #'easi-results-list--convert-field-list
		 easi-results-list-fields))))

(defun easi-results-list-mode-next (&optional arg)
  "Move to next entry in results list.

If prefix ARG is specified, move ARG entries forward."
  (interactive "P")
  (forward-line arg)
  (easi--update-result))

(defun easi-results-list-mode-previous (&optional arg)
  "Move to previous entry in results list.

If prefix ARG is specified, move ARG entries backward."
  (interactive "P")
  (forward-line (- (or arg 1)))
  (easi--update-result))

(let ((map easi-results-list-mode-map))
  (keymap-set map "n" #'easi-results-list-mode-next)
  (keymap-set map "p" #'easi-results-list-mode-previous))

(defun easi-results-list--results-to-tabulated-list (results)
  "Convert a list of EASI results for `tabulated-list-mode'.

RESULTS should be a list of results EASI will recognise. Return a
list of entries which `tabulated-list-mode' will recognise and be
able to print."
  (cl-loop for res in results
	   when res
	   collect
	   ;; Using the result object as the id allows us to use
	   ;; `tabulated-list-get-id' to get the current result
	   `(,res
	     ,(seq--into-vector
	       (mapcar (lambda (field) (or (easi-result-get-field (car field) res)
				      ;; Use an empty string, for
				      ;; cases without a field value
				      ""))
		       easi-results-list-fields)))))

(defun easi-results-list--mode-setup (results _buffer)
  "Initialisation function for `easi-results-list-mode'.

- Turn on `easi-results-list-mode' in the current buffer
- convert RESULTS using
  `easi-results-list--results-to-tabulated-list', and
  `tabulated-list-entries' to the return value
- evaluate `tabulated-list-init-header'"
  (easi-results-list-mode)
  (setq tabulated-list-entries
	(easi-results-list--results-to-tabulated-list results))
  (tabulated-list-init-header))

(defun easi-results-list--print (_results _buffer)
  "Call `tabulated-list-print' with non-nil REMEMBER-POS."
  (tabulated-list-print 'remember-pos))

;;;###autoload
(defvar easi-results-list-presenter
  (easi-results-presenter-create
   :name "Tabulated list presenter"
   :key "l"
   :before '(easi-results-list--mode-setup)
   :result-printer '(easi-results-list--print)
   :current-result-getter #'tabulated-list-get-id)
  "Results presenter based on `tabulated-list-mode'.")

(provide 'easi-results-list-presenter)
;;; easi-results-list-presenter.el ends here
