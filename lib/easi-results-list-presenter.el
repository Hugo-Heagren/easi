;;; easi-results-list-presenter.el --- EASI results presenter based on `tabulated-list-mode'  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Hugo Heagren

;; Author: Hugo Heagren <hugo@heagren.com>

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
(require 'tabulated-list)
(require 'easi)
(require 'easi-presenter)
(require 'easi-result)

(defgroup easi-results-list nil
  "Group for EASI results presenter based on `tabulated-list-mode'."
  :group 'easi)

(defcustom easi-results-list-fields nil
  "Plist of fields for `easi-results-list-presenter' to display.

`easi-results-list-presenter' gets it's value for
`tabulated-list-format' by transforming this variable. Each element
represents a column and is a plist with the following keys:
- `:name' the title of the column. Must be a string. Used as NAME in
  `tabulated-list-format'.
- `:getter' the value of this key is used to get the value to display
  in the column for each result. The value of `:getter' is passed as
  FIELD, and the result as OBJECT to `easi-result--get-field'.
- `:width' maximum width to print. Used as WIDTH in
  `tabulated-list-format'.
- `:sort' if non-nil, this field can be sorted. It is either t
  (indicating to sort alphabetically on strings), or a predicate. Used
  as WIDTH in `tabulated-list-format'.
- `:props' used as PROPS in `tabulated-list-format' without
  alteration."
  :group 'easi-results-list
  :type '(repeat plist))

(defun easi-results-list--convert-field-list (list)
  "Make `easi-results-list' field compatible with tabulated-list.

Extract values for properties `:name', `:width', `:sort' and
`:props' from LIST, and return a list of the result values
\\='(NAME WIDTH SORT . PROPS).

If this function is mapped over `easi-results-list-fields', the
result is an acceptable value for `tabulated-list-format'."
  (let ((name (plist-get list :name))
	(width (plist-get list :width))
	(sort (plist-get list :sort))
	(props (plist-get list :props)))
    `(,name ,width ,sort . ,props)))

(define-derived-mode easi-results-list-mode tabulated-list-mode "easi-results-list-mode"
  "Major mode for viewing EASI results in a tabulated list.
Derived from `tabulated-list-mode'."
  :group 'easi-results-list
  ;; NOTE There is a convention to start new major modes with
  ;; `kill-all-local-variables'. We don't do this here because EASI
  ;; relies heavily on local variables for storing data (and they may
  ;; already be set).
  (setq major-mode 'easi-results-list-mode))

(defun easi-results-list--set-format (_results session)
  (let* ((presenter (alist-get (current-buffer)
			       (easi-session-state-buffer-presenters session)))
	 (fields (slot-value presenter 'fields))
	 (format (vconcat
		  (mapcar #'easi-results-list--convert-field-list
			  fields))))
    (setq tabulated-list-format format)
    (tabulated-list-init-header)))

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

(defun easi-results-list--results-to-tabulated-list (results session)
  "Convert a list of EASI results for `tabulated-list-mode'.

RESULTS should be a list of results EASI will recognise. Return a
list of entries which `tabulated-list-mode' will recognise and be
able to print."
  (let* ((presenter (alist-get (current-buffer)
			       (easi-session-state-buffer-presenters session)))
	 (fields (slot-value presenter 'fields)))
    (cl-loop for res in results
	     when res
	     collect
	     ;; Using the result object as the id allows us to use
	     ;; `tabulated-list-get-id' to get the current result
	     `(,res
	       ,(vconcat
		 (mapcar (lambda (field) (or (easi-result--get-field (plist-get field :getter) res)
					;; Use an empty string, for
					;; cases without a field value
					""))
			 fields))))))

(defun easi-results-list--mode-setup (_results _session)
  "Initialisation function for `easi-results-list-mode'.

Turn on `easi-results-list-mode' in the current buffer "
  (easi-results-list-mode))

(defun easi-results-list--print (results session)
  "Set `tabulated-list-entries' and call `tabulated-list-print'.

`tabulated-list-entries' is set to the results of passing RESULTS
to `easi-results-list--results-to-tabulated-list'.
`tabulated-list-print' is then called with non-nil REMEMBER-POS."
  (setq tabulated-list-entries
	(easi-results-list--results-to-tabulated-list results session))
  (tabulated-list-print 'remember-pos))

;; TODO Build a version of this like `easi-result-list-presenter'
;; which displays ALL fields. Make this the default.
(defclass easi-results-list-presenter (easi-presenter)
  ((before :initform '(easi-results-list--mode-setup
		       easi-results-list--set-format
		       ))
   (printer :initform '(easi-results-list--print))
   (current-getter :initform #'tabulated-list-get-id)
   (fields :initarg :fields :initform nil
	   :documentation
	   "Plist of fields for the presenter to display.

The presentation function for this presenter gets it's value for
`tabulated-list-format' by transforming the value of this
slot (with `easi-results-list--convert-field-list'). Each element
represents a column and is a plist with the following keys:
- `:name' the title of the column. Must be a string. Used as NAME in
  `tabulated-list-format'.
- `:getter' the value of this key is used to get the value to display
  in the column for each result. The value of `:getter' is passed as
  FIELD, and the result as OBJECT to `easi-result--get-field'.
- `:width' maximum width to print. Used as WIDTH in
  `tabulated-list-format'.
- `:sort' if non-nil, this field can be sorted. It is either t
  (indicating to sort alphabetically on strings), or a predicate. Used
  as WIDTH in `tabulated-list-format'.
- `:props' used as PROPS in `tabulated-list-format' without
  alteration."))
  "Results presenter based on `tabulated-list-mode'.")

;;;;###autoload
;; XXX NOW USELESS
;; (defvar easi-results-list-presenter
;;   (easi-presenter
;;    :name "Tabulated list presenter"
;;    :key "l"
;;    :before '(easi-results-list--mode-setup easi-results-list--set-format)
;;    :printer '(easi-results-list--print)
;;    :current-getter #'tabulated-list-get-id)
;;   "Results presenter based on `tabulated-list-mode'.")

(provide 'easi-results-list-presenter)
;;; easi-results-list-presenter.el ends here
