;;; easi-presenter.el --- Present (collections of) results to user  -*- lexical-binding: t; -*-

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

;; This module contains code for presenting collections of results and
;; details about individual results, to the user in EASI.

;;; Code:

(require 'cl-lib)
(require 'easi-result)

;;;; Customizables

(defcustom easi-default-results-presenters nil
  "List of results presenters which are always available.

These are assumed to be compatible with every searchable on the
machine. Presenters which are only compatible with some engines
should be set in the \"results-presenters\" slot of those
engines."
  :group 'easi)

(defcustom easi-default-result-presenters nil
  "List of result presenters which are always available.

These are assumed to be compatible with every searchable on the
machine. Presenters which are only compatible with some engines
should be set in the \"result-presenters\" slot of those
engines.")

;;;; Presenter types

;; TODO These need to be of the same type, or class, or whatever, so
;; that it is possible to define a results printer which is also a
;; result printer (this will enable e.g. three-column file layouts)
(cl-defstruct (easi-results-presenter
	       (:constructor easi-results-presenter-create))
  name key
  ;; ALL the following are hooks, passed two args: the results list, a
  ;; buffer to display results in (if they want).
  before ;; Only run once
  result-printer
  after ;; Only run once

  ;; For getting current result. Should return a result object (i.e.
  ;; not necessarily a string)
  current-result-getter
  hook)

(cl-defstruct (easi-result-presenter
	       (:constructor easi-result-presenter-create))
  name key
  ;; ALL the following are hooks, passed two args: the results list, a
  ;; buffer to display results in (if they want).
  before ;; Only run once
  field-printer
  after ;; Only run once

  ;; For getting current field. Should return a result object (i.e.
  ;; not necessarily a string)
  current-field-getter
  ;; ACTION arg to pass to `display-buffer'
  display-action
  hook)

;;;; Presenters
;;;;; tabulated list result presenter

(defgroup easi-result-list nil
  "Group for EASI result presenter based on `tabulated-list-mode'."
  :group 'easi)

(defcustom easi-result-list-exlude-fields nil
  "List of fields to exlude.

At the moment this is just a list of strings."
  :group 'easi-result-list
  :type '(repeat string))

;; TODO Enforce major mode conventions (see info node (elisp)Major
;; Mode Conventions)
(define-derived-mode easi-result-list-mode tabulated-list-mode "easi-result-list-mode"
  "Major mode for viewing an EASI result in a tabulated list,
presenting columns of field names and values. Derived from
‘tabulated-list-mode’."
  :group 'easi-result-list
  ;; NOTE This isn't really needed, because this mode is always (?)
  ;; called from a freshly created buffer. But calling it first is one
  ;; of the Emacs major mode conventions.
  (kill-all-local-variables)
  ;; Setup format
  (setq tabulated-list-format [("Field" 30 t)
			       ("Value" 30 t)]))

(defun easi-result-list--result-to-tabulated-list (result)
  "Convert an EASI result for `tabulated-list-mode'.

Uses `easi-result-list-fields' to get a list of fields. Return a
list, suitable as a value for `tabulated-list-entries', with each
element having the field string as it's ID and DESC1, and the
value of that field as DESC2."
  (cl-loop for field in (easi-result-list-fields result)
	   unless (cl-member
		   field easi-result-list-exlude-fields :test #'string=)
	   collect
	   `(,field
	     [,field
	      (,(let ((val (easi-result-get-field field result)))
		  (cond
		   ((stringp val) val)
		   ((prin1-to-string val))
		   (t ""))))])
	   ;; End unless
	   end))

;; (defun easi-result-list--mode-setup (result _buffer)
;;   "Initialisation function for `easi-result-list-mode'.

;; - run `easi-result-list-mode'
;; - run `tabulated-list-init-header'"
;;   (easi-result-list-mode)
;;   (setq tabulated-list-entries
;; 	(easi-result-list--result-to-tabulated-list result))
;;   (tabulated-list-init-header))

;; (defun easi-result-list--print (result _buffer)
;;   ""
;;   (tabulated-list-print 'remember-pos))

(defun easi-result-list--mode-setup (result _buffer)
  "Initialisation function for `easi-result-list-mode'.

- run `easi-result-list-mode'
- run `tabulated-list-init-header'"
  (easi-result-list-mode)
  (tabulated-list-init-header))

(defun easi-result-list--print (result _buffer)
  "Set entries and print.

Set `tabulated-list-entries' on the basis of RESULT, then call
`tabulated-list-print'."
  ;; NOTE Unlike `easi-results-list--print', the result presenter has
  ;; to set `tabulated-list-entries' again each time it prints (rather
  ;; than in the setup). This is because the setup function is only
  ;; run once, when the buffers are initially setup, but the result
  ;; buffer needs to display different information each time a new
  ;; result is selected.
  (setq tabulated-list-entries
	(easi-result-list--result-to-tabulated-list result))
  (tabulated-list-print 'remember-pos))

(defvar easi-result-list-presenter
  (easi-result-presenter-create
   :name "Tabulated list result presenter"
   :key "t"
   :before '(easi-result-list--mode-setup)
   :field-printer '(easi-result-list--print)
   :current-field-getter #'tabulated-list-get-id
   :display-action '(display-buffer-at-bottom
		     . ((inhibit-same-window . t) ;; Don't reuse same window
			(window-height . 20)))) ;; 20 lines high
  "Result presenter based on `tabulated-list-mode'.")

(provide 'easi-presenter)
;;; easi-presenter.el ends here
