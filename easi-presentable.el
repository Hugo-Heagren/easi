;;; easi-presentable.el --- Present (collections of) results to user  -*- lexical-binding: t; -*-

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

;; This module contains code for presenting collections of results and
;; details about individual results, to the user in EASI.

;;; Code:

(require 'eieio)
(require 'easi-utils)
(require 'easi-session)

;;;; Customizables

(defcustom easi-default-results-presenters nil
  "List of results presenters which are always available.

These are assumed to be compatible with every searchable on the
machine. Presenters which are only compatible with some engines
should be set in the \"results-presenters\" slot of those
engines."
  :group 'easi
  :type '(repeat (choice symbol
			 (restricted-sexp
			  :match-alternatives easi-presenter-p))))

(defcustom easi-default-result-presenters nil
  "List of result presenters which are always available.

These are assumed to be compatible with every searchable on the
machine. Presenters which are only compatible with some engines
should be set in the \"result-presenters\" slot of those
engines."
  :group 'easi
  :type '(repeat (choice symbol
			 (restricted-sexp
			  :match-alternatives easi-presenter-p))))

(defcustom easi-results-default-buffer-name "EASI Results"
  "Default name for EASI buffer displaying collected results.

A string will be used as the buffer name. A function will be
called with three arguments: current searchable, the current
query, and the list of results. It should return a string."
  :group 'easi
  :type '(choice string function))

(defcustom easi-result-default-buffer-name "EASI Current Result"
  "Default name for EASI buffer displaying the current result.

A string will be used as the buffer name. A function will be
called with the current result as its sole argument."
  :group 'easi
  :type '(choice string function))

(defcustom easi-default-presenter-grouper nil
  "Default method for displaying a group of presenters.")

;;;; Basic types

;;;###autoload
(defclass easi-presenter ()
  ((name :initarg :name :initform nil)
   (key :initarg :key :initform nil)
   (display-action :initarg :display-action :initform nil)
   (before :initarg :before :initform nil)
   (printer :initarg :printer :initform nil)
   (after :initarg :after :initform nil)
   (current-getter :initarg :current-getter :initform nil)
   (hook :initarg :hook :initform nil))
  "Base class for Easi presenters.")

;;;###autoload
(defclass easi-presentable-group nil
  ((name :initarg :name :initform nil)
   (key :initarg :key :initform nil)
   (displayer :initarg :display-action :initform nil)
   (documentation :initarg :documentation :initform "")
   (presentables :initarg :presentables :initform nil))
  "A group of presentables.")

;;;; Setting up buffers

(cl-defgeneric easi-presentable--set-buffers (presentable session result-or-results)
  "Ensure that SESSION has buffers setup for PRESENTABLE.

Mutably modifies SESSION's state. For each presenter in
PRESENTABLE, ensure that SESSION has a buffer, and that the
association between buffer and presenter is registered.

RESULT-OR-RESULTS should be either of the symbols `result' or
`results', to indicate the type of thing being printed.

Do not rely on this function's return value.")

(cl-defmethod easi-presentable--set-buffers ((presentable symbol) session result-or-results)
  "Call `easi-presentable--set-buffers' on value of PRESENTABLE.

Pass SESSION and RESULT-OR-RESULTS on unchanged."
  (easi-presentable--set-buffers
   (symbol-value presentable) session result-or-results))

(cl-defmethod easi-presentable--set-buffers ((presentable cons) session result-or-results)
  "Map `easi-presentable--set-buffers' over PRESENTABLE.

Pass SESSION and RESULT-OR-RESULTS on unchanged. Mapping is done
with `mapc'."
  (mapc
   (lambda (pres)
     (easi-presentable--set-buffers pres session result-or-results))
   presentable))

(cl-defmethod easi-presentable--set-buffers ((presentable easi-presentable-group) session result-or-results)
  "Call `easi-presentable--set-buffers' on value of slot \"presentables\".

Get presentables from PRESENTABLE's slots \"presentables\", and
call `easi-presentable--set-buffers' on this value.

Pass SESSION and RESULT-OR-RESULTS on unchanged."
  (easi-presentable--set-buffers
   (slot-value presentable 'presentables) session result-or-results)
  ;; TODO run here the group display function
  )

(cl-defmethod easi-presentable--set-buffers ((presentable easi-presenter) session result-or-results)
  "Create and set a buffer for PRESENTABLE, unless there is one already.

If PRESENTABLE is already associated (in SESSION) with a buffer,
do nothing. Otherwise, get a new buffer with
`easi-utils--buffer-from-default', and set the relevant state in
the session.

RESULT-OR-RESULTS should be either of the symbols `result' or
`results', to indicate the type of thing being printed."
  ;; If the presenter already has a buffer in the list.
  ;; (`easi--kill-buffer-manage-sessions' ensures that killed buffers
  ;; are removed from this list, so anything strange happens, we can
  ;; assume that the buffer is live.) This ensures that no presenter
  ;; is used more than once.
  (unless (rassoc presentable (easi-session-state-buffer-presenters session))
    (let* ((default (cl-case result-or-results
		      (result easi-result-default-buffer-name)
		      (results easi-results-default-buffer-name)))
	   (buffer (easi-utils--buffer-from-default default session)))
      ;; If necessary, push the buffer into the result/results buffer
      ;; list
      (cl-pushnew buffer
		  (cl-case result-or-results
		    (result (easi-session-state-result-buffers session))
		    (results (easi-session-state-results-buffers session))))
      (setf (alist-get
	     buffer (easi-session-state-buffer-presenters session))
	    presentable))))

(provide 'easi-presentable)
;;; easi-presentable.el ends here
