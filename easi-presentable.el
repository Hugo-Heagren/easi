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

(require 'easi-presenter)
(require 'cl-lib)
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
(defcustom easi-results-default-display-action nil
  "ACTION arg for `display-buffer' when displaying a results buffer."
  :type display-buffer--action-custom-type
  :group 'easi)

(defcustom easi-result-default-display-action nil
  "ACTION arg for `display-buffer' when displaying a result buffer."
  :type display-buffer--action-custom-type
  :group 'easi)

;;;; Group type

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

(cl-defmethod easi-presentable--set-buffers ((presentable (eql nil)) session result-or-results)
  "Ignore.

Because there is no presentable, no buffer needs to be assigned,
but there does need to be a meaningful method for this case,
otherwise the default implementation throws an error.

Ignores PRESENTABLE, SESSION and RESULT-OR-RESULTS.

Return nil."
  (ignore presentable session result-or-results))

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

;;;; Printing

(cl-defgeneric easi-presentable--print (presentable session &key printable-or-thread slots buffer)
  "Print PRINTABLE-OR-THREAD with PRESENTABLE in associated buffer.

Get the buffer or buffers associated with PRESENTABLE in SESSION,
and print PRINTABLE-OR-THREAD (a result or list of results) into it")

(cl-defmethod easi-presentable--print ((presentable symbol) session &key printable-or-thread slots buffer)
  "Call `easi-presentable--print' on the value of PRESENTABLE.

Pass SESSION, PRINTABLE-OR-THREAD, SLOTS and BUFFER unchanged."
  (easi-presentable--print
   (symbol-value presentable) session :printable-or-thread printable-or-thread :slots slots :buffer buffer))

(cl-defmethod easi-presentable--print ((presentable cons) session &key printable-or-thread slots buffer)
  "Map `easi-presentable--print' over PRESENTABLE.

Pass SESSION, PRINTABLE-OR-THREAD, SLOTS and BUFFER unchanged. Mapping is
done with `mapc'."
  (mapc
   (lambda (pres)
     (easi-presentable--print
      pres session :printable-or-thread printable-or-thread :slots slots :buffer buffer))
   presentable))

(cl-defmethod easi-presentable--print ((presentable easi-presentable-group) session &key printable-or-thread slots buffer)
  "Call `easi-presentable--print' on value of slot \"presentables\".

Get presentables from PRESENTABLE's slots \"presentables\", and
call `easi-presentable--set-buffers' on this value.

Pass SESSION, PRINTABLE-OR-THREAD, SLOTS and BUFFER unchanged."
    (mapc
   (lambda (pres) (easi-presentable--print
	      pres session :printable-or-thread printable-or-thread :slots slots :buffer buffer))
   (slot-value presentable 'presentables)
   ;; TODO Run here the presenter-group display function. This should
   ;; account for the fact that there is a 1-1 correspondence between
   ;; buffers and presenters, so
   ))

;; Requiring Easi would be circular.
(declare-function easi-results-mode "easi")
(declare-function easi-result-mode "easi")

(cl-defmethod easi-presentable--print ((presentable easi-presenter) session &key printable-or-thread slots buffer)
  "Print PRINTABLE-OR-THREAD in BUFFER.

PRINTABLE-OR-THREAD is expected to be either a single result or a list of
results. The relevant presenter should be prepared to handle
this. It defaults to the list of result in SESSION.

Call each of the functions in each of SLOTS in PRESENTABLE
passing PRINTABLE-OR-THREAD and SESSION to each. As a special case, no args
are passed to the functions in the \"hook\" slot."
  (with-current-buffer buffer
    (dolist (slot slots)
      (if (eq slot 'hook)
	  (mapc #'funcall (slot-value presentable 'hook))
	(mapc
	 (lambda (fun)
	   (funcall
	    fun
	    (if (and (threadp printable-or-thread) (memq slot '(printer after)))
		(thread-join printable-or-thread)
	      printable-or-thread)
	    session))
	 (slot-value presentable slot))))
    ;; Turn on the relevant mode
    ;; (if the buffer is in both lists, being a results buffer
    ;; 'wins').
    ;; TODO In future, do I want to make a merged mode for such cases?
    (if (memq buffer (easi-session-state-results-buffers session))
	(easi-results-mode)
      (easi-result-mode))))

;;;; Compatibility

(cl-defgeneric easi-presentable-presenter-compat-p (presentable presenter)
  "Return non-nil if PRESENTER is compatible with PRESENTABLE.")

(cl-defmethod easi-presentable-presenter-compat-p ((presentable symbol) presenter)
  "Call `easi-presentable-presenter-compat-p' on value of PRESENTABLE.

Pass PRESENTER on."
  (easi-presentable-presenter-compat-p (symbol-value presentable) presenter))

(cl-defmethod easi-presentable-presenter-compat-p ((presentable cons) presenter)
  "Return t if PRESENTER is compatible with any member of PRESENTABLE."
  (when (memq t
	      (mapcar
	       (lambda (pres) (easi-presentable-presenter-compat-p pres presenter))
	       presentable))
    t))

(cl-defmethod easi-presentable-presenter-compat-p ((presentable easi-presentable-group) presenter)
  "Non-nil if PRESENTER is compatible with any presentable in PRESENTABLE."
  (easi-presentable-presenter-compat-p
   (slot-value presentable 'presentables)
   presenter))

(cl-defmethod easi-presentable-presenter-compat-p ((presentable easi-presenter) presenter)
  "Non-nil if PRESENTABLE and PRESENTER are `eql'."
  (eql presentable presenter))

(cl-defmethod easi-presentable-presenter-compat-p ((presentable (eql nil)) presenter)
  "Return nil.

PRESENTABLE and PRESENTER are otherwise ignored."
  (ignore presentable presenter))

;;;; Displaying

(cl-defgeneric easi-presentable--display-buffer (buffer presentable result-or-results)
  "Display BUFFER with PRESENTABLE.

RESULT-OR-RESULTS indicates what type of thing is being
displayed in BUFFER.")

(cl-defmethod easi-presentable--display-buffer (buffer (presentable symbol) result-or-results)
  "Call `easi-presentable--display-buffer' on value of PRESENTABLE.

Pass BUFFER and RESULT-OR-RESULTS unaltered."
  (easi-presentable--display-buffer buffer (symbol-value presentable)) result-or-results)

(cl-defmethod easi-presentable--display-buffer (buffer (presentable cons) result-or-results)
  "Map `easi-presentable--display-buffer' over PRESENTABLE with `mapc'.

Pass BUFFER and RESULT-OR-RESULTS unaltered."
  (mapc (lambda (pres) (easi-presentable--display-buffer buffer pres result-or-results))
	presentable))

(cl-defmethod easi-presentable--display-buffer (buffer (presentable easi-presentable-group) result-or-results)
  "Call `easi-presentable--display-buffer' on all presentables in PRESENTABLE.

Pass BUFFER and RESULT-OR-RESULTS unaltered."
  (easi-presentable--display-buffer
   buffer (slot-value presentable 'presentables) result-or-results))

(cl-defmethod easi-presentable--display-buffer (buffer (presentable easi-presenter) result-or-results)
  "Display BUFFER.

Call `display-buffer', passing either:
- the value of slot `display-action' in PRESENTABLE, if non-nil
- `easi-result-default-display-action' or
  `easi-results-default-display-action' according to
  RESULT-OR-RESULTS otherwise."
(display-buffer
   buffer
   (or (slot-value presentable 'display-action)
       (cl-case result-or-results
	 (result easi-result-default-display-action)
	 (results easi-results-default-display-action)))))


(provide 'easi-presentable)
;;; easi-presentable.el ends here
