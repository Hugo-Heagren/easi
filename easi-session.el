;;; easi-session.el --- Session managment facilities  -*- lexical-binding: t; -*-

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

;; This module provides code for managing Ease's sessions -- mutable
;; data structures used internally to keep track of state when using
;; Easi.

;;; Code:


(require 'eieio)

(defvar easi-session-list nil
  "List of easi sessions. Each is an `easi-session-state'.")

(defclass easi-session-state ()
  ((window-config
    :initarg :window-config
    :initform nil)
   (query
    :initarg :query
    :initform nil)
   (searchables
    :initarg :searchables
    :initform nil)
   (results
    :initarg :results
    :initform nil)
   (results-buffers
    :initarg :results-buffers
    :initform nil)
   (result-buffers
    :initarg :result-buffers
    :initform nil)
   (buffer-presenters
    :initarg :buffer-presenters
    :initform nil)
   (page
    :initarg :page
    :initform 1)
   (results-thread
    :initarg :results-thread
    :initform nil))
  "Holds current easi search and presentation state.")

;; TODO this should probably be a method
(defun easi-session--current-buffer-presenter (session)
  "Get results presenter for current buffer in SESSION."
  (alist-get (current-buffer)
	     (slot-value session 'buffer-presenters)))

(defun easi-session--get-current ()
  "Return first session with current buffer.

Search the \"result-buffers\" and \"results-buffers\" slots of
each item in `easi-session-list' and return first where current
buffer appears."
  (let ((buf (current-buffer)))
    (cl-find-if
     (lambda (session)
       (or (memq buf (slot-value session 'result-buffers))
	   (memq buf (slot-value session 'results-buffers))))
     easi-session-list)))

(defun easi-session--get-create-current ()
  "Get current session, creating one if necessary.

If a session is created, it is added to `easi-session-list'."
  (or (easi-session--get-current)
      (let* ((session (make-instance 'easi-session-state)))
	 (push session easi-session-list)
	 session)))

(provide 'easi-session)
;;; easi-session.el ends here
