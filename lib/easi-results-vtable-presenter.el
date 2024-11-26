;;; easi-results-vtable-presenter.el --- EASI results presenter based on `vtable'  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Hugo Heagren

;; Author: Hugo Heagren <hugo@undertown>
;; Keywords: 

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

;; ;; This modules provides a results presenter for EASI results, based
;; on `vtable'.


;;; Code:

(require 'cl-lib)
(require 'vtable)
(require 'easi-session)

(defgroup easi-results-vtable nil
  "Group for EASI results presenter based on `vtable'."
  :group 'easi)

(defun easi-results-vtable--clear-buffer (_results _session)
  (erase-buffer))

(defun easi-results-vtable--header-mode (_results _session)
  (vtable-header-mode))

(defun easi-results-vtable--print (results session)
  (vtable-insert
   (let* ((pres (alist-get (current-buffer)
			       (slot-value session 'buffer-presenters)))
	  (columns (slot-value pres 'columns)))
   (make-vtable
    :columns columns
    :objects results)
   ;; TODO Somehow make actions work... (not sure I can, with
   ;; different actions for different types of object...)
   )))

(defclass easi-results-vtable-presenter (easi-presenter)
  ((before :initform '(easi-results-vtable--clear-buffer))
   (printer :initform '(easi-results-vtable--print))
   (after :initform '(easi-results-vtable--header-mode))
   (columns :initarg :columns :initform nil
	    :documentation
	    "Used for the :columns argument in `make-vtable'.")
   (current-getter :initform 'vtable-current-object)))


;;; Implementations of things

;; (cl-defmethod easi-presenter-jump-to-result ((presenter easi-results-vtable-presenter)
;; 					     result)
;;   (let ((start (point)))
;;     (vtable-beginning-of-table)
;;     (if (text-property-search-forward 'vtable-object result #'equal)
;;         (progn
;;           (forward-line -1)
;;           (point))
;;       (goto-char start)
;;       nil)))


(provide 'easi-results-vtable-presenter)
;;; easi-results-vtable-presenter.el ends here
