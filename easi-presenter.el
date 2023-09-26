;;; easi-presenter.el --- EASI presenter objects     -*- lexical-binding: t; -*-

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

;; This module provides base presenter objects (and their API) for
;; displaying results in Easi. See also the higher-level
;; `easi-presentable'.

;;; Code:

(require 'eieio)
(require 'cl-lib)

;;;; Customizables

(defcustom easi-default-result-hide-function
  #'easi-presenter-quit-restore-hide
  "Default hide-action for a result buffer."
  :group 'easi
  :type 'function)

;; It offends my sense of tidiness, but there isn't anywhere else
;; obvious to put this and it had to go somewhere.
(defun easi-presenter-quit-restore-hide (buffer)
  "If BUFFER is displayed, call `quit-restore-window' on its window."
  ;; NOTE We HAVE to use `when-let' here, otherwise whenever we pass a
  ;; buffer which is not displayed anywhere, no window is returned,
  ;; and `quit-restore-window' interprets the nil arg as quitting the
  ;; current window!
  (when-let ((window (get-buffer-window buffer)))
    (quit-restore-window window)))

;;;; Basic type

;;;###autoload
(defclass easi-presenter ()
  ((name :initarg :name :initform nil)
   (key :initarg :key :initform nil)
   (display-action :initarg :display-action :initform nil)
   (hide-function :initarg :hide-function :initform nil)
   (before :initarg :before :initform nil)
   (printer :initarg :printer :initform nil)
   (after :initarg :after :initform nil)
   (current-getter :initarg :current-getter :initform nil)
   (hook :initarg :hook :initform nil))
  "Base class for Easi presenters.")


;;;; Hiding

(defun easi-presenter-hide-buffer (buffer presenter)
  "Hide BUFFER according to PRESENTER.

Pass BUFFER to either:
- the value of slot `hide-function' in PRESENTER, if non-nil
- `easi-default-result-hide-function' otherwise."
  (let ((hide-function
	 (or (slot-value presenter 'hide-function)
	     easi-default-result-hide-function)))
    (funcall hide-function buffer)))

;;;; Movement

(cl-defgeneric easi-presenter--next-result (presenter &optional n)
  "Generic function for going to next result in PRESENTER buffers.

With optional arg N, go to Nth next result. If N is negative, go
to previous.

The default implementation just passes N to `forward-line'."
  (ignore presenter)
  (forward-line n))

(cl-defgeneric easi-presenter--first-result (presenter)
  "Generic function for going to first result in PRESENTER buffers.

The default implementation just goes to the minimum point."
  (ignore presenter)
  (goto-char (point-min)))

(cl-defgeneric easi-presenter--last-result (presenter)
  "Generic function for going to last result in PRESENTER buffers.

The default implementation just passes N to maximum point."
  (ignore presenter)
  (goto-char (point-max)))

(provide 'easi-presenter)
;;; easi-presenter.el ends here
