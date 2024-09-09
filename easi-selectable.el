;;; easi-selectable.el --- Standard selection interface in EASI  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Hugo Heagren

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

;;; Code:

(require 'eieio)

;;;; easi-selectable

(defclass easi-selectable ()
  ((key :initarg :key
	:type (satisfies key-valid-p)
	:documentation "Key used for selection")
   (name :initarg :name
	 :type string)
   (documentation :initarg :documentation
		  :documetation "Documentation string for this object.
Standard Emacs docstrings constructs are supported."))
  "Superclass for selecting things in EASI."
  :abstract t)

(provide 'easi-selectable)
;;; easi-selectable.el ends here
