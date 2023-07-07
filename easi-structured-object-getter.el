;;; easi-structured-object-getter.el --- Get information from structured objects  -*- lexical-binding: t; -*-

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

;; 

;;; Code:

(require 'cl-lib)
(require 'seq)

;;;; Structured object field getter
;; A field is an object which can be used to query a structured object
;; (such as a result, an HTTP response, an alist, etc.) A field is:
;; - A string
;; - a function
;; - a list of field

(cl-defgeneric easi--structured-object-get-field (field object)
  "Get value of FIELD in OBJECT.

FIELD is a string, function or a list. If a string, return the
value of the FIELD-named field in OBJECT. If a function, call the
function on OBJECT. If a list, then `easi--structured-object-get-field' is
called recursively using each element in turn as FIELD, and the
return value of each previous invocation as OBJECT.

This is useful for getting data from objects with more than one
level of structure (e.g. with a cl-struct with an `age' slot,
which itself is an alist with `years' and `days', the value of
`years' could be accessed with the list (\"age\" \"years\").)

The default implementation returns nil, so that nil is returned
when there is no applicable method."
  nil)
;; NOTE Interestingly, the way this is implemented means that any
;; level of nesting is equivalent to no nesting. I.e. FIELD being
;; '((("foo") "bar") baz) will deliver the same objects as '("foo"
;; "bar" baz).

;; TODO Make search-engine field aliases work. I want to be able
;; getting a field in that list should return the value of the ALIASED
;; field, not the standard field. (I think the way to do this is
;; probably by attaching the search engine which produced a result
;; /to/ that result, like in `related-files').
(cl-defmethod easi--structured-object-get-field ((field cons) object)
  "FIELD is a list.

Call `easi--structured-object-get-field' recursively using each element of
FIELD in turn."
  (let ((with-first (easi--structured-object-get-field (car field) object)))
    (if-let ((next (cdr field)))
	(easi--structured-object-get-field next with-first)
      with-first)))

(cl-defmethod easi--structured-object-get-field ((field vector) (object sequence))
  "FIELD is a vector and OBJECT is a sequence.

Map each element of FIELD over the elements of OBJECT, and pass
the result as OBJECT to another call to
`easi--structured-object-get-field'."
  (let ((with-first
	 (seq-map
	  (apply-partially #'easi--structured-object-get-field (aref field 0))
	  object))
	(next (seq-drop field 1)))
    (if (or (null next) (eq next []))
	with-first
      (easi--structured-object-get-field next with-first))))

(cl-defmethod easi--structured-object-get-field ((field (head lambda)) object)
  "FIELD is a list.

Call `easi--structured-object-get-field' recursively using each element of
FIELD in turn."
  (funcall field object))

(cl-defmethod easi--structured-object-get-field ((field integer) (object cons))
  "Get element at index FIELD in OBJECT

(This method is for list objects.)"
  (nth field object))

(cl-defmethod easi--structured-object-get-field ((field integer) (object vector))
  "Get element at index FIELD in OBJECT.

(This method is for vector objects.)"
  (aref object field))

;; TODO Allow FIELD to be a lambda, not just a symbol for a function
;; The easiest way to do this might be to just fix the Emacs bug with
;; cl-lib---other types are meant to be supported.
(cl-defmethod easi--structured-object-get-field ((field symbol) object)
  "FIELD is a " ;; TODO
  (funcall field object))
(cl-defmethod easi--structured-object-get-field ((field compiled-function) object)
  "FIELD is a " ;; TODO
  (funcall field object))

;; TODO Wtf is a good implementation of this for lists?
(cl-defmethod easi--structured-object-get-field (field (object cons))
  "Get FIELD from list OBJECT.

An attempt to DWIM with the various types of list in Emacs.

If OBJECT passes `plistp' and the first item is not itself a
list, then try to get value of the following keys, in this order,
returning the first which is non-nil:
- OBJECT
- (intern OBJECT)
- (intern (concat \":\" OBJECT))

If OBJECT fails `plistp', assume it is an alist, and call
`alist-get', with TESTFN set to `string='."
  (let ((comp-func
	 (lambda (x y)
	   (or
	    (string= x y)
	    (eq x y)))))
    (cond
     ((cl-every #'consp object)
      (alist-get field object nil nil comp-func))
     ((plistp object)
      (plist-get
       field object comp-func)))))

(cl-defmethod easi--structured-object-get-field (field (object hash-table))
  (gethash field object))

;; TODO Write a few other obvious methods for different types of
;; object (alist, plist, cl-struct, hash-table, maybe json)

;; HACK I realise that this is named wrong, (should be easi--...) but
;; that name is taken above.
(defun easi-structured-object-get-field (field object)
  "Get FIELD or its alias in OBJECT.

Check if FIELD has an alias in the search engine which returned
OBJECT. If so, pass that as FIELD to
`easi--structured-object-get-field', and OBJECT as OBJECT. If
not, pass FIELD as FIELD.

Only strings are supported as aliases, so that other selectors
always behave in predictable ways."
  (easi--structured-object-get-field
   (if-let (((stringp field))
	    ;; TODO does `easi-result--aliases' need renaming? It
	    ;; doesn't necessarily /only/ operate on results
	    (alias-list (easi-result--aliases object))
	    (alias (alist-get field alias-list nil nil #'string=)))
       alias field)
   object))

(provide 'easi-structured-object-getter)
;;; easi-structured-object-getter.el ends here
