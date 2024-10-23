;;; easi-searchable.el --- Searchable API            -*- lexical-binding: t; -*-

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

;; This module is dedicated to EASI's pseudo-type `searchable's, and
;; interacting with them programatically.
;;
;; A searchable is one of:
;; - an `easi-search-engine'
;; - an `easi-search-engine-group'
;; - a list of searchables
;; - a symbol whose value is a searchable
;; - (and nothing else is a searchable)
;;
;; The API for searchables currently includes:
;; `easi-searchable-suggestions' and `easi-searchable--results'.

;;; Code:

(require 'cl-lib)
(require 'easi-structured-object-getter)
(require 'easi-presentable)
(require 'easi-utils)

;;;; Basic types

;; TODO would it be useful to have a more convenient constructor
;; function? (you do this by writing a specific method for
;; `make-instance')

;;TODO Check type validation for slots? ()
(defclass easi-search-engine ()
  (;; TODO Eventually, `name', `key', etc. will be abstracted out into
   ;; an `easi-selectable' class
   (name
    :initarg :name
    :initform nil
    :documentation "Name of this search engine.")
   (documentation
    :initarg :documentation
    :initform nil
    :documentation "Documentation string.")
   (key
    :initarg :key
    :initform nil
    :documentation "Key used for selection")
   (suggestion-post-processor
    :initarg :suggestion-post-processor
    :initform nil
    :documentation
    "Processing to be done to the suggestions, after retrieval but
before they are used by anything else in EASI. Takes the same
form as FIELD in `easi-result--get-field'.")
   (results-post-processor
    :initarg :results-post-processor
    :initform nil
    :documentation
    "Processing to be done to the results, after retrieval but before
they are used by anything else in EASI. Takes the same form as
FIELD in `easi-result--get-field'.")
   (field-aliases
    :initform nil
    :initarg :field-aliases
    :documentation
    "Alist of field aliases.
See `easi-result--list-fields' and `easi-result--get-field'.")
   (results-presenters
    :initform nil 
    :initarg :results-presenters
    :documentation
    "List of compatible results presenters (need not include those in
`easi-default-results-presenters'.)")
   (result-presenters
    :initform nil 
    :initarg :result-presenters
    :documentation
    "List of compatible result presenters (need not include those in
`easi-default-result-presenters'.)")
   (sorters
    :initarg :sorters
    :initform nil
    :documentation
    "List of compatible results sorters.")
   (max-results
    :initform nil
    :initarg :max-results
    :documentation
    "Maximum number of results to retrieve at once.

Acceptable values:
- nil (default): use `easi-default-max-results' as the max number
- a number: the maximum number to return
- t: no max number, return all results

This slot only applies to `easi-search'. `easi-all' always
returns all results. Value of nil means no max, get all matching
results (for searchables which interact with services like
web-based search engines, this is generally a bad idea, so this
slot defaults to `easi-default-max-results'.)")
   (max-suggestions
    :initform nil
    :initarg :max-suggestions
    :documentation
    "Maximum number of suggestions to retrieve at once."))
  "A single atomic search engine.")
Technically, this is itself a searchable, but naming it this way
makes more sense."))

;;;; Getting suggestions

(defvar easi-default-max-suggestions)

(defun easi-searchable--suggestions (query searchable &optional number)
  "TODO DOCS"
  (when-let (((not (string-empty-p query)))
	     (raw-results
	      (easi-searchable-suggestions
	       query searchable
	       (or number
		   (slot-value searchable 'max-results)
		   easi-default-max-suggestions))))
    (if-let (post-proc (slot-value searchable 'suggestion-post-processor))
	(easi-structured-object-get-field post-proc raw-results)
      raw-results)))

;; This is a public-interface function, so should only have single
;; dashes in it's name. TODO make the same true for the results
;; functions!
(cl-defgeneric easi-searchable-suggestions (query searchable &optional number)
  "Get a list of suggestions from querying SEARCHABLE with QUERY.

If NUMBER is non-nil, limit the number of suggestions from each
engine in SEARCHABLE to NUMBER.

If SEARCHABLE is an `easi-search-engine' with a defined
\"suggestion-post-processor\" slot, then the value of that slot
will be applied to the suggestions before they are returned. If
the slot's value is PROC, and the unprocessed results are RAW,
then the call is (easi-structured-object-get-field PROC RAW)."
  ;; Default -- no suggestions (`ignore' is used to appease flymake)
  (ignore query searchable number))

(cl-defmethod easi-searchable-suggestions (query (searchable easi-search-engine-group) &optional number)
  "Map `easi-searchable-suggestions' over searchables.

Get \"searchables\" slot in SEARCHABLE, and map
`easi-searchable-suggestions' over each one, passing QUERY, the
searchable and NUMBER in each case."
  (mapcar (lambda (sch) (easi-searchable-suggestions query sch number))
	  (slot-value searchable 'searchables)))

(cl-defmethod easi-searchable-suggestions (query (searchable cons) &optional number)
  "Map `easi-searchable-suggestions' over SEARCHABLE.

Flatten the list, and `mapcar' `easi-searchable-suggestions' over
the result, passing QUERY, the searchable, and NUMBER each time."
  (flatten-list
   (mapcar (lambda (sch) (easi-searchable-suggestions query sch number)) searchable)))

(cl-defmethod easi-searchable-suggestions (query (searchable symbol) &optional number)
  "Call `easi-searchable-suggestions' with value of SEARCHABLE.

Pass QUERY, the value of SEARCHABLE, and NUMBER."
  (easi-searchable-suggestions query (symbol-value searchable) number))

;;;; Getting results

(defvar easi-default-max-results)
(defvar easi-default-non-queryable-skip)

(cl-defgeneric easi-searchable-query-results (query searchable &key number page)
  "Get a list of results by querying SEARCHABLE with QUERY.

QUERY is always string. If NUMBER is non-nil, no more than NUMBER
results should be returned. PAGE is the page of results to be
returned (1-indexed).

The default implementation acts as a fallback for searchables which do
not implement this behaviour. It checks
`easi-default-non-queryable-skip' and acts accordingly."
  ;; Silence flymake
  (ignore query number page)
  (if easi-default-non-queryable-skip
      nil
    (easi-searchable-all-results searchable)))

(defvar easi-default-non-all-results-skip)

(cl-defgeneric easi-searchable-all-results (searchable)
  "Get a list of all results from SEARCHABLE.

Used for searchables which can return \"everything\" in some
meaningful sense (i.e. all the notes in my collection, not just
the ones which match a query)."
  ;; The only reason this (default) implementation is called is if the
  ;; SEARCHABLE can't handle getting all results, so this implements
  ;; what to do in that case:
  (pcase easi-default-non-all-results-skip
    ((pred stringp)
     (easi-searchable-query-results
      ;; TODO Do we need :number and/or :page here?
      easi-default-non-all-results-skip searchable))
    ;; Skip and return nothing.
    ('t nil)
    ;; Not formally defined. Raise an error so that we can extend in
    ;; the future if necessary.
    (_ (error "Undefined value for `easi-default-non-all-results-skip'"))))

(cl-defgeneric easi-searchable--results (searchable &key query number page)
  "Get and process results from SEARCHABLE.

If QUERY is non-nil, pass it along to SEARCHABLE.

If NUMBER is non-nil, limit the number of results from each
engine in SEARCHABLE to NUMBER. PAGE is the page of results to
get, if results are paginated (1-indexed).

If SEARCHABLE has a non-nil \"results-post-processor\" slot, then pass
the results through this before returning them.")

(cl-defmethod easi-searchable--results ((searchable easi-search-engine) &key query number page)
  ;; TODO Document how this function differs from (e.g.
  ;; `easi-searchable-query-results'): `easi-searchable--results' is
  ;; a generic function with methods for different types of searchable
  ;; (symbols, lists, etc.). `easi-searchable-query-results' and
  ;; friends are generics from which different classes of searchable
  ;; are supposed to derive their functionality.
  "Get and process results from SEARCHABLE.

If QUERY is non-nil, call `easi-searchable-query-results' with QUERY,
NUMBER and PAGE. If QUERY is nil, call `easi-searchable-all-results'.

If NUMBER is non-nil, limit the number of results from each engine in
SEARCHABLE to NUMBER. PAGE is the page of results to get, if results are
paginated (1-indexed).

If SEARCHABLE has a non-nil \"results-post-processor\" slot, then pass
the results through this before returning them."
  (when-let ((raw-results
	      ;; Whether query is non-nil is an indication of whether
	      ;; a query was originally passed by the user. If so, we
	      ;; are doing something like `easi-search'. If not,
	      ;; something more like `easi-all'.
	      (if query
		  (easi-searchable-query-results
		   query searchable
		   :number (or number
			       (slot-value searchable 'max-results)
			       easi-default-max-results)
		   :page page)
		(easi-searchable-all-results searchable))))
    (mapcar
     (apply-partially #'easi-utils--result-attach-search-engine searchable)
     (if-let (post-proc (slot-value searchable 'results-post-processor))
	 (easi-structured-object-get-field post-proc raw-results)
       raw-results))))

(cl-defmethod easi-searchable--results ((searchable easi-search-engine-group) &key query number page)
  "`mapcar' `easi-searchable--results' over searchables in SEARCHABLE.

Get the \"searchables\" slot from SEARCHABLE, and map
`easi-searchable--results' over these, passing QUERY, NUMBER and
PAGE directly."
  (mapcar (lambda (searchable)
	    (easi-searchable--results
	     searchable :query query :number number :page page))
	  (slot-value searchable 'searchables)))

(cl-defmethod easi-searchable--results ((searchable cons) &key query number page)
  "Mapcar `easi-searchable--results' over SEARCHABLE, `append' result.

Pass QUERY, NUMBER and PAGE `easi-searchable--results'."
  (apply 'append
	 (mapcar (lambda (searchable)
		   (easi-searchable--results
		    searchable :query query :number number :page page))
		 searchable)))

(cl-defmethod easi-searchable--results ((searchable symbol) &key query number page)
  "Call `easi-searchable--results' with value of SEARCHABLE.

Pass the value of SEARCHABLE, QUERY, NUMBER and PAGE."
  (easi-searchable--results
   (symbol-value searchable)
   :query query :number number :page page))

;;;; Getting presenters

(cl-defgeneric easi-searchable--results-presenters (searchable)
  "Return a list of results presenters supported by SEARCHABLE.")

(cl-defmethod easi-searchable--results-presenters ((searchable cons))
  "Map `easi-searchable--results-presenters' over SEARCHABLE.

Delete duplicates before returning."
  (delete-dups
   (mapcan #'easi-searchable--results-presenters searchable)))

(cl-defmethod easi-searchable--results-presenters ((searchable symbol))
  "Call recursively on `symbol-value' of SEARCHABLE."
  (easi-searchable--results-presenters (symbol-value searchable)))

(cl-defmethod easi-searchable--results-presenters ((searchable easi-search-engine))
  "Call `easi-search-engine-results-presenters' on SEARCHABLE."
  (slot-value searchable 'results-presenters))

(cl-defmethod easi-searchable--results-presenters ((searchable easi-search-engine-group))
  "Get searchables from group SEARCHABLE, and call on that list."
  ;; This works because a list of searchables is a searchables, so
  ;; there is a method for lists, and the value of the :searchables
  ;; slot is always a list.
  (easi-searchable--results-presenters
   (slot-value searchable 'searchables)))

(defun easi-searchable--get-results-presenters (searchable)
  "List all results presenters compatible with SEARCHABLE."
  (cl-delete-if
   #'null
   ;; TODO Do I want to use `cl-delete-duplicates' and test for cases
   ;; of an object and a symbol pointing at the object?
   (delete-dups
    `(,@(easi-searchable--results-presenters searchable)
      ,@easi-default-results-presenters))))

(cl-defgeneric easi-searchable--result-presenters (searchable)
  "Return a list of result presenters supported by SEARCHABLE.")

(cl-defmethod easi-searchable--result-presenters ((searchable cons))
  "Map `easi-searchable--result-presenters' over SEARCHABLE.

Delete duplicates before returning."
  (delete-dups
   (mapcan #'easi-searchable--result-presenters searchable)))

(cl-defmethod easi-searchable--result-presenters ((searchable symbol))
  "Call recursively on `symbol-value' of SEARCHABLE."
  (easi-searchable--result-presenters (symbol-value searchable)))

(cl-defmethod easi-searchable--result-presenters ((searchable easi-search-engine))
  "Call `easi-search-engine-results-presenters' on SEARCHABLE."
  (slot-value searchable 'result-presenters))

(cl-defmethod easi-searchable--result-presenters ((searchable easi-search-engine-group))
  "Get searchables from group SEARCHABLE, and call on that list."
  ;; This works because a list of searchables is a searchables, so
  ;; there is a method for lists, and the value of the :searchables
  ;; slot is always a list.
  (easi-searchable--result-presenters
   (slot-value searchable 'searchables)))

(defun easi-searchable--get-result-presenters (searchable)
  "List all result presenters compatible with SEARCHABLE."
   ;; TODO Do I want to use `cl-delete-duplicates' and test for cases
   ;; of an object and a symbol pointing at the object?
   (delete-dups
    `(,@(easi-searchable--result-presenters searchable)
      ,@easi-default-result-presenters)))

;;;; Getting sorter list

(cl-defgeneric easi-searchable--sorters (searchable)
  "Return a list of sorting functions compatible with SEARCHABLE.")

(cl-defmethod easi-searchable--sorters ((searchable cons))
  "Get a list of sorters compatible with all members of SEARCHABLE."
  (seq-reduce
   #'seq-intersection
   (mapcar #'easi-searchable--sorters (cdr searchable))
   (easi-searchable--sorters (car searchable))))

(cl-defmethod easi-searchable--sorters ((searchable symbol))
  "Call `easi-searchable--sorters' on value of SEARCHABLE."
  (easi-searchable--sorters (symbol-value searchable)))

(cl-defmethod easi-searchable--sorters ((searchable easi-search-engine))
  "Call `easi-search-engine-sorters' on SEARCHABLE."
  (slot-value searchable 'sorters))

(cl-defmethod easi-searchable--sorters ((searchable easi-search-engine-group))
  "Get SEARCHABLE's searchables, pass to `easi-searchable--sorters'."
  (easi-searchable--sorters (slot-value searchable 'searchables)))

(provide 'easi-searchable)
;;; easi-searchable.el ends here
