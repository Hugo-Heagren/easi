;;; easi-searchable.el --- Searchable API            -*- lexical-binding: t; -*-

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
;; `easi-searchable-suggestions' and `easi-searchable-results'.

;;; Code:

(require 'cl-lib)
(require 'easi-structured-object-getter)
(require 'easi-presenter)
(require 'easi-utils)

;;;; Basic types

(cl-defstruct (easi-search-engine
	       (:constructor easi-search-engine-create))
  "A single atomic search engine.

KEY is a " ;; TODO Better docstring
  (key nil
   (:type key
    :documentation "Key used for selection"))
  (name ""
   (:type string))
  documentation
  ;; opensearch ?
  (suggestions-getter nil
   (:documentation
    "Way of getting a list of suggestions. Must be a of a type for which
`easi-get-suggestions' has a method."))
  (suggestion-post-processor nil
   (:documentation
    "Processing to be done to the suggestions, after retrieval but
    before they are used by anything else in EASI. Takes the same
    form as FIELD in `easi-result-get-field'."))
  (queryable-results-getter nil
   (:documentation
    "Way of getting a list of results. Must be a of a type for which
`easi-get-results' has a method."))
  (results-post-processor nil
   (:documentation
    "Processing to be done to the results, after retrieval but
    before they are used by anything else in EASI. Takes the same
    form as FIELD in `easi-result-get-field'."))
  (field-aliases nil
   (:documentation
    "Alist of field aliases.

See `easi-result-list-fields' and `easi-result-get-field'."))
  (results-presenters nil
   (:documentation
    "List of compatible results presenters (need not include those in
    `easi-default-results-presenters'.)"))
  (result-presenters nil
   (:documentation
    "List of compatible result presenters (need not include those in
    `easi-default-result-presenters'.)"))
  (sorters nil
   (:documentation
    "List of compatible results sorters.")))

(cl-defstruct (easi-search-engine-group
	       (:constructor easi-search-engine-group-create))
  ;; TODO Better docs
  "A group of search-engines."
  (key (:type key
	:documentation "Key used for selection"))
  (name (:type string))
  documentation
  (searchables (:type list
                :documentation
		"List of searchables.
Technically, this is itself a searchable, but naming it this way makes
more sense.")))

;;;; Getting suggestions

(cl-defgeneric easi-get-suggestions (query suggestions-getter &optional number)
  "Get a list of by querying SUGGESTIONS-GETTER with QUERY.

QUERY is always string. If NUMBER is non-nil, no more than NUMBER
suggestions should be returned."
  ;; TODO Implement a default which just gets all of the results of
  ;; QUERY and prints the title or other appropriate field in each one
  )

;; Simplest case
(cl-defmethod easi-get-suggestions (query (suggestions-getter symbol) &optional number)
  "SUGGESTIONS-GETTER is a function."
  (funcall suggestions-getter query number))

;; TODO Some more interesting implementations of this ^

(cl-defgeneric easi-searchable-suggestions (query searchable &optional number)
  "Get a list of suggestions from querying SEARCHABLE with QUERY.

If NUMBER is non-nil, limit the number of suggestions from each
engine in SEARCHABLE to NUMBER.

If SEARCHABLE is an `easi-search-engine' with a defined
\"suggestion-post-processor\" slot, then the value of that slot
will be applied to the suggestions before they are returned. If
the slot's value is PROC, and the unprocessed results are RAW,
then the call is (easi-structured-object-get-field PROC RAW).")

(cl-defmethod easi-searchable-suggestions (query (searchable easi-search-engine) &optional number)
  "TODO DOCS"
  (when-let ((getter (easi-search-engine-suggestions-getter searchable))
	     (raw-results (easi-get-suggestions query getter number)))
    (if-let (post-proc (easi-search-engine-suggestion-post-processor searchable))
	(easi-structured-object-get-field post-proc raw-results)
      raw-results)))
(cl-defmethod easi-searchable-suggestions (query (searchable easi-search-engine-group) &optional number)
  (mapcar (apply-partially #'easi-searchable-suggestions query)
	  (easi-search-engine-group-searchables searchable)))
(cl-defmethod easi-searchable-suggestions (query (searchable cons) &optional number)
  "SEARCHABLE is a list."
  (flatten-list
   (mapcar (apply-partially #'easi-searchable-suggestions query) searchable)))
(cl-defmethod easi-searchable-suggestions (query (searchable symbol) &optional number)
  (easi-searchable-suggestions query (symbol-value searchable) number))

;;;; Getting results

(cl-defgeneric easi-get-results (query queryable-results-getter &optional number)
  "Get a list of by querying QUERYABLE-RESULTS-GETTER with QUERY.

QUERY is always string. If NUMBER is non-nil, no more than NUMBER
results should be returned.")

;; Simplest case
(cl-defmethod easi-get-results (query (queryable-results-getter symbol) &optional number)
  "QUERYABLE-RESULTS-GETTER is a function."
  (funcall queryable-results-getter query number))

;; TODO Some more interesting implementations of this ^

(cl-defgeneric easi-searchable-results (query searchable &optional number)
  "Get a list of results from querying SEARCHABLE with QUERY.

If NUMBER is non-nil, limit the number of results from each
engine in SEARCHABLE to NUMBER.")

(cl-defmethod easi-searchable-results (query (searchable easi-search-engine) &optional number)
  (when-let ((getter (easi-search-engine-queryable-results-getter searchable))
	     (raw-results (easi-get-results query getter number)))
    (mapcar
     (apply-partially #'easi-utils-result-attach-search-engine searchable)
     (if-let (post-proc (easi-search-engine-results-post-processor searchable))
	 (easi-structured-object-get-field post-proc raw-results)
       raw-results))))
(cl-defmethod easi-searchable-results (query (searchable easi-search-engine-group) &optional number)
  (mapcar (apply-partially #'easi-searchable-results query)
	  (easi-search-engine-group-searchables searchable)))
(cl-defmethod easi-searchable-results (query (searchable cons) &optional number)
  "SEARCHABLE is a list."
  (apply 'append
   (mapcar (apply-partially #'easi-searchable-results query) searchable)))
(cl-defmethod easi-searchable-results (query (searchable symbol) &optional number)
  (easi-searchable-results query (symbol-value searchable) number))

;;;; Getting presenters

(cl-defgeneric easi-searchable-results-presenters (searchable)
  "Return a list of results presenters supported by SEARCHABLE.")

(cl-defmethod easi-searchable-results-presenters ((searchable cons))
  "Map `easi-searchable-results-presenters' over SEARCHABLE.

Delete duplicates before returning."
  (delete-dups
   (mapcar #'easi-searchable-results-presenters searchable)))

(cl-defmethod easi-searchable-results-presenters ((searchable symbol))
  "Call recursively on `symbol-value' of SEARCHABLE."
  (easi-searchable-results-presenters (symbol-value searchable)))

(cl-defmethod easi-searchable-results-presenters ((searchable easi-search-engine))
  "Call `easi-search-engine-results-presenters' on SEARCHABLE."
  (easi-search-engine-results-presenters searchable))

(cl-defmethod easi-searchable-results-presenters ((searchable easi-search-engine-group))
  "Get searchables from group SEARCHABLE, and call on that list."
  ;; This works because a list of searchables is a searchables, so
  ;; there is a method for lists, and the value of the :searchables
  ;; slot is always a list.
  (easi-searchable-results-presenters
   (easi-search-engine-group-searchables searchable)))

(defun easi-get-results-presenters (searchables)
  "List all results presenters compatible with SEARCHABLES."
  (cl-delete-if
   #'null
   ;; TODO Do I want to use `cl-delete-duplicates' and test for cases
   ;; of an object and a symbol pointing at the object?
   (delete-dups
    `(,@(easi-searchable-results-presenters searchables)
      ,@easi-default-results-presenters))))

(cl-defgeneric easi-searchable-result-presenters (searchable)
  "Return a list of result presenters supported by SEARCHABLE.")

(cl-defmethod easi-searchable-result-presenters ((searchable cons))
  "Map `easi-searchable-result-presenters' over SEARCHABLE.

Delete duplicates before returning."
  (delete-dups
   (mapcar #'easi-searchable-result-presenters searchable)))

(cl-defmethod easi-searchable-result-presenters ((searchable symbol))
  "Call recursively on `symbol-value' of SEARCHABLE."
  (easi-searchable-result-presenters (symbol-value searchable)))

(cl-defmethod easi-searchable-result-presenters ((searchable easi-search-engine))
  "Call `easi-search-engine-results-presenters' on SEARCHABLE."
  (easi-search-engine-result-presenters searchable))

(cl-defmethod easi-searchable-result-presenters ((searchable easi-search-engine-group))
  "Get searchables from group SEARCHABLE, and call on that list."
  ;; This works because a list of searchables is a searchables, so
  ;; there is a method for lists, and the value of the :searchables
  ;; slot is always a list.
  (easi-searchable-result-presenters
   (easi-search-engine-group-searchables searchable)))

(defun easi-get-result-presenters (searchables)
  "List all result presenters compatible with SEARCHABLES."
  (cl-delete-if
   #'null
   ;; TODO Do I want to use `cl-delete-duplicates' and test for cases
   ;; of an object and a symbol pointing at the object?
   (delete-dups
    `(,@(easi-searchable-result-presenters searchables)
      ,@easi-default-result-presenters))))

;;;; Getting sorter list

(cl-defgeneric easi-searchable-sorters (searchable)
  "Return a list of sorting functions compatible with SEARCHABLE.")

(cl-defmethod easi-searchable-sorters ((searchable cons))
  (seq-reduce
   #'seq-intersection
   (mapcar #'easi-searchable-sorters (cdr searchable))
   (easi-searchable-sorters (car searchable))))

(cl-defmethod easi-searchable-sorters ((searchable symbol))
  (easi-searchable-sorters (symbol-value searchable)))

(cl-defmethod easi-searchable-sorters ((searchable easi-search-engine))
  (easi-search-engine-sorters searchable))

(cl-defmethod easi-searchable-sorters ((searchable easi-search-engine-group))
  (easi-searchable-sorters (easi-search-engine-group-searchables searchable)))

(provide 'easi-searchable)
;;; easi-searchable.el ends here
