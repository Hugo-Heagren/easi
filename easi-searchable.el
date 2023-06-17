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
`easi-query-results' has a method."))
  (all-results-getter nil
   (:documentation
    "Way of getting a list of results. Must be of a type for which
`easi-all-results' has a method."))
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
    "List of compatible results sorters."))
  (max-results nil
   (:documentation
    "Maximum number of results to retrieve at once."))
  (max-suggestions nil
   (:documentation
    "Maximum number of suggestions to retrieve at once.")))

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

(defvar easi-default-max-suggestions)

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

(cl-defmethod easi-get-suggestions (query (suggestions-getter string) &optional number)
  "SUGGESTIONS-GETTER is a string.

Replace %s with QUERY, and %n with NUMBER (or 10 if number is not
specified) in SUGGESTIONS-GETTER, then make an https request on
the result. Return a buffer holding the response text (this is
intended to be post-processed)."
  (let* ((request-string
	  (url-encode-url
	   (format-spec suggestions-getter
			`((?s . ,query)
		          (?n . ,number)))))
	 (request-url (url-generic-parse-url request-string))
	 (response-buffer (url-retrieve-synchronously
			   request-url 'silent 'inhibit-cookies)))
    response-buffer))

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
  (when-let ((_ (not (string-empty-p query)))
	     (getter (easi-search-engine-suggestions-getter searchable))
	     (raw-results
	      (easi-get-suggestions query getter
				    (or number
					(easi-search-engine-max-results searchable)
					easi-default-max-suggestions))))
    (if-let (post-proc (easi-search-engine-suggestion-post-processor searchable))
	(easi-structured-object-get-field post-proc raw-results)
      raw-results)))
(cl-defmethod easi-searchable-suggestions (query (searchable easi-search-engine-group) &optional number)
  (mapcar (lambda (sch) (easi-searchable-suggestions query sch number))
	  (easi-search-engine-group-searchables searchable)))
(cl-defmethod easi-searchable-suggestions (query (searchable cons) &optional number)
  "SEARCHABLE is a list."
  (flatten-list
   (mapcar (lambda (sch) (easi-searchable-suggestions query sch number)) searchable)))
(cl-defmethod easi-searchable-suggestions (query (searchable symbol) &optional number)
  (easi-searchable-suggestions query (symbol-value searchable) number))

;;;; Getting results

(defvar easi-default-max-results)

(cl-defgeneric easi-query-results (query queryable-results-getter &key number page)
  "Get a list of by querying QUERYABLE-RESULTS-GETTER with QUERY.

QUERY is always string. If NUMBER is non-nil, no more than NUMBER
results should be returned.")

;; Simplest case
(cl-defmethod easi-query-results (query (queryable-results-getter symbol) &key number page)
  "QUERYABLE-RESULTS-GETTER is a function.

Pass QUERY, NUMBER and PAGE to QUERYABLE-RESULTS-GETTER, in that
order."
  (funcall queryable-results-getter query number page))

(cl-defmethod easi-query-results (query (queryable-results-getter string) &key number page)
  "QUERYABLE-RESULTS-GETTER is a string.

Replace %s with QUERY, and %n with NUMBER in
QUERYABLE-RESULTS-GETTER, then make an https request on the
result. Return a buffer holding the response text (this is
intended to be post-processed)."
  (let* ((request-string
	  (url-encode-url
	   (format-spec queryable-results-getter
			`((?s . ,query)
		          (?n . ,number)
			  (?p . ,page)))))
	 (request-url (url-generic-parse-url request-string))
	 (response-buffer (url-retrieve-synchronously
		    request-url 'silent 'inhibit-cookies)))
    response-buffer))

;; TODO Some more interesting implementations of this ^

(cl-defgeneric easi-all-results (all-results-getter)
  "Get a list of all results from ALL-RESULTS-GETTER.

Used for searchables which can return \"everything\" in some
meaningful sense (i.e. all the notes in my collection, not just
the ones which match a query).")

(cl-defmethod easi-all-results ((all-results-getter symbol))
  "ALL-RESULTS-GETTER is a symbol.

If ALL-RESULTS-GETTER is a function (i.e. passes `functionp')
then call it with `funcall'. If not a function, get its
`symbol-value' (this allows using a variable which contains a
list of objects, where no function exists to return them all)."
  (if (functionp all-results-getter)
      (funcall all-results-getter)
    (symbol-value all-results-getter)))

;; TODO Some more interesting implementations of this ^

(cl-defgeneric easi-searchable-results (searchable &optional query number)
  "Get a list of results from querying SEARCHABLE with QUERY.

If NUMBER is non-nil, limit the number of results from each
engine in SEARCHABLE to NUMBER.")

(defvar easi-default-non-all-results-skip)
(defvar easi-default-non-queryable-skip)

(cl-defmethod easi-searchable-results ((searchable easi-search-engine) &optional query number)
  "Get results from "
  (when-let ((getter
	      ;; Whether query is non-nil is an indication of whether
	      ;; a query was originally passed by the user. If so, we
	      ;; are doing something like `easi-search'. If not,
	      ;; something more like `easi-all'.
	      (if query
		  (or (easi-search-engine-queryable-results-getter searchable)
		      (not easi-default-non-queryable-skip
			   (easi-search-engine-all-results-getter searchable)))
		(or (easi-search-engine-all-results-getter searchable)
		    (and (stringp easi-default-non-all-results-skip)
			 (setq query easi-default-non-all-results-skip)
			 (easi-search-engine-queryable-results-getter searchable)))))
	     (raw-results
	      (if query
		  (easi-query-results query getter
				      (or number
					  (easi-search-engine-max-results searchable)
					  easi-default-max-results))
		(easi-all-results getter))))
    (mapcar
     (apply-partially #'easi-utils-result-attach-search-engine searchable)
     (if-let (post-proc (easi-search-engine-results-post-processor searchable))
	 (easi-structured-object-get-field post-proc raw-results)
       raw-results))))
(cl-defmethod easi-searchable-results ((searchable easi-search-engine-group) &optional query number)
  (mapcar (lambda (searchable) (easi-searchable-results searchable query number))
	  (easi-search-engine-group-searchables searchable)))
(cl-defmethod easi-searchable-results ((searchable cons) &optional query number)
  "SEARCHABLE is a list."
  (apply 'append
   (mapcar (lambda (searchable) (easi-searchable-results searchable query number)) searchable)))
(cl-defmethod easi-searchable-results ((searchable symbol) &optional query number)
  (easi-searchable-results (symbol-value searchable) query number))

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
   (mapcan #'easi-searchable-result-presenters searchable)))

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
   ;; TODO Do I want to use `cl-delete-duplicates' and test for cases
   ;; of an object and a symbol pointing at the object?
   (delete-dups
    `(,@(easi-searchable-result-presenters searchables)
      ,@easi-default-result-presenters)))

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
