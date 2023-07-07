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
;; `easi-searchable--suggestions' and `easi-searchable--results'.

;;; Code:

(require 'cl-lib)
(require 'easi-structured-object-getter)
(require 'easi-presenter)
(require 'easi-utils)

;;;; Basic types

(cl-defstruct (easi-search-engine
	       (:constructor easi-search-engine-create))
  "A single atomic search engine."
  (key nil :type key :documentation "Key used for selection")
  (name "" :type string)
  documentation
  ;; opensearch ?
  (suggestions-getter
   nil
   :documentation
   "Way of getting a list of suggestions. Must be a of a type for
which `easi-searchable--get-suggestions' has a method.")
  (suggestion-post-processor
   nil
   :documentation
   "Processing to be done to the suggestions, after retrieval but
before they are used by anything else in EASI. Takes the same
form as FIELD in `easi--result-get-field'.")
  (queryable-results-getter
   nil
   :documentation
   "Way of getting a list of results. Must be a of a type for which
`easi-searchable--query-results' has a method.")
  (all-results-getter
   nil
   :documentation
   "Way of getting a list of results. Must be of a type for which
`easi-searchable--all-results' has a method.")
  (results-post-processor
   nil
   :documentation
   "Processing to be done to the results, after retrieval but before
they are used by anything else in EASI. Takes the same form as
FIELD in `easi--result-get-field'.")
  (field-aliases
   nil
   :documentation
   "Alist of field aliases.
See `easi--result-list-fields' and `easi--result-get-field'.")
  (results-presenters
   nil
   :documentation
   "List of compatible results presenters (need not include those in
`easi-default-results-presenters'.)")
  (result-presenters
   nil
   :documentation
   "List of compatible result presenters (need not include those in
`easi-default-result-presenters'.)")
  (sorters nil :documentation "List of compatible results sorters.")
  (max-results
   nil
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
   nil
   :documentation
   "Maximum number of suggestions to retrieve at once."))

(cl-defstruct (easi-search-engine-group
	       (:constructor easi-search-engine-group-create))
  "A group of search-engines."
  (key nil
       :type key
       :documentation "Key used for selection")
  (name nil :type string)
  documentation
  (searchables
   nil
   :type list
   :documentation
   "List of searchables.
Technically, this is itself a searchable, but naming it this way
makes more sense."))

;;;; Getting suggestions

(defvar easi-default-max-suggestions)

(cl-defgeneric easi-searchable--get-suggestions (query suggestions-getter &optional number)
  "Get a list of by querying SUGGESTIONS-GETTER with QUERY.

QUERY is always string. If NUMBER is non-nil, no more than NUMBER
suggestions should be returned."
  ;; TODO Implement a default which just gets all of the results of
  ;; QUERY and prints the title or other appropriate field in each one
  )

;; Simplest case
(cl-defmethod easi-searchable--get-suggestions (query (suggestions-getter symbol) &optional number)
  "SUGGESTIONS-GETTER is a function.

Call SUGGESTIONS-GETTER with args QUERY NUMBER (even if NUMBER is
not specified, then passed as nil)."
  (funcall suggestions-getter query number))

(cl-defmethod easi-searchable--get-suggestions (query (suggestions-getter string) &optional number)
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

(cl-defgeneric easi-searchable--suggestions (query searchable &optional number)
  "Get a list of suggestions from querying SEARCHABLE with QUERY.

If NUMBER is non-nil, limit the number of suggestions from each
engine in SEARCHABLE to NUMBER.

If SEARCHABLE is an `easi-search-engine' with a defined
\"suggestion-post-processor\" slot, then the value of that slot
will be applied to the suggestions before they are returned. If
the slot's value is PROC, and the unprocessed results are RAW,
then the call is (easi-structured-object-get-field PROC RAW).")

(cl-defmethod easi-searchable--suggestions (query (searchable easi-search-engine) &optional number)
  "Get suggestions with `easi-searchable--get-suggestions' and maybe post-process.

Get value of `suggestions-getter', then pass QUERY, the getter,
and NUMBER (or `easi-default-max-suggestions' when NUMBER is nil)
to `easi-searchable--get-suggestions'.

If SEARCHABLE has a non-nil \"suggestion-post-processor\" slot,
pass the results through that too."
  (when-let (((not (string-empty-p query)))
	     (getter (easi-search-engine-suggestions-getter searchable))
	     (raw-results
	      (easi-searchable--get-suggestions query getter
				    (or number
					(easi-search-engine-max-results searchable)
					easi-default-max-suggestions))))
    (if-let (post-proc (easi-search-engine-suggestion-post-processor searchable))
	(easi-structured-object-get-field post-proc raw-results)
      raw-results)))
(cl-defmethod easi-searchable--suggestions (query (searchable easi-search-engine-group) &optional number)
  "Map `easi-searchable--suggestions' over searchables.

Get \"searchables\" slot in SEARCHABLE, and map
`easi-searchable--suggestions' over each one, passing QUERY, the
searchable and NUMBER in each case."
  (mapcar (lambda (sch) (easi-searchable--suggestions query sch number))
	  (easi-search-engine-group-searchables searchable)))
(cl-defmethod easi-searchable--suggestions (query (searchable cons) &optional number)
  "Map `easi-searchable--suggestions' over SEARCHABLE.

Flatten the list, and `mapcar' `easi-searchable--suggestions' over
the result, passing QUERY, the searchable, and NUMBER each time."
  (flatten-list
   (mapcar (lambda (sch) (easi-searchable--suggestions query sch number)) searchable)))
(cl-defmethod easi-searchable--suggestions (query (searchable symbol) &optional number)
  "Call `easi-searchable--suggestions' with value of SEARCHABLE.

Pass QUERY, the value of SEARCHABLE, and NUMBER."
  (easi-searchable--suggestions query (symbol-value searchable) number))

;;;; Getting results

(defvar easi-default-max-results)

(cl-defgeneric easi-searchable--query-results (query queryable-results-getter &key number page)
  "Get a list of by querying QUERYABLE-RESULTS-GETTER with QUERY.

QUERY is always string. If NUMBER is non-nil, no more than NUMBER
results should be returned.")

;; Simplest case
(cl-defmethod easi-searchable--query-results (query (queryable-results-getter symbol) &key number page)
  "QUERYABLE-RESULTS-GETTER is a function.

Pass QUERY, NUMBER and PAGE to QUERYABLE-RESULTS-GETTER, in that
order."
  (funcall queryable-results-getter query number page))

(cl-defmethod easi-searchable--query-results (query (queryable-results-getter string) &key number page)
  "QUERYABLE-RESULTS-GETTER is a string.

Make the following replacements in QUERYABLE-RESULTS-GETTER, and
then make an https request on the result. Return a buffer holding
the response text (this is intended to be post-processed):
- %s: QUERY
- %n: NUMBER if that is a number, or the empty string otherwise.
- %p: PAGE"
  (let* ((request-string
	  (url-encode-url
	   (format-spec queryable-results-getter
			`((?s . ,query)
		          (?n . ,(if (numberp number)
				     number
				   ""))
			  (?p . ,page)))))
	 (request-url (url-generic-parse-url request-string))
	 (response-buffer (url-retrieve-synchronously
			   request-url 'silent 'inhibit-cookies)))
    response-buffer))

;; TODO Some more interesting implementations of this ^

(cl-defgeneric easi-searchable--all-results (all-results-getter)
  "Get a list of all results from ALL-RESULTS-GETTER.

Used for searchables which can return \"everything\" in some
meaningful sense (i.e. all the notes in my collection, not just
the ones which match a query).")

(cl-defmethod easi-searchable--all-results ((all-results-getter symbol))
  "ALL-RESULTS-GETTER is a symbol.

If ALL-RESULTS-GETTER is a function (i.e. passes `functionp')
then call it with `funcall'. If not a function, get its
`symbol-value' (this allows using a variable which contains a
list of objects, where no function exists to return them all)."
  (if (functionp all-results-getter)
      (funcall all-results-getter)
    (symbol-value all-results-getter)))

;; TODO Some more interesting implementations of this ^

(cl-defgeneric easi-searchable--results (searchable &key query number page)
  "Get a list of results from querying SEARCHABLE with QUERY.

If NUMBER is non-nil, limit the number of results from each
engine in SEARCHABLE to NUMBER. PAGE is the page of results to
get, if results are paginated (1-indexed).")

(defvar easi-default-non-all-results-skip)
(defvar easi-default-non-queryable-skip)

(cl-defmethod easi-searchable--results ((searchable easi-search-engine) &key query number page)
  "Get appropriate getter from SEARCHABLE, and pass QUERY, NUMBER and PAGE.

If QUERY is non-nil, get the \"queryable-results-getter\",
otherwise get \"all-results-getter\".

If QUERY is non-nil, then pass QUERY, the getter, a number
argument, and PAGE to `easi-searchable--query-results'. The number argument
is NUMBER (if non-nil), or the result of
`easi-search-engine-max-results' for SEARCHABLE (if non-nil) or
`easi-default-max-results'. If QUERY is nil, call
`easi-searchable--all-results' with the getter."
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
		  (easi-searchable--query-results query getter
				      :number
				      (or number
					  (easi-search-engine-max-results searchable)
					  easi-default-max-results)
				      :page page)
		(easi-searchable--all-results getter))))
    (mapcar
     (apply-partially #'easi--utils-result-attach-search-engine searchable)
     (if-let (post-proc (easi-search-engine-results-post-processor searchable))
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
	  (easi-search-engine-group-searchables searchable)))
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
   (mapcar #'easi-searchable--results-presenters searchable)))

(cl-defmethod easi-searchable--results-presenters ((searchable symbol))
  "Call recursively on `symbol-value' of SEARCHABLE."
  (easi-searchable--results-presenters (symbol-value searchable)))

(cl-defmethod easi-searchable--results-presenters ((searchable easi-search-engine))
  "Call `easi-search-engine-results-presenters' on SEARCHABLE."
  (easi-search-engine-results-presenters searchable))

(cl-defmethod easi-searchable--results-presenters ((searchable easi-search-engine-group))
  "Get searchables from group SEARCHABLE, and call on that list."
  ;; This works because a list of searchables is a searchables, so
  ;; there is a method for lists, and the value of the :searchables
  ;; slot is always a list.
  (easi-searchable--results-presenters
   (easi-search-engine-group-searchables searchable)))

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
  (easi-search-engine-result-presenters searchable))

(cl-defmethod easi-searchable--result-presenters ((searchable easi-search-engine-group))
  "Get searchables from group SEARCHABLE, and call on that list."
  ;; This works because a list of searchables is a searchables, so
  ;; there is a method for lists, and the value of the :searchables
  ;; slot is always a list.
  (easi-searchable--result-presenters
   (easi-search-engine-group-searchables searchable)))

(defun easi--get-result-presenters (searchable)
  "List all result presenters compatible with SEARCHABLE."
   ;; TODO Do I want to use `cl-delete-duplicates' and test for cases
   ;; of an object and a symbol pointing at the object?
   (delete-dups
    `(,@(easi-searchable--result-presenters searchable)
      ,@easi-default-result-presenters)))

;;;; Getting sorter list

(cl-defgeneric easi--searchable-sorters (searchable)
  "Return a list of sorting functions compatible with SEARCHABLE.")

(cl-defmethod easi--searchable-sorters ((searchable cons))
  "Get a list of sorters compatible with all members of SEARCHABLE."
  (seq-reduce
   #'seq-intersection
   (mapcar #'easi--searchable-sorters (cdr searchable))
   (easi--searchable-sorters (car searchable))))

(cl-defmethod easi--searchable-sorters ((searchable symbol))
  "Call `easi--searchable-sorters' on value of SEARCHABLE."
  (easi--searchable-sorters (symbol-value searchable)))

(cl-defmethod easi--searchable-sorters ((searchable easi-search-engine))
  "Call `easi-search-engine-sorters' on SEARCHABLE."
  (easi-search-engine-sorters searchable))

(cl-defmethod easi--searchable-sorters ((searchable easi-search-engine-group))
  "Get SEARCHABLE's searchables, pass to `easi--searchable-sorters'."
  (easi--searchable-sorters (easi-search-engine-group-searchables searchable)))

(provide 'easi-searchable)
;;; easi-searchable.el ends here
