;;; easi.el --- the Emacs Advanced Searching Interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Hugo Heagren

;; Author: Hugo Heagren <hugo@undertown>
;; Keywords:hypermedia, matching

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

;; My attempt at an advanced federated search facility in Emacs.

;; A searchable is one of
;; - easi-search-engine
;; - easi-search-engine-group
;; - a list of searchables
;; - a symbol whose value is a searchable ;; PROG enforce this
;; - nothing else is a searchable
;;
;; The API for searchables currently includes:
;; `easi-searchable-suggestions' and `easi-searchable-results'.


;;; Code:

(require 'cl-lib)
(require 'seq)

;;;; Customizables

(defgroup easi nil
  "The Emacs Advanced Searching Interface."
  ;; :group ;; TODO
  :prefix "easi")

;; TODO Write a better default (current one is TEMP)
(defcustom easi-searchable-prompter #'easi--completing-read-multiple-searchables
  "Function to select searchables.

Should take a list of searchables, and prompt, returning a list
of those selected."
  :group 'easi
  :type 'function)

;; TODO Define a proper custom type for this.
(defcustom easi-searchables nil
  "List of available searchables.

This list is used to prompt the user in `easi-search'.

A searchable is anything which is:
- an `easi-search-engine'
- an `easi-search-engine-group'
- a list of searchables (i.e. a list of the above, or an arbitrarily
  nested list of such objects)
- a symbol whose value is a valid searchable

Where `engine-foo' and `engine-bar' are `easi-search-engines', and
`group-foo' and `group-bar' are easi-search-engine-groups, all the
following are valid elements of this list:
- engine-foo
- (engine-foo engine-bar)
- (group-foo (engine-foo group-bar))
- (engine-foo ((engine-foo) engine-bar) (group-foo group-bar))

Notice that one valid element (in the last case `engine-foo' can
appear more than once. This will have no effect on the user
experience)."
  :group 'easi)

(defcustom easi-default-results-presenters nil
  "List of results presenters which are always available.

These are assumed to be compatible with every searchable on the
machine. Presenters which are only compatible with some engines
should be set in the \"results-presenters\" slot of those
engines."
  :group 'easi)

(defcustom easi-default-result-presenters nil
  "List of result presenters which are always available.

These are assumed to be compatible with every searchable on the
machine. Presenters which are only compatible with some engines
should be set in the \"result-presenters\" slot of those
engines.")

(defcustom easi-results-default-buffer-name "EASI Results"
  "Default name for EASI buffer displaying collected results.

A string will be used as the buffer name. A function will be
called with three arguments: current searchable, the current
query, and the list of results. It should return a string."
  :group 'easi
  :type '(choice string function))

;;;; Basic searchable

(cl-defstruct (easi-search-engine
	       (:constructor easi-search-engine-create))
  "A single atomic search engine.

KEY is a " ;; TODO Better docstring
  (key nil
   (:type key
    :documentation "Key used for selection"))
  (name ""
   (:type string))
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
  (results-getter nil
   (:documentation
    "Way of getting a list of results. Must be a of a type for which
`easi-get-results' has a method."))
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
    `easi-default-result-presenters'.)")))

(cl-defstruct (easi-search-engine-group
	       (:constructor easi-search-engine-group-create))
  ;; TODO Better docs
  "A group of search-engines."
  (key (:type key
	:documentation "Key used for selection"))
  (name (:type string))
  (searchables
   :type list
   :documentation
   "List of searchables.
Technically, this is itself a searchable, but naming it this way makes
more sense."))

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
`years' could be accessed with the list (\"age\" \"years\").)")
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
	(easi--structured-object-get-field (cdr field) with-first)
      with-first)))

;; TODO Allow FIELD to be a lambda, not just a symbol for a function
;; The easiest way to do this might be to just fix the Emacs bug with
;; cl-lib---other types are meant to be supported.
(cl-defmethod easi--structured-object-get-field ((field symbol) object)
  "FIELD is a "
  (funcall field object))
(cl-defmethod easi--structured-object-get-field ((field compiled-function) object)
  "FIELD is a "
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
  (cond
   ((cl-every #'consp object)
    (alist-get field object nil nil #'string=))
   ((plistp object)
    (plist-get
     field object
     (lambda (x y)
       (or
	(eq x y)
	(eq (intern x) y)))))))

(cl-defmethod easi--structured-object-get-field (field (object hash-table))
  (gethash field object))

;; TODO Write a few other obvious methods for different types of
;; object (alist, plist, cl-struct, hash-table, maybe json)

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
then the call is (easi--structured-object-get-field PROC RAW).")

(cl-defmethod easi-searchable-suggestions (query (searchable easi-search-engine) &optional number)
  (when-let ((getter (easi-search-engine-suggestions-getter searchable))
	     (raw-results (easi-get-suggestions query getter number)))
    (if-let (post-proc (easi-search-engine-suggestion-post-processor searchable))
	(easi--structured-object-get-field post-proc raw-results)
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

(cl-defgeneric easi-get-results (query results-getter &optional number)
  "Get a list of by querying RESULTS-GETTER with QUERY.

QUERY is always string. If NUMBER is non-nil, no more than NUMBER
results should be returned.")

;; Simplest case
(cl-defmethod easi-get-results (query (results-getter symbol) &optional number)
  "RESULTS-GETTER is a function."
  (funcall results-getter query number))

;; TODO Some more interesting implementations of this ^

(cl-defgeneric easi-searchable-results (query searchable &optional number)
  "Get a list of results from querying SEARCHABLE with QUERY.

If NUMBER is non-nil, limit the number of results from each
engine in SEARCHABLE to NUMBER.")

(cl-defmethod easi-searchable-results (query (searchable easi-search-engine) &optional number)
  (easi-get-results query (easi-search-engine-results-getter searchable) number))
(cl-defmethod easi-searchable-results (query (searchable easi-search-engine-group) &optional number)
  (mapcar (apply-partially #'easi-searchable-results query)
	  (easi-search-engine-group-searchables searchable)))
(cl-defmethod easi-searchable-results (query (searchable cons) &optional number)
  "SEARCHABLE is a list."
  (flatten-list
   (mapcar (apply-partially #'easi-searchable-results query) searchable)))
(cl-defmethod easi-searchable-results (query (searchable symbol) &optional number)
  (easi-searchable-results query (symbol-value searchable) number))

;;;; Programmatic interaction with results

(cl-defgeneric easi-result-list-fields (result)
  "Return a list of fields in RESULT.

Each field returned must be a string.")

(defalias 'easi-result-get-field
  'easi--structured-object-get-field)

;;;; Presenters

;; TODO These need to be of the same type, or class, or whatever, so
;; that it is possible to define a results printer which is also a
;; result printer (this will enable e.g. three-column file layouts)
(cl-defstruct (easi-results-presenter
	       (:constructor easi-results-presenter-create))
  name key
  ;; ALL the following are hooks, passed two args: the results list, a
  ;; buffer to display results in (if they want).
  before ;; Only run once
  result-printer
  after ;; Only run once

  ;; For getting current result. Should return a result object (i.e.
  ;; not necessarily a string)
  current-result-getter)

(cl-defstruct (easi-result-presenter
	       (:constructor easi-result-presenter-create))
  name key
  ;; ALL the following are hooks, passed two args: the results list, a
  ;; buffer to display results in (if they want).
  before ;; Only run once
  field-printer
  after ;; Only run once

  ;; For getting current field. Should return a result object (i.e.
  ;; not necessarily a string)
  current-field-getter)

;;;;; Getting Presenters

(cl-defgeneric easi-searchable-results-presenters (searchable)
  "Return a list of results presenters supported by SEARCHABLE.")

(cl-defmethod easi-searchable-results-presenters ((searchable cons))
  (delete-dups
   (mapcar #'easi-searchable-results-presenters searchable)))

(cl-defmethod easi-searchable-results-presenters ((searchable symbol))
  (easi-searchable-results-presenters (symbol-value searchable)))

(cl-defmethod easi-searchable-results-presenters ((searchable easi-search-engine))
  (easi-search-engine-results-presenters searchable))

(cl-defmethod easi-searchable-results-presenters ((searchable easi-search-engine-group))
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
  (delete-dups
   (mapcar #'easi-searchable-result-presenters searchable)))

(cl-defmethod easi-searchable-result-presenters ((searchable symbol))
  (easi-searchable-result-presenters (symbol-value searchable)))

(cl-defmethod easi-searchable-result-presenters ((searchable easi-search-engine))
  (easi-search-engine-result-presenters searchable))

(cl-defmethod easi-searchable-result-presenters ((searchable easi-search-engine-group))
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

;;;; Results user interface
;; TODO Make it possible to have more than one EASI results
;; buffer/session at once.

;; Infrastructure variables
(defvar-local easi-current-query nil
  "Query which produced current buffer's EASI results.")

(defvar-local easi-current-searchables nil
  "Searchables which produced current buffer's EASI results.")

(defvar-local easi-results-buffer nil
  "Buffer displaying collection of EASI results.")

(defvar-local easi-result-buffer nil
  "Buffer displaying current EASI result.")

(defvar-local easi-current-results-presenter nil
  "`easi-results-presenter' used in current buffer.")

;; Define commands useful in every presenter
;; TODO Define lots of these commands...
(defvar-keymap easi-base-map
  ;; TODO Do I want different commands for changing just
  ;; query/engines?
  "r" #'easi-rerun-with-new-query
  "R" #'easi-rerun-with-new-engines
  "s" #'easi-search
  "q" #'easi-quit)

;;;;; Results

;; TODO This will need to be more complex in future (kill both
;; buffers, if more than one exist)
(defun easi-quit ()
  (interactive nil easi-results-mode easi-result-mode)
  (kill-this-buffer))

(defvar-keymap easi-results-mode-map
  :parent easi-base-map
  "w" #'easi-view-result)

;; ;; TODO Enforce minor mode conventions (see info node (elisp)Minor
;; ;; Mode Conventions)
(define-minor-mode easi-results-mode
  "Minor mode for viewing a collection of EASI results.

Turned on automatically in EASI results buffers. This mode exists
to ensure consistency of various features between different
results presenters, like rerunning queries and switching between
different presenters.")

(defun easi--print-results (presenter results buffer)
  (if (symbolp presenter)
      ;; Account symbols-as-presenters
      (easi--print-results (symbol-value presenter) results buffer)
  (mapcan
   (lambda (fun) (funcall fun results buffer))
   (easi-results-presenter-before presenter))
  (mapcan
   (lambda (fun) (funcall fun results buffer))
   (easi-results-presenter-result-printer presenter))
  (mapcan
   (lambda (fun) (funcall fun results buffer))
   (easi-results-presenter-after presenter))))

(defun easi--get-current-result ()
  "Return the result at point."
  (cl-labels ((get-getter (presenter)
		(if (symbolp presenter)
		    (get-getter (symbol-value presenter))
		  (easi-results-presenter-current-result-getter presenter))))
    (funcall (get-getter easi-current-results-presenter))))

;;;;; (Current) Result

(defvar-keymap easi-result-mode-map
  :parent easi-base-map
  "w" #'easi-view-results)

(define-minor-mode easi-result-mode
  "Minor mode for viewing a single EASI result.

Turned on automatically in EASI result buffers. This mode exists
to ensure consistency of various features between different
result presenters, like rerunning queries and switching between
different presenters.")

;;;; Search functions

(defun easi--prompt-for-searchable ()
  "Get a searchable."
  (funcall easi-searchable-prompter
	   (or easi-searchables
	       (user-error "No searchables available. Consider setting `easi-searchables'"))))

(defun easi--prompt-for-query (searchable)
  ;; TODO More flexible sorting of suggestions
  ;; TODO Is this right? (does it work if the suggestions change?)
  ;; Test it with a google/youtube suggestions api
  ;; TODO Can (should?) this be replaced with
  ;; `completion-table-with-cache'?
  ;; Allow for use of text properties, to enable e.g. embark usage
  (let ((minibuffer-allow-text-properties t))
    (completing-read
     "Search: "
     (completion-table-dynamic
      (lambda (str) (easi-searchable-suggestions str searchable))))))

;;;###autoload
(defun easi-search (searchable query)
  "Search for QUERY in SEARCHABLE, and display results.

Interactively, prompt for SEARCHABLE and QUERY if they are not
passed."
  ;; TODO Is there a place to get limiting `number' arguments for
  ;; these functions?
  (interactive (let* ((searchable (easi--prompt-for-searchable))
		      (query (easi--prompt-for-query searchable)))
		 `(,searchable ,query)))
  (let* ((results (easi-searchable-results query searchable))
	 (buffer (cond
		  (easi-results-buffer)
		  ((stringp easi-results-default-buffer-name)
		   (generate-new-buffer easi-results-default-buffer-name))
		  ;; MAYBE Users might want to set the buffer name
		  ;; depending on the results (e.g. on how many there
		  ;; are). Do we want to update the buffer name on
		  ;; this basis even we reuse the buffer (e.g. if they
		  ;; rerun, reusing the buffer, with a new query and
		  ;; there are a different number of results)
		  ((functionp easi-results-default-buffer-name)
		   (funcall easi-results-default-buffer-name
			    searchable query results))))
	 (results-presenter (car (easi-get-results-presenters searchable))))
    ;; TODO This might have to move, so that I can run easi-search
    ;; from inside the result buffer...
    ;; TODO Hard coding this is going to make it difficult to do
    ;; different types of rerunning...
    (switch-to-buffer buffer)
    (easi--print-results results-presenter results buffer)
    ;; NOTE Doing this before printing results didn't work because the
    ;; printing somehow set them all to nil again. I don't know why
    ;; and I should probably sort out what was wrong, but for now,
    ;; I'll just leave them here.
    (setq-local easi-current-query query
		easi-current-searchables searchable
		easi-results-buffer buffer
		easi-current-results-presenter results-presenter)
    (easi-results-mode)
    (when-let (;; Get a results buffer in a similar way to above
	       (result-presenter (car (easi-get-result-presenters searchable))))
      ;; TODO Some way of configuring how the buffer is displayed

      ;; Display the buffer

      ;; Run a function which in a generic way updates the result
      ;; presenter to display the current result
      )))

;;;###autoload
(defun easi-rerun-with-new-engines (searchable)
  (interactive `(,(easi--prompt-for-searchable))
	       easi-results-mode easi-result-mode)
  (easi-search searchable easi-current-query))

;;;###autoload
(defun easi-rerun-with-new-query (query)
  (interactive `(,(easi--prompt-for-query easi-current-searchables))
	       easi-results-mode easi-result-mode)
  (easi-search easi-current-searchables query))

;;; Examples (not part of infrastructure)

;; TODO This is awful (and should probably live somewhere else!)
(defun easi--completing-read-multiple-searchables (searchables)
  ""
  (cl-labels ((get-name (searchable)
		;; TODO Surely there is a more elegant way to do this!! (generics?)
		(pcase (type-of searchable)
		  ('symbol (get-name (symbol-value searchable)))
		  ('easi-search-engine (easi-search-engine-name searchable))
		  ('easi-search-engine-group
		   (easi-search-engine-group-name searchable))))
	      (name-prop (searchable)
		(propertize (get-name searchable)
			    'easi-searchable
			    (if (symbolp searchable)
				(symbol-value searchable)
			      searchable))))
    (let* ((s-list (flatten-list searchables))
	   (name-s-alist (mapcar #'name-prop s-list))
	   (minibuffer-allow-text-properties t)
	   (selected
	    (completing-read-multiple
	     "Searchables: " name-s-alist nil t)))
      (mapcar (lambda (str) (get-text-property 0 'easi-searchable str)) selected))))

(provide 'easi)

;;; easi.el ends here
