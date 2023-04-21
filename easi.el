;;; Easi.el --- the Emacs Advanced Searching Interface  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'easi-searchable)
(require 'easi-structured-object-getter)
(require 'easi-result)
(require 'easi-presenter)
(require 'easi-sort)

;;;; Customizables

(defgroup easi nil
  "The Emacs Advanced Searching Interface."
  :group 'emacs
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

Where `engine-foo' and `engine-bar' are `easi-search-engine's, and
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

(defcustom easi-results-default-buffer-name "EASI Results"
  "Default name for EASI buffer displaying collected results.

A string will be used as the buffer name. A function will be
called with three arguments: current searchable, the current
query, and the list of results. It should return a string."
  :group 'easi
  :type '(choice string function))

(defcustom easi-result-default-buffer-name "EASI Current Result"
  "Default name for EASI buffer displaying the current result.

A string will be used as the buffer name. A function will be
called with the current result as its sole argument."
  :group 'easi
  :type '(choice string function))

(defcustom easi-result-default-display-action nil
  "ACTION arg for `display-buffer' when displaying a result buffer."
  :type display-buffer--action-custom-type)

(defcustom easi-default-non-queryable-skip nil
  "How query functions should handle non-queryable searchables.

If non-nil, functions which query searchables will skip non-queryable
ones.

If nil, get all results from the a search-engine's
\"all-results-getter\" and otherwise treat the results the same.

This is only a default. It can be overridden by some functions
which need to specify certain behaviour."
  :group 'easi
  :type '(choice (const :tag "Get all results" nil)
		 (const :tag "Skip" t)))

(defcustom easi-default-non-all-results-skip t
  "How all-results functions should handle query-only searchables.

If t, functions which get all results from searchables will skip
ones which can only be queried (i.e. which do not have a facility
for returning all results -- often because this would not make
sense for the data being queried).

If a string, use that as the query.

Anything else is treated as equivalent to t, though this may
change, so setting to t or a string is recommended.

This is only a default. It can be overridden by some functions
which need to specify certain behaviour."
  :group 'easi
  :type '(choice (string :tag "Use this as a query")
		 (const :tag "Skip" t)))

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

(defvar-local easi-current-result-presenter nil
  "`easi-results-presenter' used in current buffer.")

(defvar easi--saved-window-config nil
  "Stores the window configuration when Easi is called.")

(defun easi--buffer-from-default (default searchable query result)
  "Get an appropriate buffer given DEFAULT.

If DEFAULT is a string, pass it to `generate-new-buffer'. If
DEFAULT is a function, call it with SEARCHABLE, QUERY and RESULT
as args.

Return the result of whatever is done. If DEFAULT is neither a
string nor a function, return nil."
  (cond
   ((stringp default)
    (generate-new-buffer default))
   ((functionp default)
    (funcall default searchable query result))))

(defun easi-quit ()
  "Quit Easi.

Deletes all Easi buffers."
  (interactive nil easi-results-mode easi-result-mode)
  ;; NOTE Checking for each buffer stops us ever passing nil to
  ;; `kill-buffer', which just kills the current buffer.
  (when easi-result-buffer (kill-buffer easi-result-buffer))
  (when easi-results-buffer (kill-buffer easi-results-buffer))
  ;; Restore window configuration to previous state
  (when easi--saved-window-config
    (set-window-configuration easi--saved-window-config)
    ;; Reset stored window config
    (setq easi--saved-window-config nil)))

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

(defun easi-view-result ()
  "Select window of `easi-result-buffer'."
  (interactive)
  (if easi-result-buffer
      (if-let ((window (get-buffer-window easi-result-buffer)))
	  (select-window window)
	(error "Result buffer not displayed in a window"))
    (error "No result buffer")))

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
  "Present RESULTS in BUFFER with PRESENTER.

PRESENTER is either an `easi-results-presenter' or a symbol. If a
symbol this function is just called again with the value of that
symbol.

If an `easi-results-presenter' object, then with BUFFER current,
call each of the functions in the \"before\", then
\"result-printer\", then \"after\" slots, passing RESULT and
BUFFER to each.

RESULTS is a list of results. BUFFER is a buffer to print in."
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
     (easi-results-presenter-after presenter))
    (mapcan #'funcall (easi-results-presenter-hook presenter))))

(defun easi--get-current-result ()
  "Return the result at point."
  (cl-labels ((get-getter (presenter)
		(if (symbolp presenter)
		    (get-getter (symbol-value presenter))
		  (easi-results-presenter-current-result-getter presenter))))
    (funcall (get-getter easi-current-results-presenter))))

;;;;; (Current) Result

(defun easi-view-results ()
  "Select window of `easi-results-buffer'."
  (interactive)
  (if easi-results-buffer
      (if-let ((window (get-buffer-window easi-results-buffer)))
	  (select-window window)
	(error "Results buffer not displayed in a window"))
    (error "No results buffer")))

(defvar-keymap easi-result-mode-map
  :parent easi-base-map
  "w" #'easi-view-results)

(define-minor-mode easi-result-mode
  "Minor mode for viewing a single EASI result.

Turned on automatically in EASI result buffers. This mode exists
to ensure consistency of various features between different
result presenters, like rerunning queries and switching between
different presenters.")

(defun easi--result-present (result &rest slots)
  "(maybe) Display RESULT in a buffer in appropriate way.
Use `easi-get-result-presenters' to get a list of result
presenters compatible with RESULT, and treat the first one as
default. If that is nil, then bury any current result
buffer (`easi-result-buffer') with `quit-restore-window' and
return nil. If non-nil, then:
- get a result buffer (either `easi-result-buffer' or a new one, with
  `easi--buffer-from-default')
- switch to result buffer
- For each one of SLOTS, get the value of that slot in the presenter,
  and map over it, calling each element as a function, passing RESULT
  and the buffer as arguments. SLOTS are symbols, the names of slots
  in a `easi-result-presenter' object. The only slots which make sense
  for this function are `before', `field-printer' and `after'. As a
  special case, if slot is `hook', call each element with no args.
- return the buffer

This function assumes it is called from a buffer where
`easi-current-query' is non-nil (i.e. a results buffer, or a
previously setup result buffer). Getting this value is the first
thing the function does, so you need not worry about retaining it
in later operations."
  (let* ((query easi-current-query)
	 (searchable (easi-result-retrieve-search-engine result))
	 ;; Resolve presenter as symbol
	 ;; FIXME This is a really ugly and should be altered.
	 (presenter
	  (cl-loop with pres = (car (easi-get-result-presenters searchable))
		   ;; Checking presenter is non-nil stops loop hanging
		   while (and pres (symbolp pres))
		   do (setq pres (symbol-value pres))
		   finally return pres))
	 (result-buffer
	  ;; Reuse existing buffer if it exists
	  (or easi-result-buffer
	      ;; Otherwise only make a new buffer if we are going to
	      ;; use it (i.e. when presenter is non-nil).
	      (when presenter
		(easi--buffer-from-default
		 easi-result-default-buffer-name
		 searchable query result)))))
    (if presenter
	;; Non-nil presenter -- present result accordingly
	(with-current-buffer result-buffer
	  (dolist (slot slots)
	    (let ((accessor
		   (intern
		    (concat "easi-result-presenter-"
			    (symbol-name slot)))))
	      (if (eq slot 'hook)
		  (mapcan #'funcall (funcall accessor presenter))
		(mapcan
		 (lambda (fun) (funcall fun result result-buffer))
		 (funcall accessor presenter)))))
	  ;; Set mode in result buffer
	  (easi-result-mode)
	  ;; Set important values in result-buffer
	  (setq-local easi-current-query query
		      easi-current-searchables searchable
		      easi-result-buffer result-buffer
		      easi-current-result-presenter presenter)
	  ;; Display result buffer
	  (display-buffer result-buffer
			  (or (easi-result-presenter-display-action presenter)
			      easi-result-default-display-action))
	  ;; Return result-buffer
	  result-buffer)
      ;; nil presenter -- don't present result, but do hide any
      ;; previously-presented results
      ;; TODO Should this behaviour be controlled with a user variable?
      ;; NOTE We have to get and check the window separately like
      ;; this, because if there is no result buffer window, then
      ;; `window' will be nil, and passing nil to
      ;; `quit-restore-window' just quits the current window---so the
      ;; results buffer would often be buried.
      (when-let ((result-buffer)
		 (window (get-buffer-window result-buffer)))
	(quit-restore-window window)
	;; Return nil -- no (active/useful) result buffer
	nil))))

(defun easi--update-result ()
  "Update Easi's result buffer to display the current result."
  (pcase-let ((`(,result ,result-buffer)
	       (with-current-buffer easi-results-buffer
		 `(,(easi--get-current-result)
		   ,easi-result-buffer))))
    (easi--result-present
     easi-current-result-presenter
     result result-buffer
     'field-printer)))

;;;; Search functions and entry points

(defun easi--prompt-for-searchable ()
  "Get a searchable."
  (funcall easi-searchable-prompter
	   (or easi-searchables
	       (user-error "No searchables available. Consider setting `easi-searchables'"))))

(defun easi--prompt-for-query (searchable)
  "Prompt user for query for SEARCHABLE.

Get suggestions from SEARCHABLE and present them, reading a query
with `completing-read'."
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

(defun easi-present-results (searchable raw-results &optional query)
  "Present RAW-RESULTS from SEARCHABLE.

Main user-interface driver function for Easi.

Uses `easi-results-buffer' to list the results if it exists, or
creates a new buffer according to
`easi-results-default-buffer-name' if not. Similarly for
`easi-result-buffer' and `easi-results-default-buffer-name' for
displaying the selected result.

RAW-RESULTS is an unsorted list of result objects which Easi can
handle. It is sorted using the result of
`easi--sort-get-searchable-sorter'. The results are then printed
using an appropriate presenter. If all the results are from
search engines which specify particular presenters, then the
first presenter to specified by all of those searchables is used.
Otherwise the first in `easi-default-results-presenters' is used.

`easi-results-mode' is always active in the results buffer.

The currently selected result is printed using either the first
presenter specified by the search engine it comes from, or (if
the engine does not specify any presenters) the first in
`easi-default-result-presenters'.

`easi-result-mode' is always active in the result buffer."
  (let* ((results (easi-sort-results
		   (easi--sort-get-searchable-sorter searchable)
		   raw-results query))
	 (results-buffer
	  (cond
	   (easi-results-buffer)
	   ((stringp easi-results-default-buffer-name)
	    (generate-new-buffer easi-results-default-buffer-name))
	   ;; MAYBE Users might want to set the results-buffer name
	   ;; depending on the results (e.g. on how many there
	   ;; are). Do we want to update the results-buffer name on
	   ;; this basis even we reuse the results-buffer (e.g. if they
	   ;; rerun, reusing the results-buffer, with a new query and
	   ;; there are a different number of results)
	   ((functionp easi-results-default-buffer-name)
	    (funcall easi-results-default-buffer-name
		     searchable query results))))
	 (results-presenter (car (easi-get-results-presenters searchable))))
    (setq easi--saved-window-config (current-window-configuration))
    ;; TODO This might have to move, so that I can run easi-search
    ;; from inside the result buffer...
    ;; TODO Hard coding this is going to make it difficult to do
    ;; different types of rerunning...
    (switch-to-buffer results-buffer)
    (easi--print-results results-presenter results results-buffer)
    ;; NOTE Doing this before printing results didn't work because the
    ;; printing somehow set them all to nil again. I don't know why
    ;; and I should probably sort out what was wrong, but for now,
    ;; I'll just leave them here.
    (setq-local easi-current-query query
		easi-current-searchables searchable
		easi-results-buffer results-buffer
		easi-current-results-presenter results-presenter)
    (easi-results-mode)
    ;; Don't assume that there is a result presenter
    (when-let ((result-presenter (car (easi-get-result-presenters searchable)))
	       (result (easi--get-current-result))
	       ;; Get a results results-buffer in a similar way to above
	       (result-buffer
		(cond
		 (easi-result-buffer)
		 ((stringp easi-result-default-buffer-name)
		  (generate-new-buffer easi-result-default-buffer-name))
		 ((functionp easi-result-default-buffer-name)
		  (funcall easi-result-default-buffer-name
			   searchable query result)))))
      ;; TODO This is wrong -- we assume that a symbol points at a
      ;; struct, but the definition says that a symbol could point at
      ;; a symbol or a struct, so long as the chain ends in a struct.
      (let ((action (or (easi-result-presenter-display-action
			 (if (symbolp result-presenter)
			     (symbol-value result-presenter)
			   result-presenter))
			easi-result-default-display-action)))
	(display-buffer result-buffer action))
      (easi--result-present
       result-presenter result result-buffer
       'before 'field-printer 'after 'hook)
      (with-current-buffer result-buffer
	(easi-result-mode)
	(setq-local easi-current-query query
		    easi-current-searchables searchable
		    easi-result-buffer result-buffer
		    easi-results-buffer results-buffer
		    easi-current-results-presenter results-presenter
		    easi-current-result-presenter result-presenter))
      ;; Set variables in results buffer pointing at result buffer
      (with-current-buffer results-buffer
	(setq-local easi-current-result-presenter result-presenter
		    easi-result-buffer result-buffer)))))

;;;###autoload
(defun easi-all (searchable)
  "Display all results from SEARCHABLE.

Interactively, prompt for SEARCHABLE.

Results will be retrieved using the contents of the
\"all-results-getter\" slot in the search engine(s) referenced by
SEARCHABLE. If this slot is nil, behaviour is controlled by
`easi-default-non-all-results-skip'."
  (interactive `(,(easi--prompt-for-searchable)))
  (let ((raw-results (easi-searchable-results searchable)))
    (easi-present-results searchable raw-results)))

;;;###autoload
(defun easi-search (searchable query)
  "Search for QUERY in SEARCHABLE, and display results.

Interactively, prompt for SEARCHABLE and QUERY if they are not
passed. Results will be retrieved using the contents of the
\"queryable-results-getter\" slot in the search engine(s)
referenced by SEARCHABLE. If this slot is nil, behaviour is
controlled by `easi-default-non-queryable-skip'."
  (interactive (let* ((searchable (easi--prompt-for-searchable))
		      (query (easi--prompt-for-query searchable)))
		 `(,searchable ,query)))
  ;; TODO Is there a place to get limiting `number' arguments for
  ;; these functions?
  (let ((raw-results (easi-searchable-results searchable query)))
    (easi-present-results searchable raw-results query)))
 
;;;###autoload
(defun easi-rerun-with-new-engines (searchable)
  "Run last query again, but against a different SEARCHABLE.

Interactively, prompt for SEARCHABLE with
`easi--prompt-for-searchable'."
  (interactive `(,(easi--prompt-for-searchable))
	       easi-results-mode easi-result-mode)
  (easi-search searchable easi-current-query))

;;;###autoload
(defun easi-rerun-with-new-query (query)
  "Run new QUERY on the same search engines as previously used.

Interactively, prompt for QUERY with `easi--prompt-for-query',
passing `easi-current-searchables' as argument."
  (interactive `(,(easi--prompt-for-query easi-current-searchables))
	       easi-results-mode easi-result-mode)
  (easi-search easi-current-searchables query))

;;; Examples (not part of infrastructure) (to be eventually removed)

;; TODO This is awful (and should probably live somewhere else!)
(defun easi--completing-read-multiple-searchables (searchables)
  "Read multiple SEARCHABLES from `easi-searchables'."
  (cl-labels ((get-name (searchable)
		;; TODO Surely there is a more elegant way to do this!! (generics?)
		(pcase (type-of searchable)
		  ('symbol (get-name (symbol-value searchable)))
		  ('easi-search-engine (easi-search-engine-name searchable))
		  ('easi-search-engine-group
		   (easi-search-engine-group-name searchable))))
	      (name-prop (searchable)
		(propertize (get-name searchable)
			    'easi-searchable searchable)))
    (let* ((s-list (flatten-list searchables))
	   (name-s-alist (mapcar #'name-prop s-list))
	   (minibuffer-allow-text-properties t)
	   (selected
	    (completing-read-multiple
	     "Searchables: " name-s-alist nil t)))
      ;; This is a HACK
      (mapcar
       (lambda (str)
	 (seq-find
	  (lambda (searchable) (string= str (get-name searchable)))
	  searchables))
       selected))))

(provide 'easi)

;;; easi.el ends here
