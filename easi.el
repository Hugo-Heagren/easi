;;; easi.el --- The Emacs Advanced Searching Interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Hugo Heagren

;; Author: Hugo Heagren <hugo@heagren.com>
;; Keywords:hypermedia, matching
;; Version: 0.1.0
;; URL: https://github.com/Hugo-Heagren/easi
;; Package-Requires: ((emacs "27.1"))

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
(require 'easi-presentable)
(require 'easi-presenter)
(require 'easi-sort)
(require 'easi-utils)
(require 'easi-session)
(require 'thread)

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

(define-widget 'easi-searchable 'lazy
  "Widget for customising `easi-searchables'."
  :tag "Easi searchable"
  :type '(choice (restricted-sexp :tag "Search engine"
				  :match-alternatives (easi-search-engine-p))
		 (restricted-sexp :tag "Search engine group"
				  :match-alternatives (easi-search-engine-group-p))
		 symbol
		 (repeat easi-searchable)))

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
  :group 'easi
  :type 'easi-searchable)

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

(defcustom easi-default-max-results 10
  "Default maximum number of results to get.

Used as a default NUMBER argument in functions such as
`easi-searchable--results'."
  :group 'easi
  :type 'integer)

(defcustom easi-default-max-suggestions 10
  "Default maximum number of suggestions to get.

Used as a default NUMBER argument in functions such as
`easi-searchable--suggestions'."
  :group 'easi
  :type 'integer)

(defcustom easi-next-page-sorting-strategy 'merge
  "How to handle new results in `easi-get-next-page'.

`easi-get-next-page' gets a new set of results and combines them
with the preexisting results. If this variable is set to
`append', then each new set of results is first sorted, then
appended to the preexisting results. If set to `merge', then the
new results are first appended, and then the new full list of
results is sorted.

Behaviour is undefined for other values.

The default value of `merge' is probably better most of the time,
though `append' probably takes less computing power."
  :group 'easi
  :type '(choice (const append)
		 (const merge)))


;;;; Results user interface

(defun easi-quit ()
  "Quit current Easi buffer."
  (interactive nil easi-results-mode easi-result-mode)
  (when (easi-session--get-current)
    (kill-buffer (current-buffer))))

(defun easi-quit-session (&optional session)
  "Kill Easi session SESSION.

Delete all buffers in the session, restore any saved window
config, and remove the session from `easi-session-list'.

If SESSION is not specified, default to current session."
  (interactive `(,(easi-session--get-current))
   easi-results-mode easi-result-mode)
  (let* ((window-config (easi-session-state-window-config session)))
    (dolist (buf `(,@(easi-session-state-results-buffers session)
		   ,@(easi-session-state-result-buffers session)))
      (when buf (kill-buffer buf)))
    (when window-config
      (set-window-configuration window-config))
    (setq easi-session-list (delq session easi-session-list))))

;; Define commands useful in every presenter
;; TODO Define lots of these commands...
(defvar-keymap easi-base-map
  ;; TODO Do I want different commands for changing just
  ;; query/engines?
  "N" #'easi-get-next-page
  "r" #'easi-rerun-with-new-query
  "R" #'easi-rerun-with-new-engines
  "s" #'easi-search
  "q" #'easi-quit
  "Q" #'easi-quit-session)

;;;;; Results

(defun easi-view-result ()
  "Select window of current result."
  (interactive)
  (let* ((session (easi-session--get-current))
	 (buf-ls (easi-session-state-result-buffers session))
	 (buf1 (car buf-ls))
	 (buf (cond
	       ((null buf1) (error "No result buffer(s)"))
	       ((cdr buf-ls)
		(read-buffer
		 "Result buffer: " nil 'require-match
		 (lambda (buffer) (memq (cdr buffer) buf-ls))))
	       (t buf1))))
    (if-let ((window (get-buffer-window buf)))
	(select-window window)
      (error "Result buffer not displayed in a window"))))

(defun easi-results-next (&optional n)
  "Go to next result in all results buffers in EASI session.

With optional prefix arg N, go to Nth next. If N is negative, go
to previous result."
  (interactive "p")
  (let* ((session (easi-session--get-current))
	 (list (easi-session-state-results-buffers session))
	 (pres-alist (easi-session-state-buffer-presenters
		      session)))
    (dolist (buf list)
      (with-current-buffer buf
	(easi-presenter--next-result
	 (alist-get buf pres-alist #'equal)
	 n)
	;; For some reason, the `window-point' in the other buffers is
	;; not kept properly in sync with the buffer point, so we have
	;; to set it manually. Hfmsk.
	(set-window-point (get-buffer-window buf) (point)))))
  (easi--update-result))

(defun easi-results-previous (&optional n)
  "Go to previous result in all results buffers in EASI session.

With optional prefix arg N, go to Nth previous. If N is negative,
go to next result."
  (interactive "p")
  (easi-results-next (- (or n 1))))

(defun easi-presenter-first-result ()
  "Go to first result."
  (interactive)
    (let* ((session (easi-session--get-current))
	 (list (easi-session-state-results-buffers session))
	 (pres-alist (easi-session-state-buffer-presenters
		      session)))
    (dolist (buf list)
      (with-current-buffer buf
	(easi-presenter--first-result
	 (alist-get buf pres-alist #'equal))
	;; For some reason, the `window-point' in the other buffers is
	;; not kept properly in sync with the buffer point, so we have
	;; to set it manually. Hfmsk.
	(set-window-point (get-buffer-window buf) (point))))))

(defun easi-presenter-last-result ()
  "Go to last result."
  (interactive)
    (let* ((session (easi-session--get-current))
	 (list (easi-session-state-results-buffers session))
	 (pres-alist (easi-session-state-buffer-presenters
		      session)))
    (dolist (buf list)
      (with-current-buffer buf
	(easi-presenter--last-result
	 (alist-get buf pres-alist #'equal))
	;; For some reason, the `window-point' in the other buffers is
	;; not kept properly in sync with the buffer point, so we have
	;; to set it manually. Hfmsk.
	(set-window-point (get-buffer-window buf) (point))))))

(defvar-keymap easi-results-mode-map
  :parent easi-base-map
  "w" #'easi-view-result
  "n" #'easi-results-next
  "p" #'easi-results-previous
  "M-<" #'easi-presenter-first-result
  "M->" #'easi-presenter-last-result)

(defun easi--kill-buffer-manage-sessions ()
  "For use in `kill-buffer-hook'.

Get the current buffer, and remove it from all lists in the
current session. If it was the last buffer in that session, then
call `easi-quit-session', passing the session."
  (let* ((session (easi-session--get-current))
	 (buffer (current-buffer))
	 (new-results-buffers (delq buffer (easi-session-state-results-buffers session)))
	 (new-result-buffers (delq buffer (easi-session-state-result-buffers session))))
    ;; remove buffer from session
    (setf (easi-session-state-results-buffers session)
	  new-results-buffers)
    (setf (easi-session-state-result-buffers session)
	  new-result-buffers)
    (setf (alist-get
	   buffer
	   (easi-session-state-buffer-presenters session)
	   nil 'remove)
	  nil)
    ;; If session now empty, delete it
    (unless (or new-results-buffers new-result-buffers)
      (easi-quit-session session))))

;; ;; TODO Enforce minor mode conventions (see info node (elisp)Minor
;; ;; Mode Conventions)
(define-minor-mode easi-results-mode
  "Minor mode for viewing a collection of EASI results.

Turned on automatically in EASI results buffers. This mode exists
to ensure consistency of various features between different
results presenters, like rerunning queries and switching between
different presenters."
  :interactive nil
  (add-hook 'kill-buffer-hook 'easi--kill-buffer-manage-sessions nil 'local))

(cl-defun easi--print (session &key
			       (printable-or-thread
				(easi-session-state-results-thread session))
                               (slots '(before printer after hook))
                               result-or-results)
  "Print a results or list of results in all relevant buffers.

PRINTABLE is a single result, a list of results or a thread. The
relevant presenter should be prepared to handle this. PRINTABLE
defaults results thread in SESSION.

Loop over the buffer/presenter pairs in SESSION's list, and for
each relevant one (i.e. all the results or result presenters,
depending on RESULT-OR-RESULTS), call `easi-presentable--print',
passing PRINTABLE, SLOTS and the buffer, then display the buffer
`easi-presentable--display-buffer'."
  (let ((list (cl-case result-or-results
                (result (easi-session-state-result-buffers session))
                (results (easi-session-state-results-buffers session)))))
    ;; Because there is a 1-1 correspondence between buffers and
    ;; presenters, and each buffer is unique, looping over this list
    ;; ensures that we only print for each buffer once, even if some
    ;; presenters are specified more than once (e.g. in multiple
    ;; groups).
    ;; TODO Separate thread for each buffer.
    (cl-loop for (buf . pres)
             in (easi-session-state-buffer-presenters session)
             when (memql buf list)
             do (easi-presentable--print
                 pres session
		 :printable-or-thread printable-or-thread
		 :slots slots
		 :buffer buf)
             and do (easi-presentable--display-buffer
                     buf pres result-or-results)
	     end)))

(defun easi--get-current-result (session)
  "Return the result at point in SESSION."
  (let ((buf-list (easi-session-state-results-buffers session)))
    (if (memq (current-buffer) buf-list)
	(funcall (slot-value (easi-session--current-buffer-presenter session)
		       'current-getter))
      (with-current-buffer (car buf-list)
	(funcall (slot-value (easi-utils--resolve-symbol
			(easi-session--current-buffer-presenter session))
		       'current-getter))))))

;;;;; (Current) Result

(defun easi-view-results ()
  "Select window of current results."
  (interactive)
  (let* ((session (easi-session--get-current))
	 (buf-ls (easi-session-state-results-buffers session))
	 (buf1 (car buf-ls))
	 (buf (cond
	       ((null buf1) (error "No results buffer(s)"))
	       ((cdr buf-ls)
		(read-buffer
		 "Results buffer: " nil 'require-match
		 (lambda (buffer) (memq (cdr buffer) buf-ls))))
	       (t buf1))))
    (if-let ((window (get-buffer-window buf)))
	(select-window window)
      (error "Results buffer not displayed in a window"))))

(defvar-keymap easi-result-mode-map
  :parent easi-base-map
  "w" #'easi-view-results
  "M-N" #'easi-results-next
  "M-P" #'easi-results-previous)

(define-minor-mode easi-result-mode
  "Minor mode for viewing a single EASI result.

Turned on automatically in EASI result buffers. This mode exists
to ensure consistency of various features between different
result presenters, like rerunning queries and switching between
different presenters."
  :interactive nil
  (add-hook 'kill-buffer-hook 'easi--kill-buffer-manage-sessions nil 'local))

(defun easi--present-result (session slots)
  "(maybe) Display current result in a buffer in appropriate way.

SESSION is the current Easi session state object.

Use `easi-searchable--get-result-presenters' to get a list of
result presenters compatible with RESULT, and treat the first one
as default. Ensure buffers are setup correctly SESSION with
`easi-presentable--set-buffers'. Finally call `easi--print',
passing SESSION, SLOTS, the symbol `result', and the presenter."
  (let* ((result (easi--get-current-result session))
	 (result-presenter
	  (car (easi-searchable--get-result-presenters
		(easi-result--retrieve-search-engine result)))))
    (easi-presentable--set-buffers result-presenter session 'result)
    (easi--print
     session
     :printable-or-thread result
     :slots slots
     :result-or-results 'result)))

(defun easi--update-result ()
  "Update Easi's result buffer to display the current result."
  (let* ((session (easi-session--get-current)))
    (easi--present-result session '(printer))))

;;;;; Pagination

(defun easi--get-next-page (session num)
  "Fetch NUM next pages of results SESSION's searchables.

Used internally by `easi-get-next-page'. How new results are
added depends on the value of `easi-next-page-sorting-strategy',
which see."
  (let ((searchable (easi-session-state-searchables session))
	(query (easi-session-state-query session))
	(page (easi-session-state-page session))
	(strategy easi-next-page-sorting-strategy))
    (cl-loop with i = 0
	     until (eq i num)
	     with new-raw-results and new-results
	     do (setq new-raw-results (easi-searchable--results
				       searchable :query query :page (1+ page)))
	     if new-raw-results
	       do (cl-incf (easi-session-state-page session))
	       ;; I *think* this is the `correct' way of doing this,
	       ;; but it isn't perfect. The main thread doesn't
	       ;; propogate signals like all other threads would, so
	       ;; you can't properly signal an error in the main
	       ;; thread from another thread.
	       else
	       do (thread-signal main-thread 'error '("No next page of results"))
	     end ;; end `if new-raw-results'
	     do (setq new-results (if (eql strategy 'append)
				      (easi-sort--results
				       (easi-sort--get-searchable-sorter searchable)
				       new-raw-results query)
				    new-raw-results))
	     append new-results into collected-results
	     do (cl-incf i)
	     ;; This works because `setf' returns the set value.
	     finally return
	     (setf (easi-session-state-results session)
		   (if (eql strategy 'merge)
		       (easi-sort--results
			(easi-sort--get-searchable-sorter searchable)
			`(,@(easi-session-state-results session)
			  ,@collected-results)
			query)
		     `(,@(easi-session-state-results session) ,@collected-results))))))

(cl-defun easi-get-next-page (&optional (num 1))
  "Print NUM next pages of results from session's searchables.

NUM defaults to 1.

How new results are added depends on the value of
`easi-next-page-sorting-strategy', which see."
  (interactive "p" easi-results-mode)
  (when-let* ((session (easi-session--get-current))
	      (results-thread
	       (make-thread
		(lambda () (easi--get-next-page session num))
		"Easi next page results thread")))
    (setf (easi-session-state-results-thread session) results-thread)
    (easi--print session
		 :printable-or-thread results-thread
		 :slots '(printer)
		 :result-or-results 'results)))

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
      (lambda (str) (easi-searchable--suggestions str searchable))))))

(defun easi--present-results (session)
  "Present RAW-RESULTS in SESSION.

Main user-interface driver function for Easi.

RAW-RESULTS is an unsorted list of result objects which Easi can
handle. It is sorted using the result of
`easi-sort--get-searchable-sorter' (called on the searchables in
SESSION). The results are then printed using an appropriate
presenter.

Use `easi-searchable--get-results-presenters' to get a list of result
presenters compatible with RESULTS, and treat the first one as
default.

Then save state to SESSION (the sorted results, and the previous
window state) and ensure SESSION's buffers are setup correctly
with `easi-presentable--set-buffers'.

Finally call `easi--print' to present the results (passing
SESSION, the results list as \"printable\", the symbol `result',
and the results presenter) then `easi--present-result' to present
the current result as appropriate."
  (let (;; NOTE This ensures that the session is linked to buffer we
        ;; are printing into.
        (results-presenter
         (car (easi-searchable--get-results-presenters
               (easi-session-state-searchables session)))))
    ;; TODO Should I save windows earlier, at the initial session definition?
    (setf (easi-session-state-window-config session)
          (current-window-configuration))
    (easi-presentable--set-buffers results-presenter session 'results)
    (easi--print
     session
     ;; Not strictly necessary, but good to be explicit.
     :printable-or-thread (easi-session-state-results-thread session)
     :result-or-results 'results)
    (easi--present-result
     session
     '(before printer after hook))))

;;;###autoload
(defun easi-all (searchable)
  "Display all results from SEARCHABLE.

Interactively, prompt for SEARCHABLE.

Results will be retrieved using the contents of the
\"all-results-getter\" slot in the search engine(s) referenced by
SEARCHABLE. If this slot is nil, behaviour is controlled by
`easi-default-non-all-results-skip'."
  (interactive `(,(easi--prompt-for-searchable)))
  (let* ((session (easi-session--get-create-current))
	 (results-thread
	  (make-thread
	   ;; TODO Name thread in accordance with sessions?
	   (lambda () (easi--get-results session searchable))
	   "Easi results")))
    (setf (easi-session-state-results-thread session) results-thread)
    (setf (easi-session-state-searchables session) searchable)
    (make-thread
     (lambda ()
       (easi--present-results session))
     "Easi presentation")))

(defun easi--get-results (session searchable)
  "Get and store results for SEARCHABLE in SESSION.

Get results from SESSION with SEARCHABLE, clean them with
`easi-sort--results'. Finally, store the clean results in SESSION
and return the clean results."
  (let* ((query (easi-session-state-query session))
	 (raw-results (easi-searchable--results
		       searchable
		       :page (easi-session-state-page session)
		       :query query))
	 (clean-results
	  (easi-sort--results
	   (easi-sort--get-searchable-sorter
	    (easi-session-state-searchables session))
	   raw-results query)))
    ;; Store results in SESSION
    (setf (easi-session-state-results session) clean-results)))

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
  (let* ((session (easi-session--get-create-current))
	 ;; We need to set these before getting results
	 (_ (setf (easi-session-state-query session) query))
	 (_ (setf (easi-session-state-searchables session) searchable))
	 (results-thread
	  (make-thread
	   (lambda () (easi--get-results session searchable))
	   "Easi results")))
    (setf (easi-session-state-results-thread session) results-thread)
    (make-thread
     (lambda () (easi--present-results session))
     "Easi presentation")))
 
;;;###autoload
(defun easi-rerun-with-new-engines (searchable)
  "Run last query again, but against a different SEARCHABLE.

Interactively, prompt for SEARCHABLE with
`easi--prompt-for-searchable'."
  (interactive `(,(easi--prompt-for-searchable))
	       easi-results-mode easi-result-mode)
  (if-let ((query (easi-session-state-query
		   (easi-session--get-current))))
      (easi-search searchable query))
  (easi-all searchable))

;;;###autoload
(defun easi-rerun-with-new-query (query)
  "Run new QUERY on the same search engines as previously used.

Interactively, prompt for QUERY with `easi--prompt-for-query',
passing `easi-current-searchables' as argument."
  (interactive `(,(easi--prompt-for-query
		   (easi-session-state-searchables
		    (easi-session--get-current))))
	       easi-results-mode easi-result-mode)
  (easi-search
   (easi-session-state-searchables (easi-session--get-current))
   query))

;;; Examples (not part of infrastructure) (to be eventually removed)

;; TODO This is awful (and should probably live somewhere else!)
(defun easi--completing-read-multiple-searchables (searchable)
  "Read multiple SEARCHABLE from `easi-searchables'."
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
    (let* ((s-list (flatten-list searchable))
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
	  searchable))
       selected))))

(provide 'easi)

;;; easi.el ends here
