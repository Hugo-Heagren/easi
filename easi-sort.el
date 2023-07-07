;;; easi-sort.el --- Sorting and mixing functionality in EASI  -*- lexical-binding: t; -*-

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

;; This module implements customisable sorting of results from more
;; than one source in EASI.

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'easi-result)
(require 'easi-searchable)

;;;; Customizables

(defcustom easi-default-sort-functions
  '(easi-sort-round-robin
    easi-sort-append)
  "List of functions for sorting results.

Each function takes two arguments: a list of results, and a
query (a string) which was used to get those results. The
function must return a flat list of results, in the desired
order. This could, but need not, be based on the query.

The function `easi-sort--group-results-by-engine' may be helpful for
writing sorters."
  :group 'easi
  :type '(repeat function))

;;;; Utility functions

(defun easi-sort--group-results-by-engine (results)
  "Group RESULTS into lists by search engine.

Retrieve search engine from each member of RESULTS, then group
all results with the same search-engine into a list. Return a
list of lists of results."
  (mapcar #'cdr
	  (seq-group-by
	   #'easi-result--retrieve-search-engine results)))

;;;; Driver Functions

(defun easi-sort--results (sorter results query)
  "Call SORTER on RESULTS and QUERY.

RESULTS is a list of results. QUERY is the string used to get the
results. SORTER is a function which takes RESULTS and QUERY, and
must return a flat list of results, in the desired order. This
could, but need not, be based on the query."
  (funcall sorter results query))

(defun easi-sort--get-searchable-sorter (searchable)
  "Get sorter to use for SEARCHABLE.

If calling `easi-searchable--sorters' returns no-nil, return first
element of that. If not, return first element of
`easi-default-sort-functions'."
  (car `(,@(easi-searchable--sorters searchable)
	 ,@easi-default-sort-functions)))

;;;; Sorters

(defun easi-sort-append (results _query)
  "Append lists in RESULTS to each other.

This \\='sorter\\=' is useful for quickly \\='sorting\\=' a set
of results where order is not important (such as data which is
always presented with a 2D graphical presenter). It just returns
RESULTS."
  results)

(defun easi-sort-round-robin (results _query)
  "Take one element of each list in RESULTS in order."
   (cl-loop for i from 0
	    with newelts = nil with output = nil
	    do (setq newelts (mapcar (apply-partially #'nth i)
				     (easi-sort--group-results-by-engine results)))
	    until (cl-every #'null newelts)
	    do (setq output (append output newelts))
	    finally return output))

;; TODO Implement a relevance sorter

(provide 'easi-sort)
;;; easi-sort.el ends here
