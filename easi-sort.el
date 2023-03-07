;;; easi-sort.el --- Sorting and mixing functionality in EASI  -*- lexical-binding: t; -*-

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

;; This module implements customisable sorting of results from more
;; than one source in EASI.

;;; Code:

;;;; Customizables

(defcustom easi-default-sort-function nil
  "Function for sorting results.

Takes two arguments: a list of lists of results, and a query (a
string) which was used to get those results. The function must
return a flat list of results, in the desired order. This could,
but need not, be based on the query."
  :type '(repeat function))

;;;; Driver Functions

(defun easi-sort-results (sorter results query)
  "Call SORTER on RESULTS and QUERY.

RESULTS is a list of results. QUERY is the string used to get the
results. SORTER is a function which takes RESULTS and QUERY, and
must return a flat list of results, in the desired order. This
could, but need not, be based on the query."
  (funcall sorter results query))

(defun easi--sort-get-searchable-sorter (searchable)
  "Get sorter to use for SEARCHABLE.

If calling `easi-searchable-sorters' returns no-nil, return first
element of that. If not, return first element of
`easi-default-sort-functions'."
  (car `(,@(easi-searchable-sorters searchable)
	 ,@easi-default-sort-functions)))

(provide 'easi-sort)
;;; easi-sort.el ends here
