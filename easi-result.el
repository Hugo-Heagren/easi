;;; easi-result.el --- Result API                    -*- lexical-binding: t; -*-

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

;; This module is for programmatic interaction with results from
;; search engines defined in EASI. EASI tries to make as few
;; assumptions as possible about the nature of results, so a lot of
;; this code is very generic, and hopes that the user will extend it
;; should they find the need.

;;; Code:

(require 'easi-searchable)

(cl-defgeneric easi--result-list-fields (result)
  "Return a list of fields in RESULT.

Each field returned must be a string.")

(cl-defmethod easi--result-list-fields ((result list))
  "RESULT is a list.

If every element of RESULT is a cons, `mapcar' `car' over RESULT.
If not, and RESULT passes `plistp', return a list of every second
element of RESUlT, beginning with the first."
  (cond
   ((cl-every #'consp result)
    (mapcar #'car result))
   ((plistp result)
    (cl-loop for x in result by 'cddr
	     collect x))))

(defalias 'easi-result-get-field
  'easi-structured-object-get-field)

(cl-defgeneric easi-result-retrieve-search-engine (result)
  "Return the engine attached to RESULT.

For certain features (such as field aliasing) to work, this
function (and `easi--utils-result-attach-search-engine') must have an
implementation for every type of result.")

(cl-defmethod easi-result-retrieve-search-engine ((result cons))
  "Heuristically get search engine from a list RESULT.

If RESULT passes `(cl-every #'consp result)', then get key
\"easi-search-engine\". If not, but it passes `plistp', then get
the plist key \"easi-search-engine\". Otherwise return nil."
  (cond
   ((cl-every #'consp result)
    ;; TODO Do I need to abstract out the comparison function above?
    (alist-get "easi-search-engine" result nil nil #'string=))
   ((plistp result)
    (plist-get result "easi-search-engine"))))

(defun easi-result-aliases (result)
  "Return list of field alias for RESULT."
  (let ((searchable (easi-result-retrieve-search-engine result)))
    (easi-search-engine-field-aliases searchable)))


(provide 'easi-result)
;;; easi-result.el ends here
