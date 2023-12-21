;;; easi-utils.el --- Utilities for EASI             -*- lexical-binding: t; -*-

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

;; Utilities used throughout the rest of EASI and functions which are
;; often useful when building EASI searchables.

;;; Code:

(cl-defgeneric easi-utils--result-attach-search-engine (engine result)
  "Attach ENGINE to RESULT.

For certain features (such as field aliasing) to work, this
function (and `easi-result--retrieve-search-engine') must have an
implementation for every type of result.

Must return RESULT with ENGINE attached.")

(cl-defmethod easi-utils--result-attach-search-engine (engine (result cons))
  "`setf' the key \"easi-search-engine\" of RESULT to ENGINE.

RESULT is assumed to be either an alist (if every element is a
cons) or a plist (`plistp')."
  (cond
   ((cl-every #'consp result)
    (setf (alist-get "easi-search-engine" result) engine))
   ((plistp result)
    (setf (plist-get result "easi-search-engine") engine)))
  result)

(defun easi-utils--resolve-symbol (symbol)
  "Iteratively get value of SYMBOL until it is not a symbol."
  (cl-loop
   while (and symbol (symbolp symbol))
   do (setq symbol (symbol-value symbol))
   and
   finally return symbol))

(defun easi-utils--buffer-from-default (default session)
  "Get an appropriate buffer given DEFAULT.

SESSION If DEFAULT is a string, pass it to `generate-new-buffer'.
If DEFAULT is a function, call it passing SESSION as the sole
argument.

Return the result of whatever is done. If DEFAULT is neither a
string nor a function, return nil."
  (cond
   ((stringp default)
    (generate-new-buffer default))
   ((functionp default)
    (funcall default session))))

;;; Utils for writing search engines

(defun easi-utils-get-http-body (buffer)
  "Return HTTP response body in BUFFER.

Written principally to be used in EASI post-processors."
  (defvar url-http-end-of-headers)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties
       url-http-end-of-headers (point-max)))))

(when (libxml-available-p)
  (defun easi-utils-parse-html-body (buffer)
    "Parse BUFFER with `libxml-parse-html-region'."
    (with-current-buffer buffer
      (save-restriction
	(widen)
	(libxml-parse-html-region (point-min) (point-max))))))

(when (json-available-p)
  (defun easi-utils-json-parse-with-lists (string)
    "Parse STRING as a JSON object.

Uses `json-parse-string' with:
- OBJECT-TYPE set to `alist'
- ARRAY-TYPE set to `list'
- null-object set to nil
- false-object set to nil"
    (json-parse-string string
		       :object-type 'alist
		       :array-type 'list
		       :null-object nil
		       :false-object nil)))

(defun easi-utils-calculate-offset (number page &optional force-number)
  "Calculate offset given a PAGE and NUMBER (per page).

This is mostly useful for building queries which require an
offset argument instead of a page.

If NUMBER is nil then: if FORCE-NUMBER is non-nil return 0,
otherwise return nil."
  (if (numberp number) (* (- page 1) number)
    (when force-number 0)))

(provide 'easi-utils)
;;; easi-utils.el ends here
