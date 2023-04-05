;;; easi-utils.el --- Utilities for EASI             -*- lexical-binding: t; -*-

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

;; Utilities used throughout the rest of EASI.

;;; Code:

(cl-defgeneric easi-utils-result-attach-search-engine (engine result)
  "Attach ENGINE to RESULT.

For certain features (such as field aliasing) to work, this
function (and `easi-result-retrieve-search-engine') must have an
implementation for every type of result.

Must return RESULT with ENGINE attached.")

(cl-defmethod easi-utils-result-attach-search-engine (engine (result cons))
  (cond
   ((cl-every #'consp result)
    (setf (alist-get "easi-search-engine" result) engine))
   ((plistp result)
    (setf (plist-get result "easi-search-engine") engine)))
  result)

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

(provide 'easi-utils)
;;; easi-utils.el ends here
