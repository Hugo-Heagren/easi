;;; easi-presenter.el --- Present (collections of) results to user  -*- lexical-binding: t; -*-

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

;; This module contains code for presenting collections of results and
;; details about individual results, to the user in EASI.

;;; Code:

(require 'cl-lib)

;;;; Customizables

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

;;;; Presenter types

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
  current-result-getter
  hook)

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
  current-field-getter
  ;; ACTION arg to pass to `display-buffer'
  display-action
  hook)


(provide 'easi-presenter)
;;; easi-presenter.el ends here
