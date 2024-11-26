;; Attempt to build a transient chooser

(require 'eieio)
(require 'transient)

(defclass easi-selectable ()
  ((key :initarg :key)
   (name :initarg :name)
   (documentation :initarg :documentation)))

;; * Toy objects

(setq foo
      (make-instance 'easi-selectable
		     :key "f"
		     :name "foo"))

(setq bar
      (make-instance 'easi-selectable
		     :key "b"
		     :name "bar"))

(setq baz
      (make-instance 'easi-selectable
		     :key "z"
		     :name "baz"))

(setq easi-selectable-list
      `(,foo ,bar ,baz))

;; * Internal code

;; We store whether the thing is selected in the `value' slot. Store
;; the easi-selectable object we are selecting in the
;; `easi-selectable' slot.
(defclass transient-selectable (transient-infix)
  (;; Lack of an initform is deliberate---there should never be an
   ;; object without a specific value for `easi-selectable,' so a
   ;; default value would be pointless.
   (easi-selectable :initarg :easi-selectable
		    :type easi-selectable)
   (format :initform " %k %d")
   ;; Default to unselected
   (value :initform nil))
  "Class for infix commands that represent a transient selectable.")

(cl-defgeneric easi-selectable-to-transient (prefix easi-selectable)
  "Return a transient suffix spec for selecting SELECTABLE in PREFIX.")

(cl-defmethod easi-selectable-to-transient (prefix (selectable symbol))
  (easi-selectable-to-transient prefix (symbol-value selectable)))

;; TODO Make docoumentation work with transient's show-help! (This can
;; happen down the line once I've written something like
;; easi-describe-{search-engine,searchable}).
(cl-defmethod easi-selectable-to-transient (prefix (selectable easi-selectable))
  "Return a transient suffix spec for selecting SELECTABLE in PREFIX."
  (let* (;; (name (slot-value selectable 'name))
	 (key (slot-value selectable 'key))
	 (description (slot-value selectable 'name))
	 (command (lambda ()
		    (interactive)
		    ;; Standard infix definition
		    (let ((obj (transient-suffix-object)))
		      (transient-infix-set obj (transient-infix-read obj)))
		    (transient--show)))
	 (spec `(,key ,description ,command :class transient-selectable))
	 (parsed (transient-parse-suffix prefix spec)))
    ;; Mutate the property list
    (plist-put (caddr parsed) :easi-selectable selectable)
    ;; Return the whole object, with the mutated list
    parsed))

;; Methods for the new transient type I've made

;; Cribbed from transient-switch
(cl-defmethod transient-infix-read ((obj transient-selectable))
  "Toggle the selectable selected or not."
  (if (slot-value obj 'value) nil t) )

(defface easi-transient-selected '((nil . (:inherit transient-value)))
  "Face for selected searchables in `easi-choose-selectable'.")

(cl-defmethod transient-format-description ((obj transient-selectable))
  (when-let ((desc (slot-value obj 'description)))
    (if (functionp desc)
        (with-current-buffer transient--original-buffer
          (funcall desc))
      (if (slot-value obj 'value)
	  (propertize desc 'face 'easi-transient-selected)
	desc))))

;; We need this because the standard method (which
;; `transient-selectable' would fall back to) tries to access the
;; `argument' slot, but the argument slot is unitialised here.
(cl-defmethod transient-format-value ((obj transient-selectable))
  (format "%s" (slot-value obj 'value)))

;; * Transient command!

(defvar easi--transient-selectable-list nil
  "Internal use only. For keeping track of state in
`easi-choose-selectable'.")

;; TODO Groups (subgroups?) and descriptions in the searchable list
(transient-define-prefix easi--transient-choose-selectable (selectables)
  [["Searchables" ;; TODO Name?
    :class transient-column
    :setup-children
    (lambda (_)
      (mapcar (apply-partially
	       'easi-selectable-to-transient
	       'easi--transient-choose-selectable
	       ;; transient--prefix
	       )
	      selectables))]
   [("RET" "Search!"
     (lambda ()
       (interactive)
       (mapcar
	(lambda (obj)
	  (slot-value obj 'easi-selectable))
	(seq-filter
	 (lambda (obj)
	   ;; Filter for selectables...
	   (and (transient-selectable-p obj)
		;; ... but only those which are selected!
		(slot-value obj 'value)))
	 ;; current-transient-suffixes
	 (transient-args 'easi--transient-choose-selectable)
	 ))))]]
  ;; The interactive spec is here because Transient requires it. But
  ;; in practice this function should never be called without passing
  ;; arguments, so we raise an error if that happens.
  (interactive (error "No SELECTABLES arg specified"))
  (transient-setup 'easi--transient-choose-selectable))
