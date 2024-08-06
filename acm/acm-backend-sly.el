;;; acm-backend-sly.el --- SLY completion backend -*- lexical-binding: t; -*-

(require 'sly)

(defcustom acm-enable-sly nil
  "Enable SLY support for Lisp. "
  :type 'boolean
  :group 'acm-backend-sly)

(defcustom acm-backend-sly-mode-list
  '(
    lisp-mode
    sly-mrepl-mode
    )
  "The mode list to support SLY. "
  :type 'cons)

;; from https://github.com/joaotavora/sly/blob/ba40c8f054ec3b7040a6c36a1ef3e9596b936421/lib/sly-completion.el#L323C9-L335C36
;; and see `acm-icon-alist' for the icon mapping
;; from https://github.com/manateelazycat/lsp-bridge/blob/49b5497243873b1bddea09a4a988e3573ed7cc3e/acm/acm-icon.el#L94

(defun acm-backend-sly-candidate-type (candidate)
  (pcase (get-text-property 0 'sly--classification candidate)
    ("fn"             "function")
    ("generic-fn"     "method")
    ("generic-fn,cla" "method")
    ("cla,type"       "class")
    ("cla"            "class")
    ("special-op"     "operator")
    ("type"           "class")
    ("constant"       "constant")
    ("var"            "variable")
    ("pak"            "package")
    ("pak,constant"   "package")
    ("macro"          "macro")
    (_                "unknown")))

(defun acm-backend-sly-candidates (keyword)
  (when (and (member major-mode acm-backend-sly-mode-list)
	     (sly-connected-p))
    (mapcar (lambda (candidate)
	      (let* ((type (acm-backend-sly-candidate-type candidate))
		     (note (get-text-property
			    0 'sly--classification candidate)))
		(list :key          candidate
		      :icon         type
		      :label        candidate
		      :displayLabel candidate
		      :annotation   note
		      :backend      "sly")))
	    (ignore-errors
	      ;; see `sly-complete-symbol-function'
	      ;; it will return (COMPLETIONS NIL),
	      ;; where COMPLETIONS are a list of propertized strings
	      (car (funcall sly-complete-symbol-function keyword))))))

(defun acm-backend-sly-candidate-doc (candidate)
  (when (sly-connected-p)
    (let ((type (plist-get candidate :icon))
	  (key  (plist-get candidate :key)))
      ;; Note:
      ;; here assumes that all the completion is
      ;; within lisp namespace (no symbol existance check)
      (pcase type
	((or "function" "method" "operator")
	 (sly-eval `(slynk:describe-function ,key)))
	(_ (sly-eval `(slynk:describe-symbol ,key)))))))

(provide 'acm-backend-sly)

;;; acm-backend-sly.el ends here
