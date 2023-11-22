;;; acm-backend-ctags.el --- acm ctags support -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(defcustom acm-backend-ctags-candidate-min-length 3
  "Minimal length of candidate."
  :type 'integer
  :group 'acm-backend-ctags)

(defcustom acm-enable-ctags t
  "Popup search words completions when this option is turn on."
  :type 'boolean
  :group 'acm-backend-ctags)

(defvar-local acm-backend-ctags-items nil)

(defun acm-backend-ctags-candidates (keyword)
  (acm-with-cache-candidates
   acm-backend-ctags-cache-candiates
   (when (and acm-enable-ctags
              (>= (length keyword) acm-backend-ctags-candidate-min-length))
     acm-backend-ctags-items)))

(defun acm-backend-ctags-clean ()
  (setq-local acm-backend-ctags-items nil)
  (setq-local acm-backend-ctags-cache-candiates nil))

(defun acm-backend-ctags-candidate-expand (candidate-info bound-start &optional preview)
  (if preview
      (acm-preview-create-overlay bound-start (point) (plist-get candidate-info :label))
    (delete-region bound-start (point))
    (insert (plist-get candidate-info :label))))

(provide 'acm-backend-ctags)

;;; acm-backend-ctags.el ends here
