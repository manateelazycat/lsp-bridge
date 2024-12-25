;;; acm-backend-ctags.el --- acm ctags support -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;;; Code:
(require 'xref)

(defcustom acm-backend-ctags-candidate-min-length 3
  "Minimal length of candidate."
  :type 'integer
  :group 'acm-backend-ctags)

(defcustom acm-enable-ctags t
  "Popup search words completions when this option is turn on."
  :type 'boolean
  :group 'acm-backend-ctags)

(defcustom acm-backend-ctags-max-candidates 10
  "Maximal number of candidate of menu."
  :type 'integer
  :group 'acm-backend-ctags)

(defface acm-backend-ctags-annotation-face
  '((((background light))
     :foreground "#666666" :slant italic)
    (t
     :foreground "#c0c0c0" :slant italic))
  "Face used for annotations when presenting a tag.
Annotations include kind, type, etc.")

(defun acm-backend-ctags-xref--make-object (tag)
  "Make xref object of TAG."
  (let* ((path (plist-get tag :ext-abspath))
         (line (plist-get tag :line))
         (str (plist-get tag :str))
         (annotation (plist-get tag :annotation)))
    (xref-make
     (if annotation
         (concat (propertize (concat "(" annotation ") ") 'face 'acm-backend-ctags-annotation-face) str)
       str)
     (xref-make-file-location path line 0))))

(defun acm-backend-ctags-xref-callback (tags)
  (let ((fetcher (lambda () (mapcar #'acm-backend-ctags-xref--make-object tags))))
    (xref-show-xrefs fetcher nil)))

(defun acm-backend-ctags-find-def-at-point ()
  (interactive)
  (if (lsp-bridge-is-remote-file)
      (lsp-bridge-remote-send-func-request "ctags_find_def"
                                           (list
                                            (symbol-at-point)
                                            (file-local-name (buffer-file-name))))
    (lsp-bridge-call-async "ctags_find_def" (symbol-at-point) (buffer-file-name))))

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
