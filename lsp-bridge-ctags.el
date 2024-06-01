;;; lsp-bridge-ctags.el --- LSP bridge  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Lsp-Bridge ctags
;;

;;; Code:
(require 'xref)

(defface lsp-bridge-tag-annotation-face
  '((((background light))
     :foreground "#666666" :slant italic)
    (t
     :foreground "#c0c0c0" :slant italic))
  "Face used for annotations when presenting a tag.
Annotations include kind, type, etc.")

(defun lsp-bridge-xref--make-object (tag)
  "Make xref object of TAG."
  (let* ((path (plist-get tag :ext-abspath))
         (line (plist-get tag :line))
         (str (plist-get tag :str))
         (annotation (plist-get tag :annotation)))
    (xref-make
     (if annotation
         (concat (propertize (concat "(" annotation ") ") 'face 'lsp-bridge-tag-annotation-face) str)
       str)
     (xref-make-file-location path line 0))))

(defun lsp-bridge-xref-callback (tags)
  (let ((fetcher (lambda () (mapcar #'lsp-bridge-xref--make-object tags))))
    (xref-show-xrefs fetcher nil)))

(defun lsp-bridge-ctags-find-def-at-point ()
  (interactive)
  (if (lsp-bridge-is-remote-file)
      (lsp-bridge-remote-send-func-request "ctags_find_def"
                                           (list
                                            (symbol-at-point)
                                            (file-local-name (buffer-file-name))))
    (lsp-bridge-call-async "ctags_find_def" (symbol-at-point) (buffer-file-name))))

(provide 'lsp-bridge-ctags)
;;; lsp-bridge-ctags.el ends here