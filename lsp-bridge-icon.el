;;; lsp-bridge-icon.el  -*- lexical-binding: t; -*-

(require 'all-the-icons)
(require 'lsp-bridge)

(defvar lsp-bridge-icon--cache nil
  "The cache of styled and padded label (text or icon).
An alist.")

(defun lsp-bridge-icon-reset-cache ()
  "Remove all cached icons from `lsp-bridge-icon-mapping'."
  (interactive)
  (setq lsp-bridge-icon--cache nil))

(defun lsp-bridge-icon--set-default-clear-cache (&rest args)
  (lsp-bridge-icon-reset-cache)
  (apply #'set-default args))

(defvar lsp-bridge-icon--icons
  `((unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
    (text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
    (method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
    (function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
    (fun . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
    (constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
    (ctor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
    (field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
    (variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
    (var . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
    (class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
    (interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
    (i/f . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
    (module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
    (mod . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
    (property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
    (prop . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
    (unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
    (value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
    (enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
    (keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
    (k/w . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
    (snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
    (sn . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
    (color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
    (file . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
    (reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
    (ref . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
    (folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
    (dir . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
    (enum-member . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
    (enummember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
    (member . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
    (constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
    (const . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
    (struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
    (event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
    (operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
    (op . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
    (type-parameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
    (param . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
    (template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15))
    (t . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))))


(defsubst lsp-bridge-icon--metadata-get (metadata type-name)
  (or
   (plist-get completion-extra-properties (intern (format ":%s" type-name)))
   (cdr (assq (intern type-name) metadata))))

(defun lsp-bridge-icon-formatted (kind)
  "Format icon kind with all-the-icons"
  (or (alist-get kind lsp-bridge-icon--cache)
      (let ((map (assq kind lsp-bridge-icon--icons)))
        (let*  ((icon (if map
                          (cdr map)
                        (cdr (assq t lsp-bridge-icon--icons))))
                (half (/ (default-font-width) 2))
                (pad (propertize " " 'display `(space :width (,half))))
                (disp (concat pad icon pad)))
          (setf (alist-get kind lsp-bridge-icon--cache) disp)
          disp))))

(defun lsp-bridge-icon-margin-formatter (metadata)
  "Return a margin-formatter function which produces kind icons.
METADATA is the completion metadata supplied by the caller (see
info node `(elisp)Programmed Completion').  To use, add this
function to the relevant margin-formatters list."
  (if-let ((kind-func (lsp-bridge-icon--metadata-get metadata "company-kind")))
      (lambda (cand)
        (if-let ((kind (funcall kind-func cand)))
            (lsp-bridge-icon-formatted kind)
          (lsp-bridge-icon-formatted t))))) ;; as a backup

(defun lsp-bridge-company-box-icons (candidate)
  (when-let* ((item (get-text-property 0 'lsp-bridge--lsp-item candidate))
              (kind (plist-get item :kind)))
    (intern kind)))

(provide 'lsp-bridge-icon)
;;; lsp-bridge-icon.el ends here
