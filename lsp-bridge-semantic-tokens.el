;;; lsp-bridge-semantic-tokens.el --- LSP bridge  -*- lexical-binding: t -*-


(defgroup lsp-bridge-semantic-tokens nil
  "Semantic tokens support."
  :prefix "lsp-bridge-semantic-tokens-"
  :group 'lsp-bridge)

(defcustom lsp-bridge-semantic-tokens-apply-modifiers 'override
  "The method used to apply modifiers."
  :group 'lsp-bridge-semantic-tokens
  :type '(radio
          (const nil)
          (const :tag "override" override)
          (const :tag "combine" combine)))

(defface lsp-bridge-semantic-tokens-method-face
  '((t (:inherit font-lock-function-name-face)))
  "Face used for variable name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-deprecated-face
  '((t :strike-through t :inherit font-lock-comment-face))
  "Face used for deprecated token."
  :group 'lsp-bridge-semantic-tokens)

(defvar-local lsp-bridge-semantic-tokens-type-faces
    [("method" . lsp-bridge-semantic-tokens-method-face)]
  "Faces to use for semantic tokens.")

(defvar-local lsp-bridge-semantic-tokens-type-modifier-faces
    [("deprecated" . lsp-bridge-semantic-tokens-deprecated-face)]
  "Semantic tokens modifier faces.
Faces to use for semantic token modifiers.")


(defvar-local lsp-bridge-semantic-tokens-ignore-modifier-limit-types []
  "Which types need to ignore modifier limit.")


(defvar-local lsp-bridge-semantic-tokens--overlays nil  "Semantic tokens overlays.")


(defconst lsp-bridge-semantic-tokens--face-attribute-names
  (apply 'append
         (mapcar (lambda (x) (list (car x)))
                 face-attribute-name-alist)))

(defun lsp-bridge-semantic-tokens--combine-faces (faces)
  "Combine attributes of faces to one face."
  (let ((attributes (list)))
    (dolist (face faces)
      (dolist (attr-name lsp-bridge-semantic-tokens--face-attribute-names)
        (when-let* ((value (face-attribute face attr-name (window-frame)))
                   (valid (not (eq value 'unspecified))))
          (setq attributes (plist-put attributes attr-name value 'equal)))))
    `((t ,@attributes))))

(defun lsp-bridge-semantic-tokens--delete-overlays (keys)
  "Delete semantic tokens overlays."
  (dolist (key keys)
    (when-let ((ov (gethash key lsp-bridge-semantic-tokens--overlays)))
      (delete-overlay ov)
      (remhash key lsp-bridge-semantic-tokens--overlays))))

(defun lsp-bridge-semantic-tokens--update (buffer-name old-tokens new-tokens)
  "Update semantic tokens."
  ;; (message "old-tokens: %s" old-tokens)
  ;; (message "new-tokens: %s" new-tokens)
  (with-current-buffer buffer-name
    (with-silent-modifications
      (lsp-bridge-semantic-tokens--delete-overlays old-tokens)
      (save-mark-and-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (let ((current-line 0)
                (line-start-pos (point))
                (colum 0)
                (line-delta)
                (token-begin)
                (token-end)
                (ov))
            (dolist (token new-tokens)
              (setq line-delta (nth 0 token))
              (unless (= line-delta 0)
                (forward-line line-delta)
                (setq line-start-pos (point))
                (setq colum 0)
                (setq current-line (+ current-line line-delta)))
              (setq colum (+ colum (nth 1 token)))
              (setq token-begin (+ line-start-pos colum))
              (setq token-end (min (line-end-position)
                                   (+ token-begin (nth 2 token))))
              (setq ov (make-overlay token-begin token-end))
              
              ;; apply face
              (pcase lsp-bridge-semantic-tokens-apply-modifiers
                ('override
                 (if-let ((last-modifier-face-index (car (last (nth 4 token)))))
                     (overlay-put ov 'face (cdr (aref lsp-bridge-semantic-tokens-type-modifier-faces last-modifier-face-index)))
                   (overlay-put ov 'face (cdr (aref lsp-bridge-semantic-tokens-type-faces (nth 3 token))))))
                ('combine
                 (let ((faces-alist (cons (aref lsp-bridge-semantic-tokens-type-faces (nth 3 token))
                                          (mapcar #'(lambda (face-index)
                                                      (aref lsp-bridge-semantic-tokens-type-modifier-faces face-index)) 
                                                  (nth 4 token))))
                       (combine-face-name "lsp-bridge-semantic-tokens-combine")
                       (faces))
                   (dolist (face-alist faces-alist)
                     (setq combine-face-name (concat combine-face-name "-" (car face-alist)))
                     (push (cdr face-alist) faces))
                   (let ((combine-face-symbol (intern combine-face-name)))
                     (unless (facep combine-face-symbol)
                       (make-empty-face combine-face-symbol)
                       (face-spec-set combine-face-symbol 
                                      (lsp-bridge-semantic-tokens--combine-faces faces)))
                     (overlay-put ov 'face combine-face-symbol))))
                (_
                 (overlay-put ov 'face (cdr (aref lsp-bridge-semantic-tokens-type-faces (nth 3 token))))))
              
              (puthash (list current-line colum (nth 2 token) (nth 3 token) (nth 4 token))
                       ov lsp-bridge-semantic-tokens--overlays))))))))

(defun lsp-bridge-semantic-tokens-request (&optional force &rest args)
  "Try request semantic tokens.
When FORCE is t, clear all cache."
  (interactive)
  (when (and (lsp-bridge-has-lsp-server-p)
             (not (lsp-bridge-completion-ui-visible-p)))
    (when (called-interactively-p)
      (setq force t))
    (unless lsp-bridge-semantic-tokens--overlays
      (setq-local lsp-bridge-semantic-tokens--overlays (make-hash-table :test 'equal)))
    (lsp-bridge-call-file-api "semantic_tokens"  (buffer-name) force)))

(provide 'lsp-bridge-semantic-tokens)

;;; lsp-bridge-semantic-tokens.el ends here
