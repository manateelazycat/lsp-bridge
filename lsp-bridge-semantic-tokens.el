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

(defcustom lsp-bridge-semantic-tokens-delay 0.5
  "Value is a number specifying how many seconds to wait after a
window has been (re)scrolled or buffer has been changed before
requesting new semantic tokens."
  :group 'lsp-bridge-semantic-tokens
  :type 'number)

(defface lsp-bridge-semantic-tokens-variable-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for variable name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-method-face
  '((t (:inherit font-lock-function-name-face)))
  "Face used for method name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-deprecated-face
  '((t :strike-through t :inherit font-lock-comment-face))
  "Face used for deprecated token."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-global-scope-face
  '((t :weight extra-bold))
  "Face used for globalScope token."
  :group 'lsp-bridge-semantic-tokens)

(defvar-local lsp-bridge-semantic-tokens-type-faces
    [("method" . lsp-bridge-semantic-tokens-method-face)
     ("variable" . lsp-bridge-semantic-tokens-variable-face)]
  "Faces to use for semantic tokens.")

(defvar-local lsp-bridge-semantic-tokens-type-modifier-faces
    [("deprecated" . lsp-bridge-semantic-tokens-deprecated-face)
     ("globalScope" . lsp-bridge-semantic-tokens-global-scope-face)]
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


(defun lsp-bridge-semantic-tokens--request-1 (from to use-cache)
  "Try request semantic tokens between FROM to TO."
  (lsp-bridge-call-file-api "semantic_tokens"
                            (buffer-name)
                            (lsp-bridge--point-position from)
                            (lsp-bridge--point-position to)
                            (if use-cache
                                1
                              0)))

(defun lsp-bridge-semantic-tokens--after-window-scroll (window display-start)
  "Try request semantic tokens after window scroll."
  (cl-macrolet ((wsetq (sym val) `(set-window-parameter window ',sym ,val))
                (wgetq (sym) `(window-parameter window ',sym)))
    (let ((buf (window-buffer window))
          (timer (wgetq lsp-bridge-semantic-tokens--timer))
          (last-display-start (wgetq lsp-bridge-semantic-tokens--last-display-start)))
      (unless (eql last-display-start display-start)
        (when timer
          (cancel-timer timer))
        (wsetq lsp-bridge-semantic-tokens--last-display-start display-start)
        (wsetq lsp-bridge-semantic-tokens--timer
               (run-at-time lsp-bridge-semantic-tokens-delay nil (lambda ()
                                      (when (buffer-live-p buf)
                                        (with-current-buffer buf
                                          (when (eq buf (window-buffer window))
                                            (lsp-bridge-semantic-tokens--request-1 (window-start window) (window-end window) t)
                                            (wsetq lsp-bridge-semantic-tokens--timer nil)))))))))))

(defun lsp-bridge-semantic-tokens--after-window-config-change ()
  "Try request semantic tokens after window config change scroll."
  (lsp-bridge-semantic-tokens--request-1 (window-start) (window-end) t))


(defvar-local lsp-bridge-semantic-tokens--after-change-timer nil)

(defun lsp-bridge-semantic-tokens--after-change ()
  "Try request semantic tokens after change."
  (let ((buf (current-buffer)))
    (when lsp-bridge-semantic-tokens--after-change-timer
      (cancel-timer lsp-bridge-semantic-tokens--after-change-timer))

    (setq-local lsp-bridge-semantic-tokens--after-change-timer
                (run-at-time lsp-bridge-semantic-tokens-delay nil
                             (lambda ()
                               (when (buffer-live-p buf)
                                 (with-current-buffer buf
                                   (lsp-bridge-semantic-tokens--request-1 (window-start) (window-end) nil)
                                   (setq-local lsp-bridge-semantic-tokens--after-change-timer nil))))))))

(defun lsp-bridge-semantic-tokens-request ()
  "Try request semantic tokens."
  (interactive)
  (lsp-bridge-semantic-tokens--request-1 (window-start) (window-end) nil))

(define-minor-mode lsp-bridge-semantic-tokens-mode
  "Mirror mode for show semantic tokens."
  :global nil
  (cond (lsp-bridge-semantic-tokens-mode
         (when (lsp-bridge-has-lsp-server-p)
           (unless lsp-bridge-semantic-tokens--overlays
             (setq-local lsp-bridge-semantic-tokens--overlays (make-hash-table :test 'equal)))

           (add-hook 'window-scroll-functions
                     #'lsp-bridge-semantic-tokens--after-window-scroll nil t)

           (add-hook 'window-configuration-change-hook
                     #'lsp-bridge-semantic-tokens--after-window-config-change nil t)

           (run-at-time lsp-bridge-semantic-tokens-delay nil #'lsp-bridge-semantic-tokens-request)))
        (t
         (remove-hook 'window-configuration-change-hook
                      #'lsp-bridge-semantic-tokens--after-window-config-change t)

         (remove-hook 'window-scroll-functions
                   #'lsp-bridge-semantic-tokens--after-window-scroll t)

         (lsp-bridge-semantic-tokens--delete-overlays (hash-table-keys lsp-bridge-semantic-tokens--overlays))
         (setq-local lsp-bridge-semantic-tokens--overlays nil))))

(provide 'lsp-bridge-semantic-tokens)

;;; lsp-bridge-semantic-tokens.el ends here
