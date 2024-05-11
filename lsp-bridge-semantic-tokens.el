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

(defcustom lsp-bridge-semantic-tokens-auto-update 'timer
  "The method used to auto update semantic tokens."
  :group 'lsp-bridge-semantic-tokens
  :type '(radio
          (const :tag "timer" timer)
          (const :tag "hook" hook)))

(defcustom lsp-bridge-semantic-tokens-timer-update-interval 1
  "Value is a number specifying how many seconds to request semantic tokens in
timer auto update method."
  :group 'lsp-bridge-semantic-tokens
  :type 'number)

(defcustom lsp-bridge-semantic-tokens-delay 0.5
  "Value is a number specifying how many seconds to wait after a
window has been (re)scrolled or buffer has been changed before
requesting new semantic tokens."
  :group 'lsp-bridge-semantic-tokens
  :type 'number)

(defface lsp-bridge-semantic-tokens-property-face
  '((t (:inherit font-lock-property-name-face)))
  "Face used for property name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-class-face
  '((t (:inherit font-lock-type-face)))
  "Face used for class name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-number-face
  '((t (:inherit font-lock-number-face)))
  "Face used for number name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-interface-face
  '((t (:inherit font-lock-function-name-face)))
  "Face used for interface name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-namespace-face
  '((t (:inherit font-lock-keyword-face)))
  "Face used for namespace name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-decorator-face
  '((t (:inherit font-lock-comment-delimiter-face)))
  "Face used for decorator name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-regexp-face
  '((t (:inherit font-lock-regexp-face)))
  "Face used for regexp name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-operator-face
  '((t (:inherit font-lock-operator-face)))
  "Face used for operator name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-modifier-face
  '((t (:inherit font-lock-function-call-face)))
  "Face used for modifier name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-macro-face
  '((t (:inherit font-lock-builtin-face)))
  "Face used for macro name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-event-face
  '((t (:inherit font-lock-builtin-face)))
  "Face used for event name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-enum-member-face
  '((t (:inherit font-lock-type-face)))
  "Face used for enum member name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-enum-face
  '((t (:inherit font-lock-type-face)))
  "Face used for enum name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-struct-face
  '((t (:inherit font-lock-type-face)))
  "Face used for struct name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-type-face
  '((t (:inherit font-lock-type-face)))
  "Face used for type name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-string-face
  '((t (:inherit font-lock-string-face)))
  "Face used for string name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face used for keyword name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-type-parameter-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for parameter name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-parameter-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for parameter name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-variable-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for variable name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-function-face
  '((t (:inherit font-lock-function-name-face)))
  "Face used for function name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-method-face
  '((t (:inherit font-lock-function-name-face)))
  "Face used for method name."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face used for deprecated token."
  :group 'lsp-bridge-semantic-tokens)

(defface lsp-bridge-semantic-tokens-global-scope-face
  '((t (:inherit font-lock-constant-face)))
  "Face used for globalScope token."
  :group 'lsp-bridge-semantic-tokens)

(defvar-local lsp-bridge-semantic-tokens-type-faces
    [
     ("namespace" . lsp-bridge-semantic-tokens-namespace-face)
     ("type" . lsp-bridge-semantic-tokens-type-face)
     ("class" . lsp-bridge-semantic-tokens-class-face)
     ("enum" . lsp-bridge-semantic-tokens-enum-face)
     ("interface" . lsp-bridge-semantic-tokens-interface-face)
     ("struct" . lsp-bridge-semantic-tokens-struct-face)
     ("typeParameter" . lsp-bridge-semantic-tokens-type-parameter-face)
     ("parameter" . lsp-bridge-semantic-tokens-parameter-face)
     ("variable" . lsp-bridge-semantic-tokens-variable-face)
     ("property" . lsp-bridge-semantic-tokens-property-face)
     ("enumMember" . lsp-bridge-semantic-tokens-enum-member-face)
     ("event" . lsp-bridge-semantic-tokens-event-face)
     ("function" . lsp-bridge-semantic-tokens-function-face)
     ("method" . lsp-bridge-semantic-tokens-method-face)
     ("macro" . lsp-bridge-semantic-tokens-macro-face)
     ("keyword" . lsp-bridge-semantic-tokens-keyword-face)
     ("modifier" . lsp-bridge-semantic-tokens-modifier-face)
     ("comment" . lsp-bridge-semantic-tokens-comment-face)
     ("string" . lsp-bridge-semantic-tokens-string-face)
     ("number" . lsp-bridge-semantic-tokens-number-face)
     ("regexp" . lsp-bridge-semantic-tokens-regexp-face)
     ("operator" . lsp-bridge-semantic-tokens-operator-face)
     ("decorator" . lsp-bridge-semantic-tokens-decorator-face)
     ]
  "Faces to use for semantic tokens.")

(defvar-local lsp-bridge-semantic-tokens-type-modifier-faces []
  "Semantic tokens modifier faces.
Faces to use for semantic token modifiers.")

(defvar-local lsp-bridge-semantic-tokens-ignore-modifier-limit-types ["namespace"
                                                                      "class"
                                                                      "enum"
                                                                      "interface"
                                                                      "struct"
                                                                      "typeParameter"
                                                                      "parameter"
                                                                      "enumMember"
                                                                      "event"
                                                                      "macro"
                                                                      "modifier"
                                                                      "comment"
                                                                      "decorator"
                                                                      ]
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
               (run-at-time lsp-bridge-semantic-tokens-delay nil
                            (lambda ()
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

(defun lsp-bridge-semantic-tokens--timer-update ()
  "Try request semantic tokens after idle timer."
  (when lsp-bridge-semantic-tokens-mode
    (lsp-bridge-semantic-tokens--request-1 (window-start) (window-end) t)))

(defvar-local lsp-bridge-semantic-tokens--monitor-change nil)

(defun lsp-bridge-semantic-tokens--hook-enable ()
  (when (lsp-bridge-has-lsp-server-p)
    (unless lsp-bridge-semantic-tokens--overlays
      (setq-local lsp-bridge-semantic-tokens--overlays (make-hash-table :test 'equal)))

    (setq-local lsp-bridge-semantic-tokens--monitor-change t)

    (add-hook 'window-scroll-functions
              #'lsp-bridge-semantic-tokens--after-window-scroll nil t)

    (add-hook 'window-configuration-change-hook
              #'lsp-bridge-semantic-tokens--after-window-config-change nil t)

    (run-at-time lsp-bridge-semantic-tokens-delay nil #'lsp-bridge-semantic-tokens-request)))

(defun lsp-bridge-semantic-tokens--hook-disable ()
  (remove-hook 'window-configuration-change-hook
               #'lsp-bridge-semantic-tokens--after-window-config-change t)
  (remove-hook 'window-scroll-functions
               #'lsp-bridge-semantic-tokens--after-window-scroll t)
  (setq-local lsp-bridge-semantic-tokens--monitor-change nil)

  (lsp-bridge-semantic-tokens--delete-overlays (hash-table-keys lsp-bridge-semantic-tokens--overlays))
  (setq-local lsp-bridge-semantic-tokens--overlays nil))


(defvar lsp-bridge-semantic-tokens--timer-update-buffers 0 "The count of enable lsp-bridge-semantic-tokens-mode.")

(defun lsp-bridge-semantic-tokens--close-file ()
  (when lsp-bridge-semantic-tokens-mode
    (lsp-bridge-semantic-tokens-mode -1)))

(defun lsp-bridge-semantic-tokens--timer-enable ()
  (when (lsp-bridge-has-lsp-server-p)
    (unless lsp-bridge-semantic-tokens--overlays
      (setq-local lsp-bridge-semantic-tokens--overlays (make-hash-table :test 'equal)))

    (when (equal lsp-bridge-semantic-tokens--timer-update-buffers 0)
      (run-with-idle-timer lsp-bridge-semantic-tokens-timer-update-interval t #'lsp-bridge-semantic-tokens--timer-update))

    (cl-incf lsp-bridge-semantic-tokens--timer-update-buffers)

    (add-hook 'kill-buffer-hook #'lsp-bridge-semantic-tokens--close-file nil t)
    (run-at-time lsp-bridge-semantic-tokens-delay nil #'lsp-bridge-semantic-tokens-request)))


(defun lsp-bridge-semantic-tokens--timer-disable ()
  (cl-decf lsp-bridge-semantic-tokens--timer-update-buffers)
  (when (equal lsp-bridge-semantic-tokens--timer-update-buffers 0)
    (cancel-function-timers #'lsp-bridge-semantic-tokens--timer-update))

  (remove-hook 'kill-buffer-hook #'lsp-bridge-semantic-tokens--close-file t)

  (lsp-bridge-semantic-tokens--delete-overlays (hash-table-keys lsp-bridge-semantic-tokens--overlays))
  (setq-local lsp-bridge-semantic-tokens--overlays nil))

(defun lsp-bridge-semantic-tokens-request ()
  "Try request semantic tokens."
  (lsp-bridge-semantic-tokens--request-1 (window-start) (window-end) nil))

(define-minor-mode lsp-bridge-semantic-tokens-mode
  "Mirror mode for show semantic tokens."
  :global nil
  (cond (lsp-bridge-semantic-tokens-mode
         (pcase lsp-bridge-semantic-tokens-auto-update
           ('hook
            (lsp-bridge-semantic-tokens--hook-enable))
           ('timer
            (lsp-bridge-semantic-tokens--timer-enable))))
        (t
         (pcase lsp-bridge-semantic-tokens-auto-update
           ('hook
            (lsp-bridge-semantic-tokens--hook-disable))
           ('timer
            (lsp-bridge-semantic-tokens--timer-disable))))))

(provide 'lsp-bridge-semantic-tokens)

;;; lsp-bridge-semantic-tokens.el ends here
