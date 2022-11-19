;;; lsp-bridge-call-hierarchy.el --- LSP bridge  -*- lexical-binding: t -*-

(defvar lsp-bridge-call-hierarchy-name-width 40
  "Width of the call name column in the call hierarchy buffer.")

(defvar lsp-bridge-call-hierarchy-path-width 60
  "Width of the path column in the call hierarchy buffer.")

(defvar lsp-bridge-call-hierarchy-preview-height 30
  "Height of the preview window. (set 0 to disable preview)")

(defvar lsp-bridge-call-hierarchy--frame nil)

(defvar lsp-bridge-call-hierarchy--index 0)

(defvar lsp-bridge-call-hierarchy--overlay nil)

(defvar lsp-bridge-call-hierarchy--popup-response nil)

(defun lsp-bridge-incoming-call-hierarchy ()
  (interactive)
  (lsp-bridge-call-file-api "call_hierarchy"
                            (lsp-bridge--position) t))

(defun lsp-bridge-outgoing-call-hierarchy ()
  (interactive)
  (lsp-bridge-call-file-api "call_hierarchy"
                            (lsp-bridge--position) nil))

(defun lsp-bridge-call-hierarchy-posframe-show (buffer)
  (let* ((posframe-height (+ lsp-bridge-call-hierarchy-preview-height
                             (length lsp-bridge-call-hierarchy--popup-response)))
         (posframe-width (+ 6 ;; length of spaces and icon
                            lsp-bridge-call-hierarchy-name-width
                            lsp-bridge-call-hierarchy-path-width)))
    (apply #'posframe-show
           (get-buffer buffer)
           :poshandler #'posframe-poshandler-frame-center
           (list
            :max-height posframe-height
            :min-height posframe-height
            :min-width  posframe-width
            :max-width  posframe-width
            :border-width 2
            :border-color "gray"
            :accept-focus t
            ))))

(defun lsp-bridge-call-hierarchy-format-call-string (s l)
  (if (> (length s) l)
      (format "%s%s" (substring s 0 (- l (length "..."))) "...")
    (concat s (make-string (- l (length s)) ?\s))))

(defun lsp-bridge-call-hierarchy-format-call (call)
  (let ((name (plist-get call :name))
        (path (plist-get call :rel_path))
        (icon (cdr (assoc (plist-get call :icon) acm-icon-alist))))
    (format "%s %s %s\n"
            (if icon
                (apply #' acm-icon-build icon)
             " ")
            (lsp-bridge-call-hierarchy-format-call-string name lsp-bridge-call-hierarchy-name-width)
            (lsp-bridge-call-hierarchy-format-call-string path lsp-bridge-call-hierarchy-path-width))))

(defun lsp-bridge-call-hierarchy-maybe-preview ()
  (when (> lsp-bridge-call-hierarchy-preview-height 0)
    (other-window 1)
    (lsp-bridge-call-hierarchy-show)
    (recenter)
    (other-window -1)))

(defun lsp-bridge-call-hierarchy--popup (response)
  (interactive)
  (setq lsp-bridge-call-hierarchy--popup-response response)
  (setq lsp-bridge-call-hierarchy--frame
        (lsp-bridge-call-hierarchy-posframe-show (get-buffer-create "*lsp-bridge-call-hierarchy*")))
  (select-frame-set-input-focus lsp-bridge-call-hierarchy--frame)
  (dolist (call lsp-bridge-call-hierarchy--popup-response)
    (insert (lsp-bridge-call-hierarchy-format-call call)))
  (delete-backward-char 1)

  (when (> lsp-bridge-call-hierarchy-preview-height 0)
    (split-window (selected-window) nil 'down)
    (other-window 1)
    (set-window-text-height (selected-window)  (1- lsp-bridge-call-hierarchy-preview-height))
    (other-window -1))

  (lsp-bridge-call-hierarchy-maybe-preview)

  (lsp-bridge-call-hierarchy-mode))

(defun lsp-bridge-call-hierarchy--move (next)
  (when (and (>= (+ lsp-bridge-call-hierarchy--index next) 0)
             (< (+ lsp-bridge-call-hierarchy--index next)
                (length lsp-bridge-call-hierarchy--popup-response)))
      (forward-line next)
      (move-overlay lsp-bridge-call-hierarchy--overlay (line-beginning-position) (line-end-position))
      (setq lsp-bridge-call-hierarchy--index
            (+ next lsp-bridge-call-hierarchy--index))
      (lsp-bridge-call-hierarchy-maybe-preview)
      ))

(defun lsp-bridge-call-hierarchy-next ()
  (interactive)
  (lsp-bridge-call-hierarchy--move 1))

(defun lsp-bridge-call-hierarchy-prev ()
  (interactive)
  (lsp-bridge-call-hierarchy--move -1))

(defun lsp-bridge-call-hierarchy-quit ()
  (interactive)
  (posframe-delete "*lsp-bridge-call-hierarchy*"))

(defun lsp-bridge-call-hierarchy-show ()
  (let* ((call  (nth lsp-bridge-call-hierarchy--index lsp-bridge-call-hierarchy--popup-response))
         (path (plist-get call :path))
         (range-start (plist-get (plist-get call :range) :start))
         (range-end (plist-get (plist-get call :range) :end))
         (sline (plist-get range-start :line))
         (scharacter (plist-get range-start :character))
         (eline (plist-get range-end :line))
         (echaracter (plist-get range-end :character)))
    (find-file path)
    (goto-char (point-min))
    (forward-line sline)
    (goto-char (+ (point-at-bol) scharacter))
    (if (equal sline eline)
        (pulse-momentary-highlight-region (point)
                                          (+ (point) (- echaracter scharacter)))
      (pulse-momentary-highlight-region (point)
                                        (save-excursion
                                          (forward-line (- eline sline))
                                          (goto-char (+ (point-at-bol) echaracter))
                                          (point))))))

(defun lsp-bridge-call-hierarchy-select ()
  (interactive)
  (lsp-bridge-call-hierarchy-show)
  (lsp-bridge-call-hierarchy-quit))

(defvar lsp-bridge-call-hierarchy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") 'lsp-bridge-call-hierarchy-next)
    (define-key map (kbd "k") 'lsp-bridge-call-hierarchy-prev)
    (define-key map (kbd "RET") 'lsp-bridge-call-hierarchy-select)
    (define-key map (kbd "ESC") 'lsp-bridge-call-hierarchy-quit)
    (define-key map (kbd "q") 'lsp-bridge-call-hierarchy-quit)
  map))

(define-derived-mode lsp-bridge-call-hierarchy-mode text-mode "call hierarchy"
  ;; Kill all local variables.
  (kill-all-local-variables)
  ;; Switch new mode.
  (setq major-mode 'lsp-bridge-call-hierarchy-mode)
  (setq mode-name "call hierarchy")
  (setq-local cursor-type nil)
  (goto-char (1+ (point-min)))
  (setq lsp-bridge-call-hierarchy--overlay (make-overlay (line-beginning-position)  (line-end-position)))
  (overlay-put lsp-bridge-call-hierarchy--overlay 'face 'highlight)
  (setq lsp-bridge-call-hierarchy--index 0)

  ;; Injection keymap.
  (use-local-map lsp-bridge-call-hierarchy-mode-map)
  (when (featurep 'evil)
    (evil-set-initial-state 'lsp-bridge-call-hierarchy-mode 'emacs))
  (setq-local cursor-type nil)
  (read-only-mode 1)
)

(provide 'lsp-bridge-call-hierarchy)
;;; lsp-bridge-call-hierarchy.el ends here
