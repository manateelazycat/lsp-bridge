;;; lsp-bridge-call-hierarchy.el --- LSP bridge  -*- lexical-binding: t -*-
(require 'acm-frame)

(defvar lsp-bridge-call-hierarchy-name-width 40
  "Width of the call name column in the call hierarchy buffer.")

(defvar lsp-bridge-call-hierarchy-path-width 60
  "Width of the path column in the call hierarchy buffer.")

(defvar lsp-bridge-call-hierarchy-call-max-height 30
  "Maximum height of the call hierarchy buffer.")

(defvar lsp-bridge-call-hierarchy-preview-height 30
  "Height of the preview window. (set 0 to disable preview)")

(defvar lsp-bridge-call-hierarchy--frame nil)

(defvar lsp-bridge-call-hierarchy--index 0)

(defvar lsp-bridge-call-hierarchy--overlay nil)

(defvar lsp-bridge-call-hierarchy--popup-response nil)

(defvar lsp-bridge-call-hierarchy--temp-buffers nil)

(defvar lsp-bridge-call-hierarchy--local-modelines nil)

(defun lsp-bridge-incoming-call-hierarchy ()
  (interactive)
  (lsp-bridge-call-file-api "prepare_call_hierarchy_incoming"
                            (lsp-bridge--position)))

(defun lsp-bridge-outgoing-call-hierarchy ()
  (interactive)
  (lsp-bridge-call-file-api "prepare_call_hierarchy_outgoing"
                            (lsp-bridge--position)))


(defun lsp-bridge-call-hierarchy-make-frame (buffer)
  (let* ((height (+ lsp-bridge-call-hierarchy-preview-height
                    (min (length lsp-bridge-call-hierarchy--popup-response)
                         lsp-bridge-call-hierarchy-call-max-height)))
         (width (+ 6 ;; length of spaces and icon
                   lsp-bridge-call-hierarchy-name-width
                   lsp-bridge-call-hierarchy-path-width)))
    (acm-frame-create-frame-if-not-exist lsp-bridge-call-hierarchy--frame buffer
                                                "call hierarchy" 0 nil)
    (acm-frame-set-frame-size lsp-bridge-call-hierarchy--frame width height)
    (acm-frame-set-frame-colors lsp-bridge-call-hierarchy--frame)
    (acm-frame-set-frame-pos-center lsp-bridge-call-hierarchy--frame)))

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

    (if (and mode-line-format
             (not (member (current-buffer) lsp-bridge-call-hierarchy--temp-buffers)))
      (add-to-list 'lsp-bridge-call-hierarchy--local-modelines
                   (list (current-buffer) mode-line-format)))

    (setq-local mode-line-format nil)
    (other-window -1)))

(defun lsp-bridge-call-hierarchy--popup (response)
  (interactive)
  (setq lsp-bridge-call-hierarchy--popup-response response)
  (lsp-bridge-call-hierarchy-make-frame (get-buffer-create "*lsp-bridge-call-hierarchy*"))

  (with-current-buffer "*lsp-bridge-call-hierarchy*"
    (dolist (call lsp-bridge-call-hierarchy--popup-response)
      (insert (lsp-bridge-call-hierarchy-format-call call)))
    (delete-backward-char 1))

  (when (> lsp-bridge-call-hierarchy-preview-height 0)
    (split-window (selected-window) nil 'down)
    (other-window 1)
    (set-window-text-height (selected-window)  (1- lsp-bridge-call-hierarchy-preview-height))
    (other-window -1))

  (setq lsp-bridge-call-hierarchy--temp-buffers nil)
  (setq lsp-bridge-call-hierarchy--index 0)

  (lsp-bridge-call-hierarchy-maybe-preview)

  (lsp-bridge-call-hierarchy-mode)
  (with-current-buffer "*lsp-bridge-call-hierarchy*"
    (setq-local cursor-type nil)
    (setq-local truncate-lines t)
    (setq-local mode-line-format nil)))

(defun lsp-bridge-call-hierarchy--move (next)
  (when (and (>= (+ lsp-bridge-call-hierarchy--index next) 0)
             (< (+ lsp-bridge-call-hierarchy--index next)
                (length lsp-bridge-call-hierarchy--popup-response)))
      (forward-line next)
      (move-overlay lsp-bridge-call-hierarchy--overlay
                    (line-beginning-position) (1+ (line-end-position)))
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

  (acm-frame-delete-frame lsp-bridge-call-hierarchy--frame)
  (kill-buffer "*lsp-bridge-call-hierarchy*")

  (dolist (buffer-modeline lsp-bridge-call-hierarchy--local-modelines)
    (with-current-buffer (car buffer-modeline)
      (setq-local mode-line-format (cdr buffer-modeline))))

  (dolist (buffer lsp-bridge-call-hierarchy--temp-buffers)
    (kill-buffer buffer)))

(defun lsp-bridge-call-hierarchy-show ()
  (let* ((call  (nth lsp-bridge-call-hierarchy--index lsp-bridge-call-hierarchy--popup-response))
         (path (lsp-bridge-pick-file-path (plist-get call :path)))
         (range-start (plist-get (plist-get call :range) :start))
         (range-end (plist-get (plist-get call :range) :end))
         (sline (plist-get range-start :line))
         (scharacter (plist-get range-start :character))
         (eline (plist-get range-end :line))
         (echaracter (plist-get range-end :character))
         (found-buffer))

    (if (dolist (buffer (buffer-list) found-buffer)
          (when (string-equal (buffer-file-name buffer) path)
            (setq found-buffer buffer)))
        (find-file path)
      (add-to-list 'lsp-bridge-call-hierarchy--temp-buffers (find-file path)))

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
  (lsp-bridge-call-hierarchy-quit)
  (lsp-bridge-call-hierarchy-show))

(defvar lsp-bridge-call-hierarchy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") 'lsp-bridge-call-hierarchy-next)
    (define-key map (kbd "C-n") 'lsp-bridge-call-hierarchy-next)
    (define-key map (kbd "k") 'lsp-bridge-call-hierarchy-prev)
    (define-key map (kbd "C-p") 'lsp-bridge-call-hierarchy-prev)
    (define-key map (kbd "RET") 'lsp-bridge-call-hierarchy-select)
    (define-key map (kbd "ESC") 'lsp-bridge-call-hierarchy-quit)
    (define-key map (kbd "q") 'lsp-bridge-call-hierarchy-quit)
    (define-key map (kbd "C-g") 'lsp-bridge-call-hierarchy-quit)
  map))

(define-derived-mode lsp-bridge-call-hierarchy-mode text-mode "call hierarchy"
  ;; Kill all local variables.
  (kill-all-local-variables)
  ;; Switch new mode.
  (setq major-mode 'lsp-bridge-call-hierarchy-mode)
  (setq mode-name "call hierarchy")
  (goto-char (1+ (point-min)))
  (setq lsp-bridge-call-hierarchy--overlay (make-overlay (line-beginning-position)
                                                         (1+ (line-end-position))))
  ;; :extend extend face after EOL
  (set-face-attribute 'lsp-bridge-font-lock-flash (selected-frame) :extend t)
  (overlay-put lsp-bridge-call-hierarchy--overlay 'face 'lsp-bridge-font-lock-flash)
  (setq lsp-bridge-call-hierarchy--index 0)

  ;; Injection keymap.
  (use-local-map lsp-bridge-call-hierarchy-mode-map)
  (when (featurep 'evil)
    (evil-set-initial-state 'lsp-bridge-call-hierarchy-mode 'emacs))
  (read-only-mode 1))

(provide 'lsp-bridge-call-hierarchy)
;;; lsp-bridge-call-hierarchy.el ends here
