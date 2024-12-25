;;; lsp-bridge-diagnostic.el --- Diagnostic for lsp-bridge   -*- lexical-binding: t; no-byte-compile: t; -*-*-

;; Filename: lsp-bridge-diagnostic.el
;; Description: Diagnostic for lsp-bridge
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-12-14 19:55:28
;; Version: 0.1
;; Last-Updated: 2022-12-14 19:55:28
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/lsp-bridge-diagnostic
;; Keywords:
;; Compatibility: GNU Emacs 28.2
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Diagnostic for lsp-bridge
;;

;;; Installation:
;;
;; Put lsp-bridge-diagnostic.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lsp-bridge-diagnostic)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET lsp-bridge-diagnostic RET
;;

;;; Change log:
;;
;; 2022/12/14
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'acm)
(require 'acm-frame)

;;; Code:

(defcustom lsp-bridge-diagnostic-fetch-idle 0.5
  "The idle seconds to fetch diagnostics."
  :type 'float
  :group 'lsp-bridge)

(defcustom lsp-bridge-diagnostic-enable-overlays t
  "If non-nil, rendering overlays in lsp-bridge-diagnostic byself.
Set to nil if flymake/flycheck frontend is used instead."
  :type 'boolean
  :group 'lsp-bridge)

(defcustom lsp-bridge-diagnostic-update-hook '()
  "The hook for diagnostics updates."
  :type 'hook
  :group 'lsp-bridge)

(defface lsp-bridge-diagnostics-error-face
  '((t (:underline (:style wave :color "Red1"))))
  "Face error diagnostic."
  :group 'lsp-bridge)

(defface lsp-bridge-diagnostics-warning-face
  '((t (:underline (:style wave :color "DarkOrange"))))
  "Face warning diagnostic."
  :group 'lsp-bridge)

(defface lsp-bridge-diagnostics-info-face
  '((t (:underline (:style wave :color "ForestGreen"))))
  "Face info diagnostic."
  :group 'lsp-bridge)

(defface lsp-bridge-diagnostics-hint-face
  '((t (:underline (:style wave :color "grey"))))
  "Face hint diagnostic."
  :group 'lsp-bridge)

(defcustom lsp-bridge-diagnostic-buffer " *lsp-bridge-diagnostic*"
  "Buffer for display diagnostic information."
  :type 'string
  :group 'lsp-bridge)

(defcustom lsp-bridge-diagnostic-tooltip-border-width 20
  "The border width of lsp-bridge diagnostic tooltip, in pixels."
  :type 'integer
  :group 'lsp-bridge)

(defcustom lsp-bridge-diagnostic-display-errors-delay 0.25
  "The delay in seconds before displaying errors."
  :type 'integer
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-hover-diagnostic nil
  "Whether to popup diagnostic message when hover diagnostic place."
  :type 'boolean
  :group 'lsp-bridge)

(defcustom lsp-bridge-diagnostic-max-number 50
  "The maximum number of diagnostic will be render.

Too many diagnostic render will cause do long time Emacs GC."
  :type 'integer
  :group 'lsp-bridge)

(defcustom lsp-bridge-diagnostic-hide-severities nil
  "The rule to hide diagnostic that return from LSP server.

1 mean error
2 mean warning
3 mean info
4 mean hint

You can set this value with `(2 3 4) if you just need render error diagnostic."
  :type 'list
  :group 'lsp-bridge)

(defvar-local lsp-bridge-diagnostic-records nil)

(defvar-local lsp-bridge-diagnostic-count nil)

(defvar-local lsp-bridge-diagnostic-overlays '())

(defvar lsp-bridge-diagnostic-frame nil)

(defvar lsp-bridge-diagnostic-display-error-at-point-timer nil)

(autoload 'lsp-bridge--with-file-buffer "lsp-bridge")

(defun lsp-bridge-diagnostic-hide-tooltip ()
  (acm-frame-hide-frame lsp-bridge-diagnostic-frame))

(defun lsp-bridge-diagnostic-hide-overlays ()
  (when lsp-bridge-diagnostic-overlays
    (dolist (diagnostic-overlay lsp-bridge-diagnostic-overlays)
      (delete-overlay diagnostic-overlay)))

  (setq-local lsp-bridge-diagnostic-overlays nil))

(defvar lsp-bridge-diagnostic-lock (make-mutex "lsp-bridge-diagnostic-lock"))

(defun lsp-bridge-diagnostic--render (filepath filehost diagnostics diagnostic-count)
  ;; lsp-bridge is too fast, use locks to avoid interface disorder issues caused by multi-threaded rendering of overlays
  (with-mutex lsp-bridge-diagnostic-lock
    (lsp-bridge--with-file-buffer filepath filehost
                                  (setq-local lsp-bridge-diagnostic-count diagnostic-count)

                                  (setq-local lsp-bridge-diagnostic-records diagnostics)

                                  (run-hooks 'lsp-bridge-diagnostic-update-hook)

                                  (when lsp-bridge-diagnostic-enable-overlays
                                    (lsp-bridge-diagnostic-hide-overlays)
                                    (let ((diagnostic-index 0)
                                          (diagnostic-number (length diagnostics)))
                                      (dolist (diagnostic diagnostics)
                                        (let ((severity (plist-get diagnostic :severity)))
                                          (unless (member severity lsp-bridge-diagnostic-hide-severities)
                                            (let* ((diagnostic-start (acm-backend-lsp-position-to-point (plist-get (plist-get diagnostic :range) :start)))
                                                   (diagnostic-end (acm-backend-lsp-position-to-point (plist-get (plist-get diagnostic :range) :end)))
                                                   (overlay (if (eq diagnostic-start diagnostic-end)
                                                                ;; Adjust diagnostic end position if start and end is same position.
                                                                (make-overlay diagnostic-start (1+ diagnostic-start))
                                                              (make-overlay diagnostic-start diagnostic-end)))
                                                   (message (plist-get diagnostic :message))
                                                   (overlay-face (cl-case severity
                                                                   (1 'lsp-bridge-diagnostics-error-face)
                                                                   (2 'lsp-bridge-diagnostics-warning-face)
                                                                   (3 'lsp-bridge-diagnostics-info-face)
                                                                   (4 'lsp-bridge-diagnostics-hint-face))))
                                              (overlay-put overlay 'color (plist-get (face-attribute overlay-face :underline) :color))
                                              (overlay-put overlay 'face overlay-face)
                                              (overlay-put overlay 'message message)
                                              (overlay-put overlay
                                                           'display-message
                                                           (if (> diagnostic-number 1)
                                                               (format "[%s:%s] %s" (1+ diagnostic-index) diagnostic-number message)
                                                             message))
                                              (push overlay lsp-bridge-diagnostic-overlays))))

                                        (setq diagnostic-index (1+ diagnostic-index))))
                                    (setq-local lsp-bridge-diagnostic-overlays (reverse lsp-bridge-diagnostic-overlays))))))

(defvar lsp-bridge-diagnostic-frame nil)

(defun lsp-bridge-diagnostic-insert-colored-string (color text)
  "Insert a colored string at point in the current buffer."
  (let ((colored-text (propertize text 'face `(:foreground ,color))))
    (insert colored-text)))

(defun lsp-bridge-diagnostic-show-tooltip (diagnostic-overlay &optional goto-beginning)
  (let* ((diagnostic-display-message (overlay-get diagnostic-overlay 'display-message))
         (diagnostic-message (overlay-get diagnostic-overlay 'message))
         (diagnostic-wrapped-message (with-temp-buffer
                                       (insert diagnostic-display-message)
                                       (fill-region (point-min) (point-max))
                                       (buffer-string)))
         (foreground-color (plist-get (face-attribute (overlay-get diagnostic-overlay 'face) :underline) :color)))
    ;; Weather goto beginning of diagnostic.
    (when goto-beginning
      (goto-char (overlay-start diagnostic-overlay)))

    ;; Try scroll window when current command is diagnostic jump commands and code content exceeds screen.
    (when (and (> (point-max) (window-end))
               (member (format "%s" this-command) '("lsp-bridge-diagnostic-jump-next", "lsp-bridge-diagnostic-jump-prev")))
      ;; NOTE:
      ;; We need use redisplay function to make sure diagnostic show in visible area.
      ;; `redisplay' causes a slight flicker on the screen but currently there is no better solution.
      (redisplay t)

      ;; Adjusting the cursor position when it is too close to the edge of the window.
      (let* ((window-start-line (save-excursion
                                  (goto-char (window-start))
                                  (current-line)))
             (window-end-line (save-excursion
                                (goto-char (window-end))
                                (current-line)))
             (adjust-line-number 5))
        (ignore-errors
          (cond ((< (abs (- (current-line) window-start-line)) adjust-line-number)
                 (scroll-down-line adjust-line-number))
                ((< (abs (- (current-line) window-end-line)) adjust-line-number)
                 (scroll-up-line adjust-line-number))))))

    ;; Show diagnostic tooltip.
    (with-current-buffer (get-buffer-create lsp-bridge-diagnostic-buffer)
      (erase-buffer)
      (lsp-bridge-diagnostic-insert-colored-string (overlay-get diagnostic-overlay 'color) diagnostic-wrapped-message)
      (setq-local lsp-bridge-diagnostic-message diagnostic-message))

    (cond
     ((acm-frame-can-display-p)
      (acm-frame-new lsp-bridge-diagnostic-frame
                     lsp-bridge-diagnostic-buffer
                     "lsp bridge diagnostic frame")

      (set-face-background (acm-frame-border-face)
                           foreground-color
                           lsp-bridge-diagnostic-frame))
     (t (message diagnostic-message)))))

(defun lsp-bridge-diagnostic-maybe-display-error-at-point ()
  "Display error message at point with a delay, unless already displayed."
  (acm-cancel-timer lsp-bridge-diagnostic-display-error-at-point-timer)
  (when-let* ((ol (lsp-bridge-diagnostic-overlay-at-point)))
    (setq lsp-bridge-diagnostic-display-error-at-point-timer
          (run-at-time lsp-bridge-diagnostic-display-errors-delay nil
                       'lsp-bridge-diagnostic-show-tooltip ol))))

(defun lsp-bridge-in-diagnostic-overlay-area-p (overlay)
  (and
   lsp-bridge-diagnostic-frame
   (not (frame-visible-p lsp-bridge-diagnostic-frame))
   (>= (point) (overlay-start overlay))
   (<= (point) (overlay-end overlay))))

(defun lsp-bridge-diagnostic-jump-next ()
  (interactive)
  (if (zerop (length lsp-bridge-diagnostic-overlays))
      (message "[LSP-Bridge] No diagnostics.")
    (if-let* ((diagnostic-overlay (cl-find-if
                                  (lambda (overlay)
                                    (or (< (point) (overlay-start overlay))
                                        ;; Show diagnostic information around cursor if diagnostic frame is not visiable.
                                        (lsp-bridge-in-diagnostic-overlay-area-p overlay)))
                                  lsp-bridge-diagnostic-overlays)))
        (lsp-bridge-diagnostic-show-tooltip diagnostic-overlay t)
      (message "[LSP-Bridge] Reach last diagnostic."))))

(defun lsp-bridge-diagnostic-jump-prev ()
  (interactive)
  (if (zerop (length lsp-bridge-diagnostic-overlays))
      (message "[LSP-Bridge] No diagnostics.")
    (if-let* ((diagnostic-overlay (cl-find-if
                                  (lambda (overlay)
                                    (or (> (point) (overlay-end overlay))
                                        ;; Show diagnostic information around cursor if diagnostic frame is not visiable.
                                        (lsp-bridge-in-diagnostic-overlay-area-p overlay)))
                                  (reverse lsp-bridge-diagnostic-overlays))))
        (lsp-bridge-diagnostic-show-tooltip diagnostic-overlay t)
      (message "[LSP-Bridge] Reach first diagnostic."))))

(defun lsp-bridge-diagnostic-overlay-at-point ()
  (cl-dolist (overlay lsp-bridge-diagnostic-overlays)
    (when (and (>= (point) (overlay-start overlay))
               (< (point) (overlay-end overlay)))
      (cl-return overlay)
      )))

(defun lsp-bridge-diagnostic-copy ()
  (interactive)
  (if (zerop (length lsp-bridge-diagnostic-overlays))
      (message "[LSP-Bridge] No diagnostics.")
    (when-let* ((overlay (lsp-bridge-diagnostic-overlay-at-point)))
      (let ((diagnostic-message (overlay-get overlay 'message)))
        (kill-new diagnostic-message)
        (message "Copy diagnostics: '%s'" diagnostic-message)))))

(defun lsp-bridge-workspace-diagnostic-list ()
  (interactive)
  (lsp-bridge-call-file-api "workspace_diagnostics"))

(defun lsp-bridge-diagnostic-list ()
  (interactive)
  (lsp-bridge-call-file-api "list_diagnostics" lsp-bridge-diagnostic-hide-severities))

(defun lsp-bridge-diagnostic--list (diagnostics)
  (let ((filepath acm-backend-lsp-filepath)
        (current-buffer (current-buffer))
        (diagnostic-counter 0))
    (with-temp-buffer
      (insert (concat "\n" "\033[95m" filepath "\033[0m" "\n"))
      (dolist (diagnostic diagnostics)
        (let* ((range (plist-get diagnostic :range))
               (message (plist-get diagnostic :message))
               (start (plist-get range :start))
               (end (plist-get range :end))
               (start-line (1+ (plist-get start :line)))
               (start-column (plist-get start :character))
               (end-line (1+ (plist-get end :line)))
               (end-column (plist-get end :character))
               (line-content (with-current-buffer current-buffer
                               (save-excursion
                                 (goto-line start-line)
                                 (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
               (content-end-column (if (eq start-line end-line) end-column (string-width line-content))))
          (insert (concat "\033[93m" (format "%s %s" (1+ diagnostic-counter) message) "\033[0m" "\n"))

          ;; `start' point and `end' point will same if the diagnostic message is for a location rather than region.
          ;; Then we need adjust end-column to highlight diagnostic location.
          (when (equal start-column end-column)
            (setq end-column (1+ end-column)))

          (insert (format "%s:%s:%s\n\n"
                          start-line
                          start-column
                          (concat (substring line-content 0 start-column)
                                  "\033[94m"
                                  (substring line-content start-column content-end-column)
                                  (let ((line-difference (- end-line start-line)))
                                    (unless (eq line-difference 0)
                                      (format "... (+%d line%s)" line-difference (when (> line-difference 1) "s"))))
                                  "\033[0m"
                                  (substring line-content content-end-column))))

          (setq diagnostic-counter (1+ diagnostic-counter))))
      (lsp-bridge-ref-popup (buffer-string) diagnostic-counter))))

(provide 'lsp-bridge-diagnostic)

;;; lsp-bridge-diagnostic.el ends here
