;;; lbcf.el --- LSP Bridge Completion Frame

;; Filename: lbcf.el
;; Description: LSP Bridge Completion Frame
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-05-09 16:27:19
;; Version: 0.1
;; Last-Updated: 2022-05-09 16:27:19
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/lbcf
;; Keywords:
;; Compatibility: GNU Emacs 28.1
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
;; LSP Bridge Completion Frame
;;

;;; Installation:
;;
;; Put lbcf.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lbcf)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET lbcf RET
;;

;;; Change log:
;;
;; 2022/05/09
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

;;; Code:

(defvar lbcf--frame nil)

(defvar lbcf--buffer-parameters
  '((mode-line-format . nil)
    (header-line-format . nil)
    (tab-line-format . nil)
    (tab-bar-format . nil) ;; Emacs 28 tab-bar-format
    (frame-title-format . "")
    (truncate-lines . t)
    (cursor-in-non-selected-windows . nil)
    (cursor-type . nil)
    (show-trailing-whitespace . nil)
    (display-line-numbers . nil)
    (left-fringe-width . nil)
    (right-fringe-width . nil)
    (left-margin-width . 0)
    (right-margin-width . 0)
    (fringes-outside-margins . 0)
    (buffer-read-only . t))
  "Default child frame buffer parameters.")

(defun lbcf-show (candidates)
  (let* ((popup-info (lbcf-get-frame-popup-pos candidates))
         (background-color (lbcf-get-frame-background-color)))
    (unless lbcf--frame
      (setq lbcf--frame (make-frame
                         `((parent-frame . (window-frame))
                           (background-color . ,background-color)
                           (minibuffer . nil)
                           (no-accept-focus . t)
                           (no-focus-on-map . t)
                           (min-width . t)
                           (min-height . t)
                           (width . 0)
                           (height . 0)
                           (border-width . 0)
                           (child-frame-border-width . 1)
                           (internal-border-width . 15)
                           (left-fringe . 0)
                           (right-fringe . 0)
                           (vertical-scroll-bars . nil)
                           (horizontal-scroll-bars . nil)
                           (menu-bar-lines . 0)
                           (tool-bar-lines . 0)
                           (tab-bar-lines . 0)
                           (no-other-frame . t)
                           (no-other-window . t)
                           (no-delete-other-windows . t)
                           (unsplittable . t)
                           (undecorated . t)
                           (cursor-type . nil)
                           (visibility . nil)
                           (no-special-glyphs . t)
                           (desktop-dont-save . t)))))
    (let ((win (frame-root-window lbcf--frame)))
      (set-window-buffer win (lbcf--make-buffer candidates))
      (set-window-dedicated-p win t))
    (set-frame-size lbcf--frame (nth 2 popup-info) (nth 3 popup-info) t)
    (set-frame-position lbcf--frame (nth 0 popup-info) (nth 1 popup-info))
    (make-frame-visible lbcf--frame)))

(defun lbcf-get-frame-popup-pos (candidates)
  (let* ((edge (window-inside-pixel-edges))
         (cursor-pos (window-absolute-pixel-position))
         (cursor-x (car cursor-pos))
         (cursor-y (cdr cursor-pos))
         (char-width (default-font-width))
         (line-height (default-line-height))
         (frame-width (lbcf-get-frame-width candidates))
         (frame-height (lbcf-get-frame-height candidates))
         (popup-x (+ cursor-x char-width))
         (popup-y (+ cursor-y line-height)))
    (when (> (+ popup-y frame-height) (nth 3 edge))
      (setq popup-y (- cursor-y line-height frame-height)))
    (when (> (+ popup-x frame-width) (nth 2 edge))
      (setq popup-x (- cursor-x char-width)))
    (list popup-x popup-y frame-width frame-height)
    ))

(defun lbcf-get-frame-background-color ()
  (if (string-equal (format "%s" (frame-parameter nil 'background-mode)) "dark")
      "#191a1b"
    "#f0f0f0"))

(defun lbcf-get-frame-width (candidates)
  (* (reduce #'max (mapcar #'length candidates)) (default-font-width)))

(defun lbcf-get-frame-height (candidates)
  (* (min (length candidates) 10) (default-font-height)))

(defvar lbcf--mouse-ignore-map
  (let ((map (make-sparse-keymap)))
    (dotimes (i 7)
      (dolist (k '(mouse down-mouse drag-mouse double-mouse triple-mouse))
        (define-key map (vector (intern (format "%s-%s" k (1+ i)))) #'ignore)))
    map)
  "Ignore all mouse clicks.")

(defun lbcf--popup-redirect-focus ()
  "Redirect focus from popup."
  (redirect-frame-focus lbcf--frame (frame-parent lbcf--frame)))

(defface lbcf-content-buffer-face
  '((t (:height 140)))
  "Face for content area.")

(defun lbcf--make-buffer (candidates)
  "Create lbcf buffer with CONTENT."
  (let ((buffer (get-buffer-create " *lbcf*")))
    (with-current-buffer buffer
      (setq-local lbcf--completion-select-line 1)
      (setq-local lbcf--completion-select-item "")
      (setq-local lbcf--completion-candidates candidates)

      (add-hook 'pre-command-hook #'lbcf--popup-redirect-focus nil 'local)
      (use-local-map lbcf--mouse-ignore-map)
      (dolist (var lbcf--buffer-parameters)
        (set (make-local-variable (car var)) (cdr var)))

      (let ((inhibit-modification-hooks t)
            (inhibit-read-only t))
        (erase-buffer)

        (buffer-face-set 'lbcf-content-buffer-face)

        (dolist (candidate candidates)
          (insert (format "%s\n" candidate)))
        (delete-backward-char 1)
        (goto-char (point-min))

        (when (and (boundp 'lbcf-select-line-overlay)
                   lbcf-select-line-overlay)
          (delete-overlay lbcf-select-line-overlay))

        (set (make-local-variable 'lbcf-select-line-overlay) (make-overlay (point) (point) nil t t))
        (overlay-put lbcf-select-line-overlay 'face 'lbcf-select-line-face)

        (lbcf-select-line 1)
        ))
    buffer))

(defmacro with-lbcf-buffer (&rest body)
  `(let ((buffer (get-buffer-create " *lbcf*")))
     (with-current-buffer buffer
       ,@body
       )))

(defun lbcf-select-line (line)
  (with-lbcf-buffer
   (when (and (boundp 'lbcf-select-line-overlay)
              lbcf-select-line-overlay)
     (goto-line line)
     (setq-local lbcf--completion-select-item (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
     (move-overlay lbcf-select-line-overlay (line-beginning-position) (line-beginning-position 2)))))

(defun lbcf-select-next ()
  (with-lbcf-buffer
   (when (< lbcf--completion-select-line (length lbcf--completion-candidates))
     (setq-local lbcf--completion-select-line (+ 1 lbcf--completion-select-line))
     (lbcf-select-line lbcf--completion-select-line))))

(defun lbcf-select-prev ()
  (with-lbcf-buffer
   (when (> lbcf--completion-select-line 0)
     (setq-local lbcf--completion-select-line (- lbcf--completion-select-line 1))
     (lbcf-select-line lbcf--completion-select-line))))

(defun lbcf-select-first ()
  (with-lbcf-buffer
   (lbcf-select-line 1)))

(defun lbcf-select-last ()
  (with-lbcf-buffer
   (lbcf-select-line (length lbcf--completion-candidates))))

(defun lbcf-get-select-item ()
  (with-lbcf-buffer
   lbcf--completion-select-item))

(defface lbcf-select-line-face
  '((t (:foreground "White" :background "#007aff" :bold t)))
  "Face for keyword match."
  :group 'insert-translated-name)

(defun lbcf-hide ()
  (when (frame-visible-p lbcf--frame)
    (make-frame-invisible lbcf--frame)))

(provide 'lbcf)

;;; lbcf.el ends here
