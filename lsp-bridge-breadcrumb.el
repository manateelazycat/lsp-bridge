;;; lsp-bridge-breadcrumb.el --- Show symbol breadcrumb on headerline.  -*- lexical-binding: t; -*-
;; Copyright (C) 2025  Dimas Firmansyah
;; Author: Dimas Firmansyah <deirn@bai.lol>
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;; Show symbol breadcrumb on headerline.
;;
;;; Code:

(require 'acm-icon)

(defgroup lsp-bridge-breadcrumb nil
  "Show breadcrumb on headerline."
  :prefix "lsp-bridge-breadcrumb-"
  :group 'lsp-bridge)

(defcustom lsp-bridge-breadcrumb-idle-delay 0.5
  "Idle seconds before updating breadcrumb."
  :type 'float
  :group 'lsp-bridge-breadcrumb)

(defcustom lsp-bridge-breadcrumb-show-icon (display-graphic-p)
  "Wheter to show icons on breadcrumb."
  :type 'boolean
  :group 'lsp-bridge-breadcrumb)

(defcustom lsp-bridge-breadcrumb-bar-height (+ (window-font-height nil 'header-line) 4)
  "How tall the bar should be.  This controls the header line height when in GUI."
  :type 'integer
  :group 'lsp-bridge-breadcrumb)

(defcustom lsp-bridge-breadcrumb-separator (propertize ">" 'face 'shadow)
  "The breadcrumb separator."
  :type 'string
  :group 'lsp-bridge-breadcrumb)

(defcustom lsp-bridge-breadcrumb-max-segment-length 20
  "Max character length for each segment."
  :type 'integer
  :group 'lsp-bridge-breadcrumb)

(defvar-local lsp-bridge-breadcrumb--timer nil)
(defvar-local lsp-bridge-breadcrumb--last-point nil)
(defvar-local lsp-bridge-breadcrumb--last-update-tick nil)
(defvar-local lsp-bridge-breadcrumb--last-response nil)
(defvar-local lsp-bridge-breadcrumb--last-header-line nil)
(defvar-local lsp-bridge-breadcrumb--in-process nil)

(defvar lsp-bridge-breadcrumb--bar-img nil)
(defun lsp-bridge-breadcrumb--bar-img ()
  "Based on `doom-modeline--create-bar-image'."
  (if lsp-bridge-breadcrumb--bar-img lsp-bridge-breadcrumb--bar-img
    (when (and (display-graphic-p) (image-type-available-p 'pbm))
      (setq lsp-bridge-breadcrumb--bar-img
            (propertize
             " " 'display
             (let ((width (window-font-width nil 'header-line))
                   (height lsp-bridge-breadcrumb-bar-height)
                   (color (face-background 'header-line nil t)))
               (ignore-errors
                 (create-image
                  (concat (format "P1\n%i %i\n" width height)
                          (make-string (* width height) ?1)
                          "\n")
                  'pbm t :scale 1 :foreground color :ascent 'center)))))
      lsp-bridge-breadcrumb--bar-img)))

(defvar lsp-bridge-breadcrumb--icon-cache (make-hash-table :test 'equal))
(defun lsp-bridge-breadcrumb--icon (kind active)
  "Get icon for KIND and ACTIVE."
  (if (and lsp-bridge-breadcrumb-show-icon
           (image-type-available-p 'svg))
      (let* ((icon-key (format "%s_%s" kind active))
             (icon-text (gethash icon-key lsp-bridge-breadcrumb--icon-cache)))
        (unless icon-text
          (let* ((icon (cdr (assoc (downcase kind) acm-icon-alist)))
                 (display-icon (or icon (cdr (assoc t acm-icon-alist)))))
            (setq icon-text (propertize
                             (apply #'concat (make-list acm-icon-width "-"))
                             'display (acm-icon (nth 0 display-icon) (nth 1 display-icon)
                                                (if active (nth 2 display-icon) (face-foreground 'mode-line-inactive nil t))))))
          (puthash icon-key icon-text lsp-bridge-breadcrumb--icon-cache))
        icon-text)
    " "))

(defun lsp-bridge-breadcrumb--update (window)
  "Update the breadcrumb for WINDOW."
  (let ((active (eq (selected-window) window)))
    (if lsp-bridge-breadcrumb--last-response
        (let* ((segments-face (if active 'header-line `(:foreground ,(face-foreground 'mode-line-inactive nil t))))
               (segments
                (mapcar
                 (lambda (s)
                   (let* ((name (plist-get s :name))
                          (name-length (length name))
                          (pos (plist-get s :pos))
                          (kind (plist-get s :kind)))
                     (concat
                      "\u200b"
                      (lsp-bridge-breadcrumb--icon kind active)
                      (propertize
                       (if (<= name-length lsp-bridge-breadcrumb-max-segment-length) name
                         (concat (substring name 0 (min name-length (- lsp-bridge-breadcrumb-max-segment-length 1))) "…"))
                       'face segments-face
                       'mouse-face 'highlight
                       'help-echo (concat name "\nmouse-1: Go to definition")
                       'local-map
                       (let ((map (make-sparse-keymap)))
                         (define-key
                          map [header-line mouse-1]
                          #'(lambda ()
                              (interactive)
                              (select-window window)
                              (when (fboundp 'evil-set-jump) (evil-set-jump))
                              (goto-char (acm-backend-lsp-position-to-point pos))))
                         map))
                      " ")))
                 lsp-bridge-breadcrumb--last-response))
               (segments-text (string-join segments lsp-bridge-breadcrumb-separator))
               (header-text (concat
                             (lsp-bridge-breadcrumb--bar-img)
                             lsp-bridge-breadcrumb-separator
                             segments-text))
               (text-width (string-width segments-text))
               (sep-width (string-width lsp-bridge-breadcrumb-separator))
               (max-width (- (window-max-chars-per-line window 'header-line) 3 sep-width)))
          (when (> text-width max-width)
            (let* ((sub-text (substring header-text (- text-width max-width)))
                   (first-segment-pos (string-match "\u200b" sub-text))
                   (sub-text (substring sub-text first-segment-pos)))
              (setq header-text (concat
                                 (lsp-bridge-breadcrumb--bar-img)
                                 lsp-bridge-breadcrumb-separator
                                 (propertize " … " 'face segments-face)
                                 lsp-bridge-breadcrumb-separator
                                 sub-text))))
          (set-window-parameter
           window 'lsp-bridge-breadcrumb--last-header-line
           header-text))
      (set-window-parameter
       window 'lsp-bridge-breadcrumb--last-header-line
       (lsp-bridge-breadcrumb--bar-img)))))

(defun lsp-bridge-breadcrumb--call ()
  "Call backend."
  (if (lsp-bridge-call-file-api-p)
      (progn
        (setq lsp-bridge-breadcrumb--in-process t)
        (lsp-bridge-call-file-api "breadcrumb" (lsp-bridge--position)))
    (lsp-bridge-breadcrumb--disable)))

(defun lsp-bridge-breadcrumb--callback (filename filehost response)
  "RESPONSE callback from backend for FILENAME:FILEHOST."
  (lsp-bridge--with-file-buffer
      filename filehost
      (setq lsp-bridge-breadcrumb--in-process nil)
      (setq lsp-bridge-breadcrumb--last-response response)
      (dolist (w (get-buffer-window-list)) (lsp-bridge-breadcrumb--update w))))

(defun lsp-bridge-breadcrumb--header-line (&optional window)
  "The breadcrumb header line for WINDOW."
  (let* ((modified-tick (buffer-chars-modified-tick))
         (changed (or (not lsp-bridge-breadcrumb--last-update-tick)
                      (/= modified-tick lsp-bridge-breadcrumb--last-update-tick)))
         (current-point (point))
         (moved (or (not lsp-bridge-breadcrumb--last-point)
                    (/= current-point lsp-bridge-breadcrumb--last-point))))
    (when (and (or changed moved) (not lsp-bridge-breadcrumb--in-process))
      (setq lsp-bridge-breadcrumb--last-update-tick modified-tick)
      (setq lsp-bridge-breadcrumb--last-point current-point)
      (when lsp-bridge-breadcrumb--timer (cancel-timer lsp-bridge-breadcrumb--timer))
      (setq lsp-bridge-breadcrumb--timer (run-with-idle-timer lsp-bridge-breadcrumb-idle-delay nil #'lsp-bridge-breadcrumb--call)))
    (or (window-parameter window 'lsp-bridge-breadcrumb--last-header-line)
        (lsp-bridge-breadcrumb--bar-img))))

(defun lsp-bridge-breadcrumb--on-window-change (window)
  "Update the breadcrumb on WINDOW change."
  (with-current-buffer (window-buffer window)
    (lsp-bridge-breadcrumb--update window)
    (when (eq (selected-window) window)
      (setq lsp-bridge-breadcrumb--last-point nil)
      (lsp-bridge-breadcrumb--header-line window))))

(defun lsp-bridge-breadcrumb-goto-last-definition ()
  "Go to the definition of the last breadcrumb symbol."
  (interactive)
  (when-let* ((symbol (car (last lsp-bridge-breadcrumb--last-response)))
              (pos (plist-get symbol :pos)))
    (when (fboundp 'evil-set-jump) (evil-set-jump))
    (goto-char (acm-backend-lsp-position-to-point pos))))

(defun lsp-bridge-breadcrumb--disable ()
  "Disable `lsp-bridge-breadcrumb-mode'."
  (lsp-bridge-breadcrumb-mode -1))

;;;###autoload
(define-minor-mode lsp-bridge-breadcrumb-mode
  "Minor mode to show symbol breadcrumb on headerline."
  :init-value nil
  (cond
   (lsp-bridge-breadcrumb-mode
    (when (lsp-bridge-call-file-api-p)
      (setq-local window-selection-change-functions (cons #'lsp-bridge-breadcrumb--on-window-change window-selection-change-functions))
      (setq-local window-size-change-functions (cons #'lsp-bridge-breadcrumb--on-window-change window-size-change-functions))
      (add-to-list 'header-line-format '(:eval (lsp-bridge-breadcrumb--header-line)))))
   (t
    (setq-local window-selection-change-functions (delete #'lsp-bridge-breadcrumb--on-window-change window-selection-change-functions))
    (setq-local window-size-change-functions (delete #'lsp-bridge-breadcrumb--on-window-change window-size-change-functions))
    (setq header-line-format (delete '(:eval (lsp-bridge-breadcrumb--header-line)) header-line-format)))))

(provide 'lsp-bridge-breadcrumb)
;;; lsp-bridge-breadcrumb.el ends here
