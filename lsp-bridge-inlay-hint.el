;;; lsp-bridge-inlay-hint.el --- Inlay hint protocol   -*- lexical-binding: t; -*-

;; Filename: lsp-bridge-inlay-hint.el
;; Description: Inlay hint protocol
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2023, Andy Stewart, all rights reserved.
;; Created: 2023-10-03 21:35:08
;; Version: 0.1
;; Last-Updated: 2023-10-03 21:35:08
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/lsp-bridge-inlay-hint
;; Keywords:
;; Compatibility: GNU Emacs 30.0.50
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
;; Inlay hint protocol
;;

;;; Installation:
;;
;; Put lsp-bridge-inlay-hint.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lsp-bridge-inlay-hint)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET lsp-bridge-inlay-hint RET
;;

;;; Change log:
;;
;; 2023/10/03
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

(defun lsp-bridge-inlay-hint ()
  (lsp-bridge-call-file-api "inlay_hint"
                            (lsp-bridge--point-position (window-start))
                            ;; We need pass UPDATE argument to `window-end', to make sure it's value is update, not cache.
                            (lsp-bridge--point-position (window-end nil t))))

(defvar-local lsp-bridge-inlay-hint-overlays '())

(defun lsp-bridge-inlay-hint-hide-overlays ()
  (when lsp-bridge-inlay-hint-overlays
    (dolist (inlay-hint-overlay lsp-bridge-inlay-hint-overlays)
      (delete-overlay inlay-hint-overlay)))

  (setq-local lsp-bridge-inlay-hint-overlays nil))

(defun lsp-bridge-inlay-hint--render (filepath filehost inlay-hints)
  (lsp-bridge--with-file-buffer filepath filehost
                                (lsp-bridge-inlay-hint-hide-overlays)

                                (save-excursion
                                  (save-restriction
                                    (let ((hint-index 0))
                                      (dolist (hint inlay-hints)
                                        (goto-char (acm-backend-lsp-position-to-point (plist-get hint :position)))
                                        (let* ((hint-kind (plist-get hint :kind))
                                               (hint-peg-after-p (eql hint-kind 1))
                                               (hint-text (format "%s"
                                                                  (cond ((listp (plist-get hint :label))
                                                                         (apply 'concat
                                                                                (mapcar (lambda (label)
                                                                                          (concat (plist-get label :value) " "))
                                                                                        (plist-get hint :label)
                                                                                        )))
                                                                        (t (plist-get hint :label)))))
                                               (hint-padding-left (plist-get hint :paddingLeft))
                                               (hint-padding-right (plist-get hint :paddingRight))
                                               (hint-padding-left-text (and hint-padding-left
                                                                            (not (eq hint-padding-left :json-false))
                                                                            (not (memq (char-before) '(32 9)))
                                                                            " "))
                                               (hint-padding-right-text (and hint-padding-right
                                                                             (not (eq hint-padding-right :json-false))
                                                                             (not (memq (char-after) '(32 9)))
                                                                             " "))
                                               (hint-insert-text (concat hint-padding-left-text hint-text hint-padding-right-text))
                                               (overlay (if hint-peg-after-p
                                                            (make-overlay (point) (1+ (point)) nil t)
                                                          (make-overlay (1- (point)) (point) nil nil nil))))
                                          (when (and (equal hint-index 0)
                                                     hint-peg-after-p)
                                            (put-text-property 0 1 'cursor 1 hint-insert-text))
                                          (overlay-put overlay
                                                       (if hint-peg-after-p 'before-string 'after-string)
                                                       (propertize hint-insert-text 'face  `(:foreground "#aaaaaa")))
                                          (overlay-put overlay 'evaporate t)
                                          (push overlay lsp-bridge-inlay-hint-overlays)

                                          (setq hint-index (1+ hint-index))
                                          )))))))

(provide 'lsp-bridge-inlay-hint)

;;; lsp-bridge-inlay-hint.el ends here
