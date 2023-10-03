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
                            (lsp-bridge--point-position (window-end))))

(defvar-local lsp-bridge-inlay-hint-overlays '())

(defun lsp-bridge-inlay-hint-hide-overlays ()
  (when lsp-bridge-inlay-hint-overlays
    (dolist (inlay-hint-overlay lsp-bridge-inlay-hint-overlays)
      (delete-overlay inlay-hint-overlay)))

  (setq-local lsp-bridge-inlay-hint-overlays nil))

(defun lsp-bridge-inlay-hint--render (filepath filehost inlay-hints)
  (lsp-bridge--with-file-buffer filepath filehost
                                (lsp-bridge-inlay-hint-hide-overlays)

                                (dolist (hint inlay-hints)
                                  (let* ((hint-start-pos (acm-backend-lsp-position-to-point (plist-get hint :position)))
                                         (hint-at-end-of-line-p (save-excursion
                                                                  (goto-char hint-start-pos)
                                                                  (eolp)))
                                         (hint-text (format "%s"
                                                            (cond ((listp (plist-get hint :label))
                                                                   (apply 'concat
                                                                          (mapcar (lambda (label)
                                                                                    (concat (plist-get label :value) " "))
                                                                                  (plist-get hint :label)
                                                                                  )))
                                                                  (t (plist-get hint :label)))))
                                         (hint-insert-text (if hint-at-end-of-line-p
                                                               (format "%s\n" hint-text)
                                                             hint-text))
                                         (overlay (make-overlay
                                                   hint-start-pos
                                                   (1+ hint-start-pos)
                                                   nil t t)))
                                    (overlay-put overlay 'display hint-insert-text)
                                    (overlay-put overlay 'face `(:foreground "#aaaaaa"))
                                    (overlay-put overlay 'read-only t)
                                    (push overlay lsp-bridge-inlay-hint-overlays)
                                    ))))

(provide 'lsp-bridge-inlay-hint)

;;; lsp-bridge-inlay-hint.el ends here
