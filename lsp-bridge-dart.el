;;; lsp-bridge-dart.el --- Dart protocol   -*- lexical-binding: t; no-byte-compile: t; -*-*-

;; Filename: lsp-bridge-dart.el
;; Description: Dart protocol
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2023, Andy Stewart, all rights reserved.
;; Created: 2023-10-03 21:35:08
;; Version: 0.1
;; Last-Updated: 2023-10-03 21:35:08
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/lsp-bridge-dart
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
;; Dart protocol
;;

;;; Installation:
;;
;; Put lsp-bridge-dart.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lsp-bridge-dart)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET lsp-bridge-dart RET
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

(defface lsp-bridge-dart-closing-label-face
  `((t :foreground "#aaaaaa"))
  "Face for dart closing tab."
  :group 'lsp-bridge-dart)

(defvar-local lsp-bridge-dart-closing-label-overlays '())

(defun lsp-bridge-dart-hide-closing-label-overlays ()
  (when lsp-bridge-dart-closing-label-overlays
    (dolist (dart-overlay lsp-bridge-dart-closing-label-overlays)
      (delete-overlay dart-overlay)))

  (setq-local lsp-bridge-dart-closing-label-overlays nil))

(defvar lsp-bridge-dart-closing-labels-lock (make-mutex "lsp-bridge-dart-closing-labels-lock"))

(defun lsp-bridge-dart-closing-labels--render (filepath filehost closing-labels)
  ;; lsp-bridge is too fast, use locks to avoid interface disorder issues caused by multi-threaded rendering of overlays
  (with-mutex lsp-bridge-dart-closing-labels-lock
    (lsp-bridge--with-file-buffer
        filepath filehost
        ;; Hide previous overlays first.
        (lsp-bridge-dart-hide-closing-label-overlays)

        ;; Render new overlays.
        (save-excursion
          (save-restriction
            (dolist (closing-label closing-labels)
              (let* ((label (plist-get closing-label :label))
                     (range (plist-get closing-label :range))
                     (end (plist-get range :end)))
                (goto-char (acm-backend-lsp-position-to-point end))

                ;; Looks dart's closing label always at end of line.
                (end-of-line)

                (let* ((overlay (make-overlay (point) (1+ (point)) nil t)))
                  (overlay-put overlay 'before-string (propertize (concat " " label) 'face 'lsp-bridge-dart-closing-label-face))
                  (overlay-put overlay 'evaporate t) ; NOTE, `evaporate' is import
                  (push overlay lsp-bridge-dart-closing-label-overlays)
                  ))))))))

(provide 'lsp-bridge-dart)

;;; lsp-bridge-dart.el ends here
