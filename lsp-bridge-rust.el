;;; lsp-bridge-rust.el --- Rust protocol for lsp-bridge   -*- lexical-binding: t; -*-

;; Filename: lsp-bridge-rust.el
;; Description: Rust protocol for lsp-bridge
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2024, Andy Stewart, all rights reserved.
;; Created: 2024-08-17 23:47:41
;; Version: 0.1
;; Last-Updated: 2024-08-17 23:47:41
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/lsp-bridge-rust
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50
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
;; Rust protocol for lsp-bridge
;;

;;; Installation:
;;
;; Put lsp-bridge-rust.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lsp-bridge-rust)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET lsp-bridge-rust RET
;;

;;; Change log:
;;
;; 2024/08/17
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

(defun lsp-bridge-rust-expand-macro--update (name expansion)
  ;; The name is macro name, not project name, you maybe want to replace it
  (let ((buf (get-buffer-create (format "*rust macro expansion %s*" name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert expansion)
        ;; if you use `rust-mode', you should replace fuction
        (rust-ts-mode)))
    (switch-to-buffer-other-window buf)))

(defun lsp-bridge-rust-expand-macro ()
  (interactive)
  (lsp-bridge-call-file-api "rust_expand_macro" (lsp-bridge--position)))

(defun lsp-bridge-rust-reload-workspace ()
  (interactive)
  (lsp-bridge-call-file-api "rust_reload_workspace"))

(provide 'lsp-bridge-rust)

;;; lsp-bridge-rust.el ends here
