;;; acm-backend-lsp-workspace-symbol.el --- LSP backend for acm  -*- lexical-binding: t; no-byte-compile: t; -*-

;; Filename: acm-backend-lsp-workspace-symbol.el
;; Description: LSP backend for acm
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-06-07 08:56:16
;; Version: 0.1
;; Last-Updated: 2022-10-10 14:09:54 +0800
;;           By: Gong Qijian
;; URL: https://www.github.org/manateelazycat/acm-backend-lsp-workspace-symbol
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
;; LSP backend for acm
;;

;;; Installation:
;;
;; Put acm-backend-lsp-workspace-symbol.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'acm-backend-lsp-workspace-symbol)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET acm-backend-lsp-workspace-symbol RET
;;

;;; Change log:
;;
;; 2022/06/07
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

(defgroup acm-backend-lsp-workspace-symbol nil
  "LSP backend for acm."
  :group 'acm)

(defcustom acm-enable-lsp-workspace-symbol nil
  "Popup LSP workspace symbol completions when this option is turn on."
  :type 'boolean
  :group 'acm-backend-lsp-workspace-symbol)

(defcustom acm-backend-lsp-workspace-symbol-candidate-min-length 0
  "Minimal length of candidate."
  :type 'integer
  :group 'acm-backend-lsp-workspace-symbol)

(defcustom acm-backend-lsp-workspace-symbol-candidates-max-number 100
  "Maximal number of candidate of menu."
  :type 'integer
  :group 'acm-backend-lsp-workspace-symbol)

(defcustom acm-backend-lsp-workspace-symbol-frontend-filter-p nil
  "Because LSP workspace symbols has filtered at Python backend.

So don't need filter candidates again when show candidates in acm menu.

Anyway, if want use `acm-candidate-fuzzy-search' filter again in acm menu, turn on this option."
  :type 'string
  :group 'acm-backend-lsp-workspace-symbol)

(defun acm-backend-lsp-workspace-symbol-init ()
  (setq-local acm-backend-lsp-workspace-symbol-cache-candidates nil)
  (setq-local acm-backend-lsp-workspace-symbol-completion-position nil)
  (setq-local acm-backend-lsp-workspace-symbol-completion-trigger-characters nil)
  (setq-local acm-backend-lsp-workspace-symbol-server-names nil)
  (setq-local acm-backend-lsp-workspace-symbol-items (make-hash-table :test 'equal)))

(defun acm-backend-lsp-workspace-symbol-candidates (keyword)
  (let ((match-candidates
         (acm-with-cache-candidates
          acm-backend-lsp-workspace-symbol-cache-candidates
          (let* ((candidates (list)))
            (when (and
                   (>= (length keyword) acm-backend-lsp-workspace-symbol-candidate-min-length)
                   (boundp 'acm-backend-lsp-workspace-symbol-items)
                   acm-backend-lsp-workspace-symbol-items
                   (boundp 'acm-backend-lsp-workspace-symbol-server-names)
                   acm-backend-lsp-workspace-symbol-server-names
                   (hash-table-p acm-backend-lsp-workspace-symbol-items))
              (dolist (server-name acm-backend-lsp-workspace-symbol-server-names)
                (when-let* ((server-items (gethash server-name acm-backend-lsp-workspace-symbol-items)))
                  (maphash (lambda (k v)
                             (add-to-list 'candidates v t))
                           server-items))))

            ;; NOTE:
            ;; lsp-bridge has sort candidate at Python side,
            ;; please do not do secondary sorting here, elisp is very slow.
            candidates))))

    ;; Show candidates
    (cond
     ;; Don't filter candidates is prefix is empty.
     ((string-equal keyword "")
      match-candidates)
     ;; Fitler candidates when `acm-backend-lsp-workspace-symbol-frontend-filter-p' is non-nil.
     (acm-backend-lsp-workspace-symbol-frontend-filter-p
      (seq-filter (lambda (c) (acm-candidate-fuzzy-search keyword (plist-get c :label))) match-candidates))
     ;; Don't filter candidates default, because LSP workspace symbols has filtered at Python backend.
     (t
      match-candidates))))

(defun acm-backend-lsp-workspace-symbol-clean ()
  (setq-local acm-backend-lsp-workspace-symbol-items (make-hash-table :test 'equal))
  (setq-local acm-backend-lsp-workspace-symbol-cache-candidates nil))

(provide 'acm-backend-lsp-workspace-symbol)

;;; acm-backend-lsp-workspace-symbol.el ends here
