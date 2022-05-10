;;; lsp-bridge-orderless.el --- LSP bridge  -*- lexical-binding: t; -*-

;; Filename: lsp-bridge-orderless.el
;; Description: LSP bridge
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.5
;; Last-Updated: Sun Nov 21 04:35:02 2022 (-0500)
;;           By: Mingde (Matthew) Zeng
;; URL: https://github.com/manateelazycat/lsp-bridge
;; Keywords:
;; Compatibility: emacs-version >= 27
;;
;; Features that might be required by this library:
;;
;; Please check README
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
;; Lsp-Bridge
;;

;;; Installation:
;;
;; Please check README
;;

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET lsp-bridge RET
;;

;;; Change log:
;;
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Code:
(require 'orderless)

(defvar +orderless-dispatch-alist
  '((?% . char-fold-to-regexp)
    (?! . orderless-without-literal)
    (?`. orderless-initialism)
    (?= . orderless-literal)
    (?~ . orderless-flex)))

(defun +orderless-dispatch (pattern index _total)
  (cond
   ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
   ((string-suffix-p "$" pattern)
    `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
   ;; File extensions
   ((and
     ;; Completing filename or eshell
     (or minibuffer-completing-file-name
         (derived-mode-p 'eshell-mode))
     ;; File extension
     (string-match-p "\\`\\.." pattern))
    `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
   ;; Ignore single !
   ((string= "!" pattern) `(orderless-literal . ""))
   ;; Prefix and suffix
   ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
        (cons (cdr x) (substring pattern 1))
      (when-let (x (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist))
        (cons (cdr x) (substring pattern 0 -1)))))))

;; Define orderless style with initialism by default
(orderless-define-completion-style +orderless-with-initialism
  (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

(defun lsp-bridge-orderless-dispatch-flex-first (_pattern index _total)
  "orderless-flex for corfu."
  (and (eq index 0) 'orderless-flex))

(defun lsp-bridge-orderless-setup ()
  (setq-local completion-styles '(orderless partial-completion)
              completion-category-defaults nil
              completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
                                              ;; enable initialism by default for symbols
                                              (command (styles +orderless-with-initialism))
                                              (variable (styles +orderless-with-initialism))
                                              (symbol (styles +orderless-with-initialism)))
              orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
              orderless-style-dispatchers '(+orderless-dispatch)
              orderless-matching-styles '(orderless-flex)
              orderless-style-dispatchers nil)
  (add-hook 'orderless-style-dispatchers #'lsp-bridge-orderless-dispatch-flex-first nil t))

(provide 'lsp-bridge)

;;; lsp-bridge-orderless.el ends here
