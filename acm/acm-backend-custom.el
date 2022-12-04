;;; acm-backend-custom.el --- Custom backend for acm  -*- lexical-binding: t -*-

;; Filename: acm-backend-custom.el
;; Description: Custom backend for acm
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-12-05 07:23:38
;; Version: 0.1
;; URL: https://github.com/manateelazycat/lsp-bridge/tree/master/acm
;; Keywords: lisp
;; Compatibility: GNU Emacs 28.1

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
;; Run custom backend functions that provides completion suggestions to ACM.
;; Heavy backend functions risk slowing down the whole ACM so add them at your own risk.
;;

;;; Customize:
;;
;; Add a backend that works for all buffers like follows:
;;
;;     (add-hook 'acm-backend-custom-functions #'your-custom-backend)
;;
;; You can add buffer-local backends from hooks.
;;
;;     (defun init-php-mode ()
;;         (add-hook 'acm-backend-custom-functions #'your-php-custom-backend nil t))
;;     (with-eval-after-load 'php-mode
;;       (add-hook 'php-mode-hook #'init-php-mode))
;;

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(defgroup acm-backend-custom nil
  "LSP backend for user customize."
  :group 'acm)

(defcustom acm-backend-custom-functions nil
  "A list of custom backend functions for ACM."
  :type '(repeat function)
  :safe (lambda (v) (and (listp v) (null (memq nil (mapcar #'functionp v)))))
  :group 'acm)

(defun acm-backend-custom-candidates (keyword)
  "Return candidates called by passing KEYWORD to custom functions."
  (cl-loop for f in acm-backend-custom-functions
           nconc (funcall f keyword)))

(provide 'acm-backend-custom)
;;; acm-backend-custom.el ends here
