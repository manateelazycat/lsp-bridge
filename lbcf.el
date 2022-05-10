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
(defvar-local company-selection 0)
(defvar-local company-prefix "")
(defvar-local company-candidates nil)
(defvar-local company-candidates-length 0)
(defvar-local company-common "")
(defvar-local company-point 0)

(defun lbcf-company-call-backend (old-func &rest args)
  ;; (message "lbcf-company-call-backend: %S" args))
  (if (boundp 'lsp-bridge-flag)
      nil ;; TODO annotation / doc
    (apply old-func args)))

(defun lbcf-company-fill-properties (old-func x &rest args)
  (if (boundp 'lsp-bridge-flag)
      x
    (apply old-func x args)))

(defun lbcf-initalize ()
  (advice-add 'company-call-backend :around #'lbcf-company-call-backend)
  (advice-add 'company-fill-properties :around #'lbcf-company-fill-properties))

(defun lbcf-show (candidates prefix common)
  (setq company-prefix prefix)
  (setq company-candidates candidates)
  (setq company-common common)
  (setq company-candidates-length (length candidates))
  (setq company-selection 0)
  (setq company-point (point))
  (company-box-frontend 'show))

(defun lbcf-select-next ()
  (interactive)
  (setq company-selection
        (mod (+ company-selection 1)
              company-candidates-length))
  (company-box-frontend 'update))

(defun lbcf-select-prev ()
  (interactive)
  (setq company-selection
        (mod (+ company-selection -1 company-candidates-length)
             company-candidates-length))
  (company-box-frontend 'update))

(defun lbcf-select-first ()
  (interactive)
  (setq company-selection 0)
  (company-box-frontend 'update))

(defun lbcf-select-last ()
  (interactive)
  (setq company-selection (1- company-candidates-length))
  (company-box-frontend 'update))

(defun lbcf-hide ()
  (company-box-frontend 'hide))

(defun lbcf-get-select-item ()
  (nth company-selection company-candidates))

(provide 'lbcf)

;;; lbcf.el ends here
