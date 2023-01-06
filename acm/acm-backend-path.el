;;; acm-backend-path.el --- Path backend for acm  -*- lexical-binding: t -*-

;; Filename: acm-backend-path.el
;; Description: Path backend for acm
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-06-07 08:58:47
;; Version: 0.1
;; Last-Updated: 2022-06-07 08:58:47
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/acm-backend-path
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
;; Path backend for acm
;;

;;; Installation:
;;
;; Put acm-backend-path.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'acm-backend-path)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET acm-backend-path RET
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

(defgroup acm-backend-path nil
  "Path backend for acm."
  :group 'acm)

(defcustom acm-enable-path t
  "Popup path completions when this option is turn on."
  :type 'boolean
  :group 'acm-backend-path)

(defvar-local acm-backend-path-items nil)

(defun acm-backend-path-candidates (keyword)
  (when acm-enable-path
    acm-backend-path-items))

(defun acm-backend-path-candidate-expand (candidate-info bound-start)
  (let* ((keyword (acm-get-input-prefix))
         (file-name (plist-get candidate-info :label))
         (parent-dir (file-name-directory keyword)))

    ;; Avoid insert duplicate `.' for file that have LSP server, such as python, golang, rust etc.
    (when (and (string-equal (char-to-string (char-before bound-start)) ".")
               (string-equal (substring file-name 0 1) "."))
      (setq bound-start (1- bound-start)))

    (delete-region bound-start (point))
    (insert (concat parent-dir file-name))))

(provide 'acm-backend-path)

;;; acm-backend-path.el ends here
