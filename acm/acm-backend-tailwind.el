;;; acm-backend-tailwind.el --- Tailwind backend for acm  -*- lexical-binding: t -*-

;; Filename: acm-backend-tailwind.el
;; Description: Tailwind backend for acm
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-06-07 08:55:28
;; Version: 0.1
;; Last-Updated: 2022-06-07 08:55:28
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/acm-backend-tailwind
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
;; Tailwind backend for acm
;;

;;; Installation:
;;
;; Put acm-backend-tailwind.el to your load-path.
;; The load-path is usually ~/tailwind/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/tailwind"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'acm-backend-tailwind)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET acm-backend-tailwind RET
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

(defgroup acm-backend-tailwind nil
  "Tailwind backend for acm."
  :group 'acm)

(defvar-local acm-backend-tailwind-items nil)

(defun acm-backend-tailwind-candidates (keyword)
  (let* ((candidates (list)))
    (dolist (tailwind-symbol acm-backend-tailwind-items)
      (add-to-list 'candidates (list :key tailwind-symbol
                                     :icon "tailwind"
                                     :label tailwind-symbol
                                     :display-label tailwind-symbol
                                     :annotation "Tailwind"
                                     :backend "tailwind")
                   t))

    candidates))

(provide 'acm-backend-tailwind)

;;; acm-backend-tailwind.el ends here
