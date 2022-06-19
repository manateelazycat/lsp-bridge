;;; acm-backend-english.el --- English backend for acm

;; Filename: acm-backend-english.el
;; Description: English backend for acm
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-06-07 12:24:05
;; Version: 0.1
;; Last-Updated: 2022-06-07 12:24:05
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/acm-backend-english
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
;; English backend for acm
;;

;;; Installation:
;;
;; Put acm-backend-english.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'acm-backend-english)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET acm-backend-english RET
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

(defcustom acm-backend-english-min-length 3
  "Minimum length of english word."
  :type 'integer)

(defun acm-backend-english-candidates (keyword)
  (let* ((candidates (list)))
    (when (>= (length keyword) acm-backend-english-min-length)
      (dolist (candidate acm-backend-english-completions)
        (when (string-prefix-p (downcase keyword) candidate)
          (add-to-list 'candidates (list :key candidate
                                         :icon "translate"
                                         :label candidate
                                         :display-label candidate
                                         :annotation (get-text-property 0 :initials candidate)
                                         :backend "english")
                       t))))

    candidates))

(defun acm-backend-english-candidate-expand (candidate-info bound-start)
  (let* ((keyword (acm-get-input-prefix))
         (label (plist-get candidate-info :label)))
    (delete-region bound-start (point))
    (insert (acm-backend-english-convert-candidate keyword label))
    ))

(defun acm-backend-english-convert-candidate (input candidate)
  (cond ((acm-backend-english-upcase-string-p input)
         (upcase candidate))
        ((acm-backend-english-capitalize-string-p input)
         (capitalize candidate))
        (t candidate)))

(defun acm-backend-english-upcase-string-p (str)
  (let ((case-fold-search nil))
    (and (> (length str) 1)
         (string-match-p "\\`[A-Z]*\\'" str))))

(defun acm-backend-english-capitalize-string-p (str)
  (let ((case-fold-search nil))
    (string-match-p "\\`[A-Z][a-z]*\\'" str)))

(provide 'acm-backend-english)

;;; acm-backend-english.el ends here
