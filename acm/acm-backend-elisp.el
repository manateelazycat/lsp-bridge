;;; acm-backend-elisp.el --- Elisp backend for acm

;; Filename: acm-backend-elisp.el
;; Description: Elisp backend for acm
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-06-07 08:55:28
;; Version: 0.1
;; Last-Updated: 2022-06-07 08:55:28
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/acm-backend-elisp
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
;; Elisp backend for acm
;;

;;; Installation:
;;
;; Put acm-backend-elisp.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'acm-backend-elisp)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET acm-backend-elisp RET
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

(defcustom acm-backend-elisp-min-length 2
  "Minimum length of elisp symbol."
  :type 'integer)

(defun acm-backend-elisp-sort-predicate (x y)
  "Sorting predicate which compares X and Y."
  (or (< (length x) (length y))
      (and (= (length x) (length y))
           (string< x y))))

(defun acm-backend-elisp-candidates (keyword)
  (let* ((candidates (list)))
    (when (and (or (derived-mode-p 'emacs-lisp-mode)
                   (derived-mode-p 'inferior-emacs-lisp-mode)
                   (derived-mode-p 'lisp-interaction-mode))
               (>= (length keyword) acm-backend-elisp-min-length))
      (let ((elisp-symbols (sort (all-completions keyword obarray) 
                                 'acm-backend-elisp-sort-predicate)))
        (dolist (elisp-symbol (cl-subseq elisp-symbols 0 (min (length elisp-symbols) 10)))
          (let ((symbol-type (acm-backend-elisp-symbol-type (intern elisp-symbol))))
            (add-to-list 'candidates (list :key elisp-symbol
                                           :icon symbol-type
                                           :label elisp-symbol
                                           :display-label elisp-symbol
                                           :annotation (capitalize symbol-type)
                                           :backend "elisp")
                         t)))))

    candidates))

(defun acm-backend-elisp-candidate-fetch-doc (candidate)
  (acm-doc-show))

(defun acm-backend-elisp-candidate-doc (candidate)
  (let* ((symbol (intern (plist-get candidate :label)))
         (doc (ignore-errors (documentation symbol))))
    (cond (doc
           doc)
          ((facep symbol)
           (documentation-property symbol 'face-documentation))
          (t
           (documentation-property symbol 'variable-documentation)))))  

(defun acm-backend-elisp-symbol-type (symbol)
  (cond ((featurep symbol)
         "feature")
        ((special-form-p symbol)
         "special form")
        ((functionp symbol)
         "function")
        ((macrop symbol)
         "macro")
        ((facep symbol)
         "face")
        ((custom-variable-p symbol)
         "custom")
        ((get symbol 'risky-local-variable)
         "constant")
        (t
         "variable")))

(provide 'acm-backend-elisp)

;;; acm-backend-elisp.el ends here
