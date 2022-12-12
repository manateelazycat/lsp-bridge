;;; acm-backend-elisp.el --- Elisp backend for acm  -*- lexical-binding: t -*-

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

(defgroup acm-backend-elisp nil
  "Elisp backend for acm."
  :group 'acm)

(defcustom acm-backend-elisp-search-max-number 300
  "The maximum number of search candidates."
  :type 'integer
  :group 'acm-backend-elisp)

(defvar-local acm-backend-elisp-items nil)

(defvar acm-backend-elisp-parse-depth 100)
(defvar acm-backend-elisp-parse-limit 30)
(defvar acm-backend-elisp-var-binding-regexp
  "\\_<\\(?:cl-\\)?\\(?:def\\(?:macro\\|subst\\|un\\)\\|l\\(?:ambda\\|e\\(?:\\(?:xical-le\\)?t\\)\\)\\)\\*?\\_>")
(defvar acm-backend-elisp-var-binding-regexp-1
  "\\_<\\(?:cl-\\)?\\(?:do\\(?:list\\|times\\)\\)\\*?\\_>")

(defvar acm-backend-elisp-symbols-update-timer nil)
(defvar acm-backend-elisp-symbols-update-size 0)

(defun acm-backend-elisp-candidates (keyword)
  (let* ((candidates (list)))
    (when (and (acm-is-elisp-mode-p))
      (dolist (elisp-symbol acm-backend-elisp-items)
        (let ((symbol-type (acm-backend-elisp-symbol-type (intern elisp-symbol))))
          (add-to-list 'candidates (list :key elisp-symbol
                                         :icon symbol-type
                                         :label elisp-symbol
                                         :display-label elisp-symbol
                                         :annotation (capitalize symbol-type)
                                         :backend "elisp")
                       t))))

    candidates))

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

(defun acm-backend-elisp-global-symbols ()
  (all-completions ""
                   obarray
                   (lambda (symbol)
                     (or (fboundp symbol)
                         (boundp symbol)
                         (featurep symbol)
                         (facep symbol)))))

(defun acm-backend-elisp-local-symbols ()
  (when (acm-is-elisp-mode-p)
    (let ((regexp "[ \t\n]*\\(\\_<\\(?:\\sw\\|\\s_\\)*\\_>\\)")
          (pos (point))
          res)
      (condition-case nil
          (save-excursion
            (dotimes (_ acm-backend-elisp-parse-depth)
              (up-list -1)
              (save-excursion
                (when (eq (char-after) ?\()
                  (forward-char 1)
                  (when (ignore-errors
                          (save-excursion (forward-list)
                                          (<= (point) pos)))
                    (skip-chars-forward " \t\n")
                    (cond
                     ((looking-at acm-backend-elisp-var-binding-regexp)
                      (down-list 1)
                      (condition-case nil
                          (dotimes (_ acm-backend-elisp-parse-limit)
                            (save-excursion
                              (when (looking-at "[ \t\n]*(")
                                (down-list 1))
                              (when (looking-at regexp)
                                (cl-pushnew (match-string-no-properties 1) res)))
                            (forward-sexp))
                        (scan-error nil)))
                     ((looking-at acm-backend-elisp-var-binding-regexp-1)
                      (down-list 1)
                      (when (looking-at regexp)
                        (cl-pushnew (match-string-no-properties 1) res)))))))))
        (scan-error nil))

      res)))

(defun acm-backend-elisp-get-symbols ()
  (append (acm-backend-elisp-local-symbols)
          (acm-backend-elisp-global-symbols)))

(provide 'acm-backend-elisp)

;;; acm-backend-elisp.el ends here
