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
(require 'acm)
(require 'acm-backend-english-data)

;;; Code:

(defgroup acm-backend-english nil
  "English backend for acm."
  :group 'acm)

(defcustom acm-backend-english-min-length 3
  "Minimum length of english word."
  :type 'integer
  :group 'acm-backend-english)

(defcustom acm-backend-english-predicate
  #'acm-backend-english-predicate
  "Predicate of `acm-backend-english'."
  :type 'symbol
  :local t
  :group 'acm-backend-english)
(put 'acm-backend-english-predicate 'safe-local-variable 'symbolp)

(defvar-local acm-enable-english-helper nil)

(defun acm-backend-english-predicate (keyword)
  (and acm-enable-english-helper
       (>= (length keyword) acm-backend-english-min-length)))

(defun acm-backend-english-candidates (keyword)
  (let ((candidates (list)))
    (dolist (candidate acm-backend-english-completions)
      (when (string-prefix-p (downcase keyword) candidate)
        (add-to-list 'candidates (list :key candidate
                                       :icon "translate"
                                       :label candidate
                                       :display-label candidate
                                       :annotation (get-text-property 0 :initials candidate)
                                       :backend "english")
                     t)))

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

(defun acm-backend-english--predicate-enable-everywhere (keyword)
  (and acm-enable-english-helper
       (>= (length keyword) acm-backend-english-min-length)))


;; TODO
(defvar acm-backend-english-helper-dict nil)

(defvar-local acm-backend-english--original-backend nil)

(defun acm-toggle-english-helper ()
  "Toggle english helper.
Enable acm english backend only and disable other backends, or
restore original backend-group."
  (interactive)
  (unless acm-backend-english-helper-dict
    (setq acm-backend-english-helper-dict (make-hash-table :test 'equal))
    (acm--register-backend-or-subgroup 'acm-backend-english))


  (if acm-enable-english-helper
      (progn
        ;; Disable `lsp-bridge-mode' if it is enable temporality.
        (when (gethash (buffer-name) acm-backend-english-helper-dict)
          (lsp-bridge-mode -1)
          (remhash (buffer-name) acm-backend-english-helper-dict))
        (when acm-backend-english--original-backend
          (setq-local acm-general-backend-group (car acm-backend-english--original-backend))
          (setq-local acm-snippet-backend-group (cdr acm-backend-english--original-backend))
          (setq-local acm-backend-english--original-backend nil))
        (message "Turn off english helper."))
    ;; We enable `lsp-bridge-mode' temporality if current-mode not enable `lsp-bridge-mode' yet.
    (unless lsp-bridge-mode
      (puthash (buffer-name) t acm-backend-english-helper-dict)
      (lsp-bridge-mode 1))
    (setq-local acm-backend-english--original-backend (cons acm-general-backend-group acm-snippet-backend-group))
    (setq-local acm-general-backend-group '(acm-backend-english))
    (setq-local acm-snippet-backend-group nil)
    (message "Turn on english helper."))
  (setq-local acm-enable-english-helper (not acm-enable-english-helper)))

(provide 'acm-backend-english)

;;; acm-backend-english.el ends here
