;;; acm-backend-citre.el --- Citre backend for acm

;; Filename: acm-backend-citre.el
;; Description: Citre backend for acm
;; Author: skfwe <wcq-062821@163.com>
;; Copyright (C) 2022, skfwe, all rights reserved.
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
;; Citre backend for acm
;;

;;; Installation:
;;
;; Put acm-backend-citre.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'acm-backend-citre)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET acm-backend-citre RET
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

(defcustom acm-enable-citre nil
  "Popup citre completions when this option is turn on."
  :type 'boolean)


(defvar-local acm-citre-search-keyword "")

(defvar-local acm-citre-prefix-keyword ""
  "when type . keyword is \"\" so we use symbol befoe . as keyword prefix")


(defun acm-citre-clear-prefix-keyword ()
  (setq-local acm-citre-prefix-keyword ""))

(defun acm-backend-citre-candidates (keyword)
  (when acm-enable-citre
    (let* ((candidates (list)))
      (if (string= keyword "")
          (progn
            (setq-local acm-citre-prefix-keyword 
                        (save-excursion
                          (backward-char)
                          (if (string= (char-to-string (following-char)) ".")
                              (thing-at-point 'symbol)
                            "")))
            (setq-local acm-citre-search-keyword acm-citre-prefix-keyword))
        (if (string-suffix-p (substring keyword 0 (1- (length keyword))) acm-citre-search-keyword)
            (setq-local acm-citre-search-keyword (concat acm-citre-search-keyword (substring (string-reverse keyword) 0 1)))
          (setq-local acm-citre-search-keyword keyword)))

      (let* ((collection (if (string= acm-citre-search-keyword "")
                             nil
                           (delete-dups (citre-capf--get-collection acm-citre-search-keyword)))))
        (when collection
          (dolist (candidate collection)
            (when (acm-candidate-fuzzy-search acm-citre-search-keyword candidate)
              (let* ((annotation (replace-regexp-in-string "[() ]" "" (replace-regexp-in-string ")?@.*" "" (citre-get-property 'annotation candidate))))
                     (candidate-fix (replace-regexp-in-string (concat "^" acm-citre-prefix-keyword "\\.?") "" candidate)))
                (add-to-list 'candidates (list :key candidate-fix
                                               :icon (downcase annotation)
                                               :label candidate-fix
                                               :display-label candidate-fix
                                               :annotation annotation 
                                               :backend "citre")
                             t))))
          (acm-candidate-sort-by-prefix acm-citre-search-keyword candidates))))))

(defun acm-backend-citre-candidate-expand (candidate-info bound-start)
  (delete-region bound-start (point))
  (insert (plist-get candidate-info :label)))

(provide 'acm-backend-citre)

;;; acm-backend-citre.el ends here
