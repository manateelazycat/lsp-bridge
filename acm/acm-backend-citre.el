;;; acm-backend-citre.el --- Path backend for acm

;; Filename: acm-backend-citre.el
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
;; Put acm-backend-citre.el to your load-path.
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

(defcustom acm-enable-citre t
  "Popup citre completions when this option is turn on."
  :type 'boolean)

(defun acm-backend-citre-candidates (keyword)
  (when acm-enable-citre
    (let* ((candidates (list))
           (symbol (citre-get-symbol))
           (bounds (citre-get-property 'bounds symbol))
           (start (car bounds))
           (end (cdr bounds))
           (collection (delete-dups (citre-capf--get-collection symbol))))
      (message "keyword : %s" keyword)
      (message "symbol : %s" symbol)
      ;; (message "collection : %s" collection)

      (when collection
        (dolist (candidate collection)
          (message "candidate : %S" candidate)
          (when (acm-candidate-fuzzy-search keyword candidate)
            (add-to-list 'candidates (list :key candidate
                                           :icon "file"
                                           :label candidate
                                           :display-label candidate
                                           :annotation (get-text-property 0 'annotation candidate)
                                           :backend "citre")
                         t)))
        (acm-candidate-sort-by-prefix keyword candidates)))))

(defun acm-backend-citre-candidate-expand (candidate-info bound-start)
  (let* ((keyword (acm-get-input-prefix))
         (file-name (plist-get candidate-info :label))
         (parent-dir (file-name-directory keyword)))
    (delete-region bound-start (point))
    (insert (concat parent-dir file-name))))

(provide 'acm-backend-citre)

;;; acm-backend-citre.el ends here
