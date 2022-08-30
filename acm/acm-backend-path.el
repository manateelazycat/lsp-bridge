;;; acm-backend-path.el --- Path backend for acm

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

(defcustom acm-enable-path t
  "Popup path completions when this option is turn on."
  :type 'boolean)

(defun acm-backend-path-candidates (keyword)
  (when acm-enable-path
    (let* ((candidates (list))
          (parent-dir (ignore-errors (expand-file-name (file-name-directory (thing-at-point 'filename))))))
      (when (and parent-dir
                 (file-exists-p parent-dir))
        (let ((current-file (file-name-base keyword))
              (files (cl-remove-if (lambda (subdir) (or (member subdir '("." ".."))))
                                   (directory-files parent-dir))))
          (dolist (file files)
            (when (acm-candidate-fuzzy-search current-file file)
              (let* ((file-path (expand-file-name file parent-dir))
                     (file-type (if (file-directory-p file-path) "dir" "file")))
                (add-to-list 'candidates (list :key file
                                               :icon file-type
                                               :label file
                                               :display-label file
                                               :annotation (capitalize file-type)
                                               :backend "path")
                             t))))))
      (acm-candidate-sort-by-prefix keyword candidates))))

(defun acm-backend-path-candidate-expand (candidate-info bound-start)
  (let* ((keyword (acm-get-input-prefix))
         (file-name (plist-get candidate-info :label))
         (parent-dir (file-name-directory keyword)))
    (delete-region bound-start (point))
    (insert (concat parent-dir file-name))))

(provide 'acm-backend-path)

;;; acm-backend-path.el ends here
