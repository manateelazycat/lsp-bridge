;;; acm-backend-search-file-words.el --- Path backend for acm  -*- lexical-binding: t -*-

;; Filename: acm-backend-search-file-words.el
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
;; Put acm-backend-search-file-words.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'acm-backend-search-file-words)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET acm-backend-search-file-words RET
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

(defgroup acm-backend-search-file-words nil
  "Backend fo completion words in other buffer."
  :group 'acm)

(defcustom acm-enable-search-file-words t
  "Popup search words completions when this option is turn on."
  :type 'boolean
  :group 'acm-backend-search-file-words)

(defvar-local acm-backend-search-file-words-items nil)

(defun acm-backend-search-file-words-candidates (keyword)
  (when acm-enable-search-file-words
    (let* ((candidates (list)))
      (dolist (candidate-label acm-backend-search-file-words-items)
        (add-to-list 'candidates (list :key candidate-label
                                       :icon "search"
                                       :label candidate-label
                                       :display-label candidate-label
                                       :annotation "Search Word"
                                       :backend "search-file-words")
                     t))

      candidates)))

(defun acm-backend-search-file-words-candidate-expand (candidate-info bound-start)
  (if (acm-is-elisp-mode-p)
      (delete-region (car (bounds-of-thing-at-point 'symbol)) (point))
    (delete-region
     (save-excursion
       (skip-syntax-backward "^ " (line-beginning-position))
       (point))
     (point)))
  (insert (plist-get candidate-info :label)))

(defun acm-backend-search-file-words-get-point-string ()
  "Get string around point."
  (if (acm-is-elisp-mode-p)
      (or (thing-at-point 'symbol t) "")
    (buffer-substring-no-properties
     (save-excursion
       (skip-syntax-backward "^ " (line-beginning-position))
       (point))
     (save-excursion
       (skip-syntax-forward "^ " (line-end-position))
       (point))
     )))

(defun acm-backend-search-file-words-clean ()
  (setq-local acm-backend-search-file-words-items nil))

(provide 'acm-backend-search-file-words)

;;; acm-backend-search-file-words.el ends here
