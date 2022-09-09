;;; acm-backend-yas.el --- Yasnippet backend for acm

;; Filename: acm-backend-yas.el
;; Description: Yasnippet backend for acm
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-06-07 08:52:11
;; Version: 0.1
;; Last-Updated: 2022-06-07 08:52:11
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/acm-backend-yas
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
;; Yasnippet backend for acm
;;

;;; Installation:
;;
;; Put acm-backend-yas.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'acm-backend-yas)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET acm-backend-yas RET
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

(defcustom acm-enable-yas t
  "Popup yasnippet completions when this option is turn on."
  :type 'boolean)

(defcustom acm-backend-yas-candidates-number 2
  "Maximal number of yas candidate of menu."
  :type 'integer)

(defcustom acm-backend-yas-show-trigger-keyword t
  "Display yasnippet trigger keyword after snippet file name"
  :type 'boolean)

(defun acm-backend-yas-candidates (keyword)
  (when acm-enable-yas
    (let* ((candidates (list))
           (snippets (ignore-errors
                       (cl-remove-if (lambda (subdir) (or (member subdir '("." ".."))
                                                      (string-prefix-p "." subdir)))
                                     (directory-files (expand-file-name (prin1-to-string major-mode) (car yas/root-directory))))))
           (match-snippets (seq-filter (lambda (s) (acm-candidate-fuzzy-search keyword s)) snippets)))
      (dolist (snippet (cl-subseq match-snippets 0 (min (length match-snippets) acm-backend-yas-candidates-number)))
        (add-to-list 'candidates (list :key snippet
                                       :icon "snippet"
                                       :label snippet
                                       :display-label (if acm-backend-yas-show-trigger-keyword
                                                          (concat snippet " (" (acm-backend-yas-get-trigger-kw snippet) ")")
                                                        snippet)
                                       :annotation "Yas-Snippet"
                                       :backend "yas")
                     t))
      (acm-candidate-sort-by-prefix keyword candidates))))

(defun acm-backend-yas-candidate-expand (candidate-info bound-start)
  (delete-region bound-start (point))
  (yas-expand-snippet (acm-backend-yas-get-snippet candidate-info)))

(defun acm-backend-yas-candidate-fetch-doc (candidate)
  (acm-doc-show))

(defun acm-backend-yas-candidate-doc (candidate)
  (acm-backend-yas-get-snippet candidate))

(defun acm-backend-yas-get-snippet (candidate)
  (let ((snippet-file (expand-file-name (plist-get candidate :label)
                                        (expand-file-name (prin1-to-string major-mode) (car yas/root-directory)))))
    (with-temp-buffer
      (insert-file-contents snippet-file)
      (search-forward "# --\n" nil t)
      (buffer-substring-no-properties (point) (point-max)))))

(defun acm-backend-yas-get-trigger-kw (snippet)
  (let ((snippet-file (expand-file-name snippet (expand-file-name (prin1-to-string major-mode) (car yas/root-directory)))))
    (with-temp-buffer
      (insert-file-contents snippet-file)
      (re-search-forward "# key:\\s-*\\(.*\\)\\s-*\n" nil t)
      (match-string 1))))

(provide 'acm-backend-yas)

;;; acm-backend-yas.el ends here
