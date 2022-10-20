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

(require 'yasnippet)

;;; Code:
(defgroup acm-backend-yas nil
  "Yasnippet backend for ACM."
  :group 'acm)

(defcustom acm-enable-yas t
  "Popup yasnippet completions when this option is turn on."
  :type 'boolean
  :group 'acm-backend-yas)

(defcustom acm-backend-yas-candidates-number 2
  "Maximal number of yas candidate of menu."
  :type 'integer
  :group 'acm-backend-yas)

(defcustom acm-backend-yas-show-trigger-keyword " (%s)"
  "Format to display yasnippet trigger keyword after snippet file name."
  :type '(choice (boolean :tag "Enable or not, t to use default value")
                 (string :tag "Literal text with %s"))
  :group 'acm-backend-yas)

(defcustom acm-backend-yas-match-by-trigger-keyword nil
  "Match yasnippet candidates by trigger keyword or not.
Setting to nil means matching uses snippet file names by default."
  :type 'boolean
  :group 'acm-backend-yas)

(defface acm-backend-yas-trigger-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for yas trigger keyword."
  :group 'acm-backend-yas)

(defun acm-backend-yas-candidates-display (name trigger)
  (let* ((default (car (get 'acm-backend-yas-show-trigger-keyword 'standard-value)))
         (k acm-backend-yas-show-trigger-keyword)
         ;; WORKAROUND backward compatibility for boolean value
         (form (or (and (booleanp k) k default) k)))
    (format (concat "%s" form) name (propertize trigger 'face 'acm-backend-yas-trigger-keyword-face))))

(defun acm-backend-yas-candidates (keyword)
  (when (and acm-enable-yas
             (not (string-empty-p keyword)))
    (let* ((candidates (list))
           (templates (yas--all-templates (yas--get-snippet-tables)))
           (match-templates (seq-filter (lambda (s)
                                          (if acm-backend-yas-match-by-trigger-keyword
                                              (string-prefix-p keyword (yas--template-key s))
                                            (acm-candidate-fuzzy-search keyword (yas--template-name s))))
                                        templates)))
      (dolist (template (cl-subseq match-templates 0 (min (length match-templates) acm-backend-yas-candidates-number)))
        (let ((name (yas--template-name template))
              (content (yas--template-content template))
              (trigger (or (yas--template-key template)
                           (and (functionp 'yas--template-regexp-key)
                                (yas--template-regexp-key template)))))
        (add-to-list 'candidates (list :key name
                                       :icon "snippet"
                                       :label name
                                       :display-label (acm-backend-yas-candidates-display name trigger)
                                       :content content
                                       :annotation "Yas-Snippet"
                                       :backend "yas") t)))
      (acm-candidate-sort-by-prefix keyword candidates))))

(defun acm-backend-yas-candidate-expand (candidate bound-start)
  (delete-region bound-start (point))
  (yas-expand-snippet (plist-get candidate :content)))

(defun acm-backend-yas-candidate-doc (candidate)
  (ignore-errors (plist-get candidate :content)))

(provide 'acm-backend-yas)

;;; acm-backend-yas.el ends here
