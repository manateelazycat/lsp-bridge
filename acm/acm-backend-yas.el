;;; acm-backend-yas.el --- Yasnippet backend for acm  -*- lexical-binding: t -*-

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
  "Format for displaying extra info about yas candidaets.
If the value is nil, then format is \"filename\".
If the value is t or string $str contains one %s, then format is
\"filename trigger\" or \"trigger filename\" which is controlled by
`acm-backend-yas-match-by-trigger-keyword'."
  :type '(choice (boolean :tag "Enable or not, t to use default value")
                 (string :tag "Literal text with %s"))
  :group 'acm-backend-yas)

(defcustom acm-backend-yas-match-by-trigger-keyword nil
  "If non-nil, match yasnippet candidates by trigger keyword.
Default matching use snippet filename."
  :type 'boolean
  :group 'acm-backend-yas)

(defface acm-backend-yas-extra-info-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for yas extra information."
  :group 'acm-backend-yas)

;;; Variables

(defvar acm-backend-yas--cache nil)
(defvar acm-backend-yas--cache-modes nil)
(defvar acm-backend-yas--cache-expire-p nil)

;;; Functions

(defun acm-backend-yas--cache-expire (&rest _)
  (when acm-enable-yas
    (setq acm-backend-yas--cache-expire-p t)))

(advice-add #'yas-load-directory :after #'acm-backend-yas--cache-expire)
(advice-add #'yas-load-snippet-buffer :after #'acm-backend-yas--cache-expire)

(defun acm-backend-yas--template-cons (template)
  (let ((name (yas--template-name template))
        (trigger (yas--template-key template)))
    (if (and acm-backend-yas-show-trigger-keyword
             acm-backend-yas-match-by-trigger-keyword)
        (cons trigger name)
      (cons name trigger))))

(defun acm-backend-yas--template-sort (keyword templates)
  (when templates
    (if keyword
        (cl-sort templates
                 (lambda (a b)
                   (let* ((a-label (car (acm-backend-yas--template-cons a)))
                          (b-label (car (acm-backend-yas--template-cons b)))
                          (a-include-prefix (string-prefix-p keyword a-label))
                          (b-include-prefix (string-prefix-p keyword b-label)))
                     (cond
                      ((and b-include-prefix (not a-include-prefix))
                       nil)
                      ((and a-include-prefix (not b-include-prefix))
                       t)
                      ((and a-include-prefix b-include-prefix)
                       (string-lessp a-label b-label))
                      (t
                       (< (length a-label) (length b-label)))))))
      templates)))

(defun acm-backend-yas-candidates (keyword)
  (when (and acm-enable-yas
             (not (string-empty-p keyword)))
    (when (or acm-backend-yas--cache-expire-p
              (null acm-backend-yas--cache)
              (not (equal acm-backend-yas--cache-modes (yas--modes-to-activate))))
      (setq acm-backend-yas--cache (yas--all-templates (yas--get-snippet-tables)))
      (setq acm-backend-yas--cache-expire-p nil)
      (setq acm-backend-yas--cache-modes (yas--modes-to-activate)))
    (let* ((candidates (list))
           (show-key acm-backend-yas-show-trigger-keyword)
           ;; WORKAROUND backward compatibility for boolean value
           (form (concat "%s" (or (and (booleanp show-key) show-key " (%s)") show-key)))
           (match-templates (acm-backend-yas--template-sort
                             keyword
                             (seq-filter (lambda (s)
                                           (acm-candidate-fuzzy-search
                                            keyword
                                            (car (acm-backend-yas--template-cons s))))
                                         acm-backend-yas--cache)))
           (limit (min (length match-templates) acm-backend-yas-candidates-number)))
      (dolist (template (cl-subseq match-templates 0 limit))
        (let* ((pair (acm-backend-yas--template-cons template))
               (match (car pair))
               (display (format form
                                match
                                (propertize (cdr pair)
                                            'face
                                            'acm-backend-yas-extra-info-face)))
               (content (yas--template-content template)))
          (add-to-list 'candidates (list :key match
                                         :icon "snippet"
                                         :label match
                                         :display-label display
                                         :content content
                                         :annotation "Yas-Snippet"
                                         :backend "yas") t)))
      candidates)))

(defun acm-backend-yas-candidate-expand (candidate bound-start)
  (delete-region bound-start (point))
  (yas-expand-snippet (plist-get candidate :content)))

(defun acm-backend-yas-candidate-doc (candidate)
  (ignore-errors (plist-get candidate :content)))

(provide 'acm-backend-yas)

;;; acm-backend-yas.el ends here
