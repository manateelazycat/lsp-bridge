;;; acm-backend-lsp.el --- LSP backend for acm

;; Filename: acm-backend-lsp.el
;; Description: LSP backend for acm
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-06-07 08:56:16
;; Version: 0.1
;; Last-Updated: 2022-06-07 08:56:16
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/acm-backend-lsp
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
;; LSP backend for acm
;;

;;; Installation:
;;
;; Put acm-backend-lsp.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'acm-backend-lsp)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET acm-backend-lsp RET
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

(defun acm-backend-lsp-candidates (keyword)
  (let* ((candidates (list)))
    (dolist (backend-hash-table (list acm-backend-local-items))
      (when (and backend-hash-table
                 (hash-table-p backend-hash-table))
        (dolist (backend-name (hash-table-keys backend-hash-table))
          (maphash
           (lambda (k v)
             (let ((candidate-label (plist-get v :label)))
               (when (or (string-equal keyword "")
                         (acm-candidate-fuzzy-search keyword candidate-label))
                 (if (> (length candidate-label) acm-menu-candidate-limit)
                     (plist-put v :display-label (format "%s ..." (substring candidate-label 0 acm-menu-candidate-limit)))
                   (plist-put v :display-label candidate-label))

                 (plist-put v :backend "lsp")
                 (add-to-list 'candidates v t))))
           (gethash backend-name backend-hash-table)))))

    (acm-candidate-sort-by-prefix keyword candidates)))

(defun acm-backend-lsp-candidate-expand (candidate-info bound-start)
  (let* ((label (plist-get candidate-info :label))
         (insert-text (plist-get candidate-info :insertText))
         (insert-text-format (plist-get candidate-info :insertTextFormat))
         (text-edit (plist-get candidate-info :textEdit))
         (new-text (plist-get text-edit :newText))
         (additionalTextEdits (plist-get candidate-info :additionalTextEdits))
         (kind (plist-get candidate-info :icon))
         (snippet-fn (and (or (eql insert-text-format 2) (string= kind "snippet")) (lsp-bridge--snippet-expansion-fn)))
         (completion-start-pos (lsp-bridge--lsp-position-to-point lsp-bridge-completion-position))
         (delete-start-pos (if text-edit
                               (lsp-bridge--lsp-position-to-point (plist-get (plist-get text-edit :range) :start))
                             bound-start))
         (range-end-pos (if text-edit
                            (lsp-bridge--lsp-position-to-point (plist-get (plist-get text-edit :range) :end))
                          completion-start-pos))
         (delete-end-pos (+ (point) (- range-end-pos completion-start-pos)))
         (insert-candidate (or new-text insert-text label)))

    ;; Move bound start position forward one character, if the following situation is satisfied:
    ;; 1. `textEdit' is not exist
    ;; 2. bound-start character is `lsp-bridge-completion-trigger-characters'
    ;; 3. `label' start with bound-start character
    ;; 4. `insertText' is not start with bound-start character
    (unless text-edit
      (let* ((bound-start-char (save-excursion
                                 (goto-char delete-start-pos)
                                 (char-to-string (char-after)))))
        (when (and (member bound-start-char lsp-bridge-completion-trigger-characters)
                   (string-prefix-p bound-start-char label)
                   (not (string-prefix-p bound-start-char insert-text)))
          (setq delete-start-pos (1+ delete-start-pos)))))

    ;; Delete region.
    (delete-region delete-start-pos delete-end-pos)

    ;; Insert candidate or expand snippet.
    (funcall (or snippet-fn #'insert) insert-candidate)

    ;; Do `additionalTextEdits' if return auto-imprt information.
    (when (and lsp-bridge-enable-auto-import
               (cl-plusp (length additionalTextEdits)))
      (lsp-bridge--apply-text-edits additionalTextEdits))))

(defun acm-backend-lsp-candidate-fetch-doc (candidate)
  (let* ((label (plist-get candidate :label))
         (kind (plist-get candidate :icon))
         (documentation (plist-get candidate :documentation)))

    ;; Popup candidate documentation directly if `documentation' is exist in candidate.
    (when documentation
      (setq-local lsp-bridge-completion-item-popup-doc-tick (format "%s,%s" label kind))
      (acm-doc-show))

    ;; Try send `completionItem/resolve' request to fetch `documentation' and `additionalTextEdits' information.
    (unless (equal lsp-bridge-completion-item-fetch-tick (list lsp-bridge-filepath label kind))
      (lsp-bridge-call-async "fetch_completion_item_info" lsp-bridge-filepath (format "%s,%s" label kind))
      (setq lsp-bridge-completion-item-fetch-tick (list lsp-bridge-filepath label kind)))))

(defun acm-backend-lsp-candidate-doc (candidate)
  (plist-get candidate :documentation))

(provide 'acm-backend-lsp)

;;; acm-backend-lsp.el ends here
