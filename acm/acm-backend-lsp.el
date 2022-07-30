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

(defcustom acm-backend-lsp-candidate-max-length 30
  "Maximal length of candidate."
  :type 'integer)

(defcustom acm-backend-lsp-candidates-max-number 1000
  "Maximal number of candidate of menu."
  :type 'integer)

(defcustom acm-backend-lsp-enable-auto-import t
  "Whether to enable auto-import."
  :type 'boolean)

(defvar acm-backend-lsp-fetch-completion-item-func nil)

(defun acm-backend-lsp-candidates (keyword)
  (let* ((candidates (list)))
    (when (and
           (boundp 'acm-backend-lsp-items)
           acm-backend-lsp-items
           (boundp 'acm-backend-lsp-server-names)
           acm-backend-lsp-server-names
           (hash-table-p acm-backend-lsp-items))
      ;; Sort multi-server items by 
      (dolist (server-name acm-backend-lsp-server-names)
        (when-let* ((server-items (gethash server-name acm-backend-lsp-items)))
          (maphash (lambda (k v)
                     (let ((candidate-label (plist-get v :label)))
                       (when (or (string-equal keyword "")
                                 (acm-candidate-fuzzy-search keyword candidate-label))
                         (if (> (length candidate-label) acm-backend-lsp-candidate-max-length)
                             (plist-put v :display-label (format "%s ..." (substring candidate-label 0 acm-backend-lsp-candidate-max-length)))
                           (plist-put v :display-label candidate-label))

                         (plist-put v :backend "lsp")
                         (add-to-list 'candidates v t))))
                   server-items))))

    (acm-candidate-sort-by-prefix keyword candidates)))

(defun acm-backend-lsp-candidate-expand (candidate-info bound-start)
  (let* ((label (plist-get candidate-info :label))
         (insert-text (plist-get candidate-info :insertText))
         (insert-text-format (plist-get candidate-info :insertTextFormat))
         (text-edit (plist-get candidate-info :textEdit))
         (new-text (plist-get text-edit :newText))
         (additionalTextEdits (plist-get candidate-info :additionalTextEdits))
         (kind (plist-get candidate-info :icon))
         (snippet-fn (and (or (eql insert-text-format 2) (string= kind "snippet")) (acm-backend-lsp-snippet-expansion-fn)))
         (completion-start-pos (acm-backend-lsp-position-to-point acm-backend-lsp-completion-position))
         (delete-start-pos (if text-edit
                               ;; Use smaller one between `bound-start' and `range-start' used as the starting point of delete.
                               (min (acm-backend-lsp-position-to-point (plist-get (plist-get text-edit :range) :start)) bound-start)
                             bound-start))
         (range-end-pos (if text-edit
                            (acm-backend-lsp-position-to-point (plist-get (plist-get text-edit :range) :end))
                          completion-start-pos))
         (delete-end-pos (+ (point) (- range-end-pos completion-start-pos)))
         (insert-candidate (or new-text insert-text label)))

    ;; Move bound start position forward one character, if the following situation is satisfied:
    ;; 1. `textEdit' is not exist
    ;; 2. `char-before-input' is `acm-backend-lsp-completion-trigger-characters'
    ;; 3. `label' start with `char-before-input'
    ;; 4. `insertText' is not start with `char-before-input'
    (unless text-edit
      (let* ((char-before-input (save-excursion
                                  (goto-char (1+ delete-start-pos))
                                  (acm-char-before))))
        (when (and (member char-before-input acm-backend-lsp-completion-trigger-characters)
                   (string-prefix-p char-before-input label)
                   (not (string-prefix-p char-before-input insert-text)))
          (setq delete-start-pos (1+ delete-start-pos)))))

    ;; Delete region.
    (delete-region delete-start-pos delete-end-pos)

    ;; Insert candidate or expand snippet.
    (funcall (or snippet-fn #'insert) insert-candidate)

    ;; Do `additionalTextEdits' if return auto-imprt information.
    (when (and acm-backend-lsp-enable-auto-import
               (cl-plusp (length additionalTextEdits)))
      (acm-backend-lsp-apply-text-edits additionalTextEdits))))

(defun acm-backend-lsp-candidate-fetch-doc (candidate)
  (let* ((key (plist-get candidate :key))
         (documentation (plist-get candidate :documentation)))

    ;; Popup candidate documentation directly if `documentation' is exist in candidate.
    (when documentation
      (acm-doc-show))

    ;; Call fetch documentation function.
    (when acm-backend-lsp-fetch-completion-item-func
      (funcall acm-backend-lsp-fetch-completion-item-func candidate))))

(defun acm-backend-lsp-candidate-doc (candidate)
  (plist-get candidate :documentation))

(defun acm-backend-lsp-position-to-point (pos-plist &optional marker)
  "Convert LSP position POS-PLIST to Emacs point.
If optional MARKER, return a marker instead"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (min most-positive-fixnum
                         (plist-get pos-plist :line)))
      (unless (eobp) ;; if line was excessive leave point at eob
        (let ((tab-width 1)
              (character (plist-get pos-plist :character)))
          (unless (wholenump character)
            (message
             "[LSP Bridge] Caution: LSP server sent invalid character position %s. Using 0 instead."
             character)
            (setq character 0))
          ;; We cannot use `move-to-column' here, because it moves to *visual*
          ;; columns, which can be different from LSP columns in case of
          ;; `whitespace-mode', `prettify-symbols-mode', etc.
          (goto-char (min (+ (line-beginning-position) character)
                          (line-end-position)))))
      (if marker (copy-marker (point-marker)) (point)))))

(defun acm-backend-lsp-apply-text-edits (edits)
  (dolist (edit (acm-backend-lsp-sort-edits edits))
    (let* ((range (plist-get edit :range)))
      (acm-backend-lsp-insert-new-text (plist-get range :start) (plist-get range :end) (plist-get edit :newText)))))

(defun acm-backend-lsp-sort-edits (edits)
  (sort edits #'(lambda (edit-a edit-b)
                  (> (acm-backend-lsp-position-to-point (plist-get (plist-get edit-a :range) :start))
                     (acm-backend-lsp-position-to-point (plist-get (plist-get edit-b :range) :start))))))

(defun acm-backend-lsp-snippet-expansion-fn ()
  "Compute a function to expand snippets.
Doubles as an indicator of snippet support."
  (and (boundp 'yas-minor-mode)
       (symbol-value 'yas-minor-mode)
       'yas-expand-snippet))

(defun acm-backend-lsp-insert-new-text (start-pos end-pos new-text)
  (let* ((start-point (acm-backend-lsp-position-to-point start-pos))
         (end-point (acm-backend-lsp-position-to-point end-pos)))
    (save-excursion
      (delete-region start-point end-point)
      (goto-char start-point)
      (insert new-text))))

(provide 'acm-backend-lsp)

;;; acm-backend-lsp.el ends here
