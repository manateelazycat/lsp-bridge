;;; lsp-bridge-ui-history.el --- Sorting by history for Lsp-Bridge-Ui -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (lsp-bridge-ui "0.24"))
;; Homepage: https://github.com/minad/lsp-bridge-ui

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Enable `lsp-bridge-ui-history-mode' to sort candidates by their history
;; position. Maintain a list of recently selected candidates. In order
;; to save the history across Emacs sessions, enable `savehist-mode' and
;; add `lsp-bridge-ui-history' to `savehist-additional-variables'.
;;
;; (lsp-bridge-ui-history-mode 1)
;; (savehist-mode 1)
;; (add-to-list 'savehist-additional-variables 'lsp-bridge-ui-history)

;;; Code:

(require 'lsp-bridge-ui)
(eval-when-compile
  (require 'cl-lib))

(defcustom lsp-bridge-ui-history-length nil
  "Lsp-Bridge-Ui history length."
  :type '(choice (const nil) integer)
  :group 'lsp-bridge-ui)

(defvar lsp-bridge-ui-history--hash nil
  "Hash table of Lsp-Bridge-Ui candidates.")

(defvar lsp-bridge-ui-history nil
  "History of Lsp-Bridge-Ui candidates.")

(defun lsp-bridge-ui-history--sort-predicate (x y)
  "Sorting predicate which compares X and Y."
  (pcase-let ((`(,sx . ,hx) x)
              (`(,sy . ,hy) y))
    (or (< hx hy)
      (and (= hx hy)
           (or (< (length sx) (length sy))
               (and (= (length sx) (length sy))
                    (string< sx sy)))))))

(defun lsp-bridge-ui-history--sort (candidates)
  "Sort CANDIDATES by history."
  (unless lsp-bridge-ui-history--hash
    (setq lsp-bridge-ui-history--hash (make-hash-table :test #'equal :size (length lsp-bridge-ui-history)))
    (cl-loop for elem in lsp-bridge-ui-history for index from 0 do
             (unless (gethash elem lsp-bridge-ui-history--hash)
               (puthash elem index lsp-bridge-ui-history--hash))))
  ;; Decorate each candidate with (index<<13) + length. This way we sort first by index and then by
  ;; length. We assume that the candidates are shorter than 2**13 characters and that the history is
  ;; shorter than 2**16 entries.
  (cl-loop for cand on candidates do
           (setcar cand (cons (car cand)
                              (+ (lsh (gethash (car cand) lsp-bridge-ui-history--hash #xFFFF) 13)
                                 (length (car cand))))))
  (setq candidates (sort candidates #'lsp-bridge-ui-history--sort-predicate))
  (cl-loop for cand on candidates do (setcar cand (caar cand)))
  candidates)

(defun lsp-bridge-ui-history--insert (&rest _)
  "Advice for `lsp-bridge-ui--insert'."
  (when (>= lsp-bridge-ui--index 0)
    (add-to-history 'lsp-bridge-ui-history
                    (nth lsp-bridge-ui--index lsp-bridge-ui--candidates)
                    lsp-bridge-ui-history-length)
    (setq lsp-bridge-ui-history--hash nil)))

;;;###autoload
(define-minor-mode lsp-bridge-ui-history-mode
  "Update Lsp-Bridge-Ui history and sort completions by history."
  :global t
  :group 'lsp-bridge-ui
  (cond
   (lsp-bridge-ui-history-mode
    (setq lsp-bridge-ui-sort-function #'lsp-bridge-ui-history--sort)
    (advice-add #'lsp-bridge-ui--insert :before #'lsp-bridge-ui-history--insert))
   (t
    (setq lsp-bridge-ui-sort-function #'lsp-bridge-ui-sort-length-alpha)
    (advice-remove #'lsp-bridge-ui--insert #'lsp-bridge-ui-history--insert))))

(provide 'lsp-bridge-ui-history)
;;; lsp-bridge-ui-history.el ends here
