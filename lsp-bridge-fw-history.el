;;; lsp-bridge-fw-history.el --- Sorting by history for Lsp-Bridge-Fw -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (lsp-bridge-fw "0.24"))
;; Homepage: https://github.com/minad/lsp-bridge-fw

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

;; Enable `lsp-bridge-fw-history-mode' to sort candidates by their history
;; position. Maintain a list of recently selected candidates. In order
;; to save the history across Emacs sessions, enable `savehist-mode' and
;; add `lsp-bridge-fw-history' to `savehist-additional-variables'.
;;
;; (lsp-bridge-fw-history-mode 1)
;; (savehist-mode 1)
;; (add-to-list 'savehist-additional-variables 'lsp-bridge-fw-history)

;;; Code:

(require 'lsp-bridge-fw)
(eval-when-compile
  (require 'cl-lib))

(defcustom lsp-bridge-fw-history-length nil
  "Lsp-Bridge-Fw history length."
  :type '(choice (const nil) integer)
  :group 'lsp-bridge-fw)

(defvar lsp-bridge-fw-history--hash nil
  "Hash table of Lsp-Bridge-Fw candidates.")

(defvar lsp-bridge-fw-history nil
  "History of Lsp-Bridge-Fw candidates.")

(defun lsp-bridge-fw-history--sort-predicate (x y)
  "Sorting predicate which compares X and Y."
  (pcase-let ((`(,sx . ,hx) x)
              (`(,sy . ,hy) y))
    (or (< hx hy)
      (and (= hx hy)
           (or (< (length sx) (length sy))
               (and (= (length sx) (length sy))
                    (string< sx sy)))))))

(defun lsp-bridge-fw-history--sort (candidates)
  "Sort CANDIDATES by history."
  (unless lsp-bridge-fw-history--hash
    (setq lsp-bridge-fw-history--hash (make-hash-table :test #'equal :size (length lsp-bridge-fw-history)))
    (cl-loop for elem in lsp-bridge-fw-history for index from 0 do
             (unless (gethash elem lsp-bridge-fw-history--hash)
               (puthash elem index lsp-bridge-fw-history--hash))))
  ;; Decorate each candidate with (index<<13) + length. This way we sort first by index and then by
  ;; length. We assume that the candidates are shorter than 2**13 characters and that the history is
  ;; shorter than 2**16 entries.
  (cl-loop for cand on candidates do
           (setcar cand (cons (car cand)
                              (+ (lsh (gethash (car cand) lsp-bridge-fw-history--hash #xFFFF) 13)
                                 (length (car cand))))))
  (setq candidates (sort candidates #'lsp-bridge-fw-history--sort-predicate))
  (cl-loop for cand on candidates do (setcar cand (caar cand)))
  candidates)

(defun lsp-bridge-fw-history--insert (&rest _)
  "Advice for `lsp-bridge-fw--insert'."
  (when (>= lsp-bridge-fw--index 0)
    (add-to-history 'lsp-bridge-fw-history
                    (nth lsp-bridge-fw--index lsp-bridge-fw--candidates)
                    lsp-bridge-fw-history-length)
    (setq lsp-bridge-fw-history--hash nil)))

;;;###autoload
(define-minor-mode lsp-bridge-fw-history-mode
  "Update Lsp-Bridge-Fw history and sort completions by history."
  :global t
  :group 'lsp-bridge-fw
  (cond
   (lsp-bridge-fw-history-mode
    (setq lsp-bridge-fw-sort-function #'lsp-bridge-fw-history--sort)
    (advice-add #'lsp-bridge-fw--insert :before #'lsp-bridge-fw-history--insert))
   (t
    (setq lsp-bridge-fw-sort-function #'lsp-bridge-fw-sort-length-alpha)
    (advice-remove #'lsp-bridge-fw--insert #'lsp-bridge-fw-history--insert))))

(provide 'lsp-bridge-fw-history)
;;; lsp-bridge-fw-history.el ends here
