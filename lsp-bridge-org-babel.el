;;; lsp-bridge-org-babel.el --- lsp-bridge for Org Babel -*- lexical-binding: t; no-byte-compile: t; -*-*-
;;
;; Copyright (C) 2023 royokong
;;
;; Author: royokong <j614023177@icloud.com>
;; Maintainer: royokong <j614023177@icloud.com>
;; Created: 三月 27, 2023
;; Modified: 三月 27, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/royokong/lsp-bridge-org-babel
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defcustom lsp-bridge-enable-org-babel nil
  "Use `lsp-bridge' in org-babel, default is disable.")

(defcustom lsp-bridge-org-babel-lang-list nil
  "A list of org babel languages like (\"python\" \"bash\"), which enable lsp-bridge. if nil means enable all languages."
  :type '(repeat string))

;; org babel cache
(defvar-local lsp-bridge-org-babel--info-cache nil)
(defvar-local lsp-bridge-org-babel--block-bop nil)
(defvar-local lsp-bridge-org-babel--block-eop nil)
(defvar-local lsp-bridge-org-babel--update-file-before-change nil)

(defun lsp-bridge-org-babel-in-block-p (pos)
  "Check if POS is in org babel block."
  (and lsp-bridge-org-babel--block-bop
       lsp-bridge-org-babel--block-eop
       (>= pos lsp-bridge-org-babel--block-bop)
       (<= pos lsp-bridge-org-babel--block-eop)))

(defun lsp-bridge-org-babel-clean-cache ()
  "Clean org babel cache."
  (setq-local lsp-bridge-org-babel--info-cache nil)
  (setq-local lsp-bridge-org-babel--block-bop nil)
  (setq-local lsp-bridge-org-babel--block-eop nil))

(defun lsp-bridge-org-babel-check-lsp-server ()
  "Check if current point is in org babel block. "
  (if (and lsp-bridge-org-babel--info-cache (lsp-bridge-org-babel-in-block-p (point)))
      lsp-bridge-org-babel--info-cache
    (setq-local lsp-bridge-org-babel--info-cache (org-element-context))
    ;; TODO support latex block like `latex-environment' nad `latex-block'
    (if (not (eq (org-element-type lsp-bridge-org-babel--info-cache) 'src-block))
        (setq-local lsp-bridge-org-babel--info-cache nil)
      (save-excursion
        (goto-char (org-element-property :post-affiliated lsp-bridge-org-babel--info-cache))
        (setq-local lsp-bridge-org-babel--block-bop (1+ (point-at-eol))))
      (setq-local lsp-bridge-org-babel--block-eop (+ lsp-bridge-org-babel--block-bop -1
                                                     (length (org-element-property :value lsp-bridge-org-babel--info-cache))))
      ;; sync it in `lsp-bridge-monitor-before-change'
      (setq-local lsp-bridge-org-babel--update-file-before-change t)))

  (and lsp-bridge-org-babel--info-cache
       (lsp-bridge-org-babel-get-single-lang-server)))

(defun lsp-bridge-org-babel-get-single-lang-server ()
  "Get single lang server for org block."
  (let* ((lang (org-element-property :language lsp-bridge-org-babel--info-cache)) ;get language name in src block
         (org-src-lang (cdr (assoc lang org-src-lang-modes))) ;find match language name from `org-src-lang-modes'
         (lang-name (if org-src-lang
                        (symbol-name org-src-lang) ;convert to symbol name if found in `org-src-lang-modes'
                      (format "%s" lang))) ;otherwise use src block name
         (mode-name (concat lang-name "-mode"))
         (major-mode (intern mode-name))
         (langserver-info
          (lsp-bridge-lang-server-by-mode major-mode lsp-bridge-single-lang-server-mode-list)))
    (setq-local acm-is-elisp-mode-in-org (eq major-mode 'emacs-lisp-mode))
    ;; if `lsp-bridge-org-babel-lang-list' is set, only enable lsp when lang in it
    (when (and langserver-info
               (lsp-bridge-org-babel-in-block-p (point))
               (or (eq lsp-bridge-org-babel-lang-list nil)
                   (member lang-name lsp-bridge-org-babel-lang-list)))
      (lsp-bridge-get-symbol-string-value (cdr langserver-info)))))

(defun lsp-bridge-org-babel-monitor-after-change (begin end length)
  "Monitor org babel after change, BEGIN END LENGTH."
  ;; estimate org block end point according change length
  (when (and lsp-bridge-enable-org-babel (eq major-mode 'org-mode)
             lsp-bridge-org-babel--block-bop lsp-bridge-org-babel--block-eop)
    (setq-local lsp-bridge-org-babel--block-eop
                (- lsp-bridge-org-babel--block-eop length (- begin end)))
    ;; end_src or begin_src has been changed, reload block
    (when (or (not (lsp-bridge-org-babel-in-block-p begin))
              (<= lsp-bridge-org-babel--block-eop lsp-bridge-org-babel--block-bop))
      (lsp-bridge-org-babel-clean-cache))))

(defun lsp-bridge-org-babel-send-src-block-to-lsp-server ()
  (when (and lsp-bridge-enable-org-babel (eq major-mode 'org-mode)
             lsp-bridge-org-babel--block-bop
             lsp-bridge-org-babel--update-file-before-change)
    (setq-local lsp-bridge-org-babel--update-file-before-change nil)
    (lsp-bridge-call-file-api "update_file" (buffer-name)
                              (1- (line-number-at-pos lsp-bridge-org-babel--block-bop t)))))

(provide 'lsp-bridge-org-babel)
;;; lsp-bridge-org-babel.el ends here
