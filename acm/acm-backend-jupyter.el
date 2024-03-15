;;; acm-backend-jupyter.el --- acm codeium support -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(defgroup acm-backend-jupyter nil
  "ACM codeium support."
  :group 'acm)

(defcustom acm-enable-jupyter nil
  "Enable codeium support."
  :type 'boolean
  :group 'acm-backend-jupyter)

(defvar-local acm-backend-jupyter-items nil)

(defun acm-backend-jupyter-candidates (keyword)
  (acm-with-cache-candidates
   acm-backend-jupyter-cache-candiates
   (acm-candidate-sort-by-prefix
    keyword
    acm-backend-jupyter-items)))

(defun lsp-bridge-jupyter-record (candidates)
  (setq-local acm-backend-jupyter-items candidates)
  (lsp-bridge-try-completion))

(defun acm-backend-jupyter-clean ()
  (setq-local acm-backend-jupyter-items nil)
  (setq-local acm-backend-jupyter-cache-candiates nil))

(provide 'acm-backend-jupyter)
;;; acm-backend-jupyter.el ends here
