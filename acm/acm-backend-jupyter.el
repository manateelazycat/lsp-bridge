;;; acm-backend-jupyter.el --- acm codeium support -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(defgroup acm-backend-jupyter nil
  "ACM jupyter support."
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

(defun acm-backend-jupyter-record (current-symbol)
  (interactive)
  (unless (fboundp 'jupyter-org-completion-at-point)
    (require 'jupyter)
    (require 'jupyter-org-client)
    (require 'ob-jupyter))
  (setq-local acm-backend-jupyter-items nil)
  (let* ((candidates (mapcar #'substring-no-properties (all-completions "" (nth 2 (jupyter-org-completion-at-point)))))
         (candidates (if (stringp current-symbol)
                         (seq-filter (apply-partially #'string-prefix-p current-symbol) candidates)
                       candidates))
         (candidates-with-metadata (mapcar (lambda (candidate)
                                             (list :key candidate
                                                   :icon "jupyter"
                                                   :label candidate
                                                   :displayLabel candidate
                                                   :annotation "jupyter"
                                                   :backend "jupyter"))
                                           candidates)))
    (when candidates
      (lsp-bridge-search-backend--record-items "jupyter" candidates-with-metadata))))

(defun acm-backend-jupyter-clean ()
  (setq-local acm-backend-jupyter-items nil)
  (setq-local acm-backend-jupyter-cache-candiates nil))

(provide 'acm-backend-jupyter)
;;; acm-backend-jupyter.el ends here
