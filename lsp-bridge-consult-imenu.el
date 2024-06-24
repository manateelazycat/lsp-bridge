;;; lsp-bridge-consult-imenu.el --- LSP bridge  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'consult-imenu)
(require 'lsp-bridge)

;;;###autoload
(defun lsp-bridge-consult-imenu-show ()
  (interactive)
  (lsp-bridge-call-file-api "document_symbol" nil))

(defun lsp-bridge--consult-imenu (filename filehost res)
  "Compute `imenu--index-alist' for RES vector of FILEHOST:FILENAME."
  (lsp-bridge--with-file-buffer filename filehost
                                (let* ((index-items
                                        (cl-labels
                                            ((dfs (&key name children range &allow-other-keys)
                                               (if (seq-empty-p children)
                                                   (cons name (acm-backend-lsp-position-to-point
                                                               (plist-get range :start)))
                                                 (cons name
                                                       (mapcar (lambda (c) (apply #'dfs c)) children)))))
                                          (mapcar (lambda (s) (apply #'dfs s)) res))))
                                  (setq consult-imenu--cache (cons (buffer-modified-tick)
                                                                   (consult-imenu--flatten
                                                                    nil nil index-items nil)))
                                  (consult-imenu))))

(provide 'lsp-bridge-consult-imenu)
;;; lsp-bridge-consult-imenu.el ends here