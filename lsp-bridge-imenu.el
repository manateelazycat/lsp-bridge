;;; lsp-bridge-imenu.el --- LSP bridge  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'imenu)
(require 'lsp-bridge)

;;;###autoload
(defun lsp-bridge-imenu ()
  (interactive)
  (if (equal major-mode 'emacs-lisp-mode)
      (call-interactively 'imenu)
    (lsp-bridge-call-file-api "imenu")))

(defun lsp-bridge--imenu-show (filename filehost res)
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
                                          (mapcar (lambda (s) (apply #'dfs s)) res)))
                                       (imenu-create-index-function #'(lambda () index-items))
                                       (last-nonmenu-event 13))
                                  (call-interactively 'imenu))))

(provide 'lsp-bridge-imenu)
;;; lsp-bridge-imenu.el ends here
