;;; lsp-bridge-imenu.el --- LSP bridge  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'imenu)
(require 'lsp-bridge)

(defcustom lsp-bridge-raw-imenu-modes
  '(emacs-lisp-mode org-mode)
  "Modes that use `imenu' instead of Lsp-bridge imenu."
  :type 'cons)

(defcustom lsp-bridge-imenu-function 'imenu
  "Determines which Imenu function is used.
Possible values: `imenu', `consult-imenu', or `consult-imenu-multi'."
  :type '(choice (const :tag "Default" imenu)
          (const :tag "Use consult-imenu" consult-imenu)
          (const :tag "Use consult-imenu-multi" consult-imenu-multi))
  :group 'lsp-bridge)

;;;###autoload
(defun lsp-bridge-imenu ()
  "Run an Imenu comprehension with the LSP server."
  (interactive)
  (if (memq major-mode lsp-bridge-raw-imenu-modes)
      (call-interactively lsp-bridge-imenu-function)
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
