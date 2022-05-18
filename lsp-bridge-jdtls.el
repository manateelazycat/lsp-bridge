;;; lsp-bridge-jdtls.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst lsp-bridge-jdtls-workspace-file-name "jdtls.json")

(defcustom lsp-bridge-jdtls-worksapce
  (expand-file-name (locate-user-emacs-file "lsp-bridge-jdtls/"))
  "LSP-Bridge jdtls workspace directory.")

(defvar lsp-bridge-jdtls-default-file
  (expand-file-name "langserver/jdtls.json"
                    (file-name-directory load-file-name)))

(defun lsp-bridge-get-jdtls-server-by-project (project-path filepath)
  "Get JDTLS configuration"
  (if (lsp-bridge-jdtls-single-file-mode? project-path filepath)
      lsp-bridge-jdtls-default-file
    (let ((workspace-config-path (lsp-bridge-jdtls-workspace-file-path project-path)))
      (if (not (file-exists-p workspace-config-path))
          ;; Create a project configuration file
          (lsp-bridge-jdtls-init-jdtls-config project-path filepath))
      workspace-config-path)))

(defun lsp-bridge-jdtls-init-jdtls-config (project-path filepath)
  "Initialize JDTLS configuration"
  (let* ((json-object-type 'plist)
         (config (json-read-file lsp-bridge-jdtls-default-file))
         (project-hash (md5 project-path))
         (data-directory (expand-file-name project-hash lsp-bridge-jdtls-worksapce)))

    ;; Add the `-data` parameter to the startup parameter
    ;; --jvm-arg support?
    (plist-put config :command (vconcat (plist-get config :command) `("-data" ,data-directory)))

    (with-temp-buffer
      (insert (json-encode config))
      (json-pretty-print-buffer)
      (write-file (lsp-bridge-jdtls-workspace-file-path project-path)))))

(defun lsp-bridge-jdtls-workspace-file-path (project-path)
  "JDTLS configuration file path of the project"
  (expand-file-name lsp-bridge-jdtls-workspace-file-name project-path))

(defun lsp-bridge-jdtls-single-file-mode? (project-path filepath)
  "Determine whether it is a single file mode?"
  (equal project-path filepath))

(add-hook 'java-mode-hook (lambda ()
                            (setq-local lsp-bridge-get-lang-server-by-project
                                        'lsp-bridge-get-jdtls-server-by-project)))

(provide 'lsp-bridge-jdtls)
;;; lsp-bridge-jdtls.el ends here
