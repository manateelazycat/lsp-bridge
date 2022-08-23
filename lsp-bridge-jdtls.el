;;; lsp-bridge-jdtls.el --- Provide jdtls configuration dynamically -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'json)

(defconst lsp-bridge-jdtls-workspace-file-name "jdtls.json")

(defcustom lsp-bridge-jdtls-worksapce
  (expand-file-name "~/.cache/lsp-bridge-jdtls")
  "LSP-Bridge jdtls workspace directory.")

(defvar lsp-bridge-jdtls-default-file
  (expand-file-name "langserver/jdtls.json"
                    (file-name-directory load-file-name)))

(defcustom lsp-bridge-jdtls-jvm-args '()
  "Specifies additional VM parameters for starting the Java language server.
E.g. Use `-javaagent:/home/user/.emacs.d/plugin/lombok.jar` to add lombok support")

(defun lsp-bridge-get-jdtls-server-by-project (project-path filepath)
  "Get JDTLS configuration"
  (let ((config-file (lsp-bridge-jdtls-config-file project-path filepath)))
    (if (not (file-exists-p config-file))
        (lsp-bridge-jdtls-init-config project-path filepath))
    config-file))


(defun lsp-bridge-jdtls-init-config (project-path filepath)
  "Initialize JDTLS configuration"
  (let* ((json-object-type 'plist)
         (config (json-read-file lsp-bridge-jdtls-default-file))
         (config-file (lsp-bridge-jdtls-config-file project-path filepath))
         (data-directory (lsp-bridge-jdtls-project-data-dir project-path filepath)))

    ;; An additional JVM option (can be used multiple times. Note, use with equal sign. For example: --jvm-arg=-Dlog.level=ALL
    (when (listp lsp-bridge-jdtls-jvm-args)
      (let ((jvm-args (mapcar (lambda (arg) (concat "--jvm-arg=" arg))
                              lsp-bridge-jdtls-jvm-args)))

        (plist-put config :command (vconcat (plist-get config :command)
                                            jvm-args))))

    ;; Add the `-data` parameter to the startup parameter
    (plist-put config :command (vconcat (plist-get config :command) `("-data" ,data-directory)))

    "Create parent directory if not exists while visiting file."
    (make-directory (file-name-directory config-file) t)

    (with-temp-file config-file
      (insert (json-encode config)))))

(defun lsp-bridge-jdtls-cache-dir (project-path filepath)
  "[LSP-Bridge] JDTLS cache directory"
  (if (lsp-bridge-jdtls-single-file-mode? project-path filepath)
      (lsp-bridge-jdtls-single-file-cache-dir filepath)
    (lsp-bridge-jdtls-project-cache-dir project-path)))

(defun lsp-bridge-jdtls-project-cache-dir (project-path)
  "Project cache directory"
  (let ((project-name (file-name-nondirectory project-path))
        (project-hash (md5 project-path)))
    (expand-file-name (concat project-name "-" project-hash)
                      lsp-bridge-jdtls-worksapce)))

(defun lsp-bridge-jdtls-single-file-cache-dir (filepath)
  "Signle file cache directory"
  (let ((single-file-hash (md5 filepath)))
    (expand-file-name single-file-hash
                      lsp-bridge-jdtls-worksapce)))

(defun lsp-bridge-jdtls-config-file (project-path filepath)
  "JDTLS configuration file path of the project"
  (expand-file-name lsp-bridge-jdtls-workspace-file-name
                    (lsp-bridge-jdtls-cache-dir project-path filepath)))

(defun lsp-bridge-jdtls-project-data-dir (project-path filepath)
  "Project data directory"
  (expand-file-name "jdtls-data"
                    (lsp-bridge-jdtls-cache-dir project-path filepath)))


(defun lsp-bridge-jdtls-single-file-mode? (project-path filepath)
  "Determine whether it is a single file mode?"
  (string-equal project-path filepath))

(add-hook 'java-mode-hook (lambda ()
                            (setq-local lsp-bridge-get-single-lang-server-by-project 'lsp-bridge-get-jdtls-server-by-project)))

(provide 'lsp-bridge-jdtls)
;;; lsp-bridge-jdtls.el ends here
