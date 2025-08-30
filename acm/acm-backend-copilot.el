;;; acm-backend-copilot.el --- Description -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Copyright (C) 2023 royokong
;;
;; Author: royokong <j614023177@icloud.com>
;; Maintainer: royokong <j614023177@icloud.com>
;; Created: 七月 22, 2023
;; Modified: 七月 22, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/royokong/acm-backend-copilot
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defcustom acm-enable-copilot nil
  "Enable copilot support."
  :type 'boolean
  :group 'acm-backend-copilot)

(defcustom acm-backend-copilot-launch-mode 'auto
  "How to launch the Copilot language server.
`auto' - Automatically detect (try node first, fallback to binary).
`binary' - Use copilot-language-server binary.
`node' - Use Node.js to run the language server."
  :type '(choice (const :tag "Auto detect" auto)
                 (const :tag "Binary executable" binary)
                 (const :tag "Node.js" node))
  :group 'acm-backend-copilot)

(defcustom acm-backend-copilot-node-path "node"
  "The path to node for copilot."
  :type 'string
  :group 'acm-backend-copilot)

(defcustom acm-backend-copilot-binary-path "copilot-language-server"
  "The path to copilot-language-server for copilot."
  :type 'string
  :group 'acm-backend-copilot)

(defcustom acm-backend-copilot-show-trailing nil
  "Show only the trailing parts for copilot. (like the VSCode)"
  :group 'acm-backend-copilot)

(defcustom acm-backend-copilot-accept nil
  "Send accept request."
  :type 'boolean
  :group 'acm-backend-copilot)

(defcustom acm-backend-copilot-network-proxy nil
  " from `copilot.el'
 Network proxy to use for Copilot. Nil means no proxy.
Format: '(:host \"127.0.0.1\" :port 80 :username \"username\" :password \"password\")
Username and password are optional.

If you are using a MITM proxy which intercepts TLS connections, you may need to disable
TLS verification. This can be done by setting a pair ':rejectUnauthorized :json-false'
in the proxy plist. For example:

  (:host \"127.0.0.1\" :port 80 :rejectUnauthorized :json-false)
"
  :type '(plist :tag "Uncheck all to disable proxy" :key-type symbol)
  :options '((:host string) (:port integer) (:username string) (:password string))
  :group 'acm-backend-copilot)


(defvar-local acm-backend-copilot-items nil)

(defun acm-backend-copilot-candidates (keyword)
  (acm-with-cache-candidates
   acm-backend-copilot-cache-candiates
   (if acm-backend-copilot-show-trailing
       (acm-backend-copilot-candidates-preprocess acm-backend-copilot-items)
     acm-backend-copilot-items)))

(defun acm-backend-copilot-candidates-preprocess (copilot-candidates)
  "Preprocess copilot candidates to show only trailing parts like VSCode Copilot."
  (let* ((line-text (buffer-substring-no-properties (line-beginning-position) (point)))
         (trimmed (string-trim-left line-text))
         (prefix (if (string-match ".*\\s-+" trimmed)
                     (match-string 0 trimmed)
                   ""))
         (prefix-len (length prefix)))
    (mapcar (lambda (copilot-candidate)
              (let* ((copilot-line (plist-get copilot-candidate :displayLabel))
                     (trailing-text (if (string-prefix-p prefix copilot-line)
                                        (substring copilot-line prefix-len)
                                      copilot-line)))
                (plist-put copilot-candidate :displayLabel trailing-text)
                copilot-candidate))
            copilot-candidates)))

(defun acm-backend-copilot-candidate-expand (candidate-info bound-start &optional preview)
  ;; We need replace whole area with copilot label.
  (let ((end-position (line-end-position)))
    (forward-line (- (plist-get candidate-info :line) (count-lines (point-min) (line-beginning-position))))
    (if preview
        (acm-preview-create-overlay (point) end-position (plist-get candidate-info :label))
      (delete-region (point) end-position)
      (insert (plist-get candidate-info :label))
      (when acm-backend-copilot-accept
        (lsp-bridge-call-async
         "copilot_completion_accept" (plist-get candidate-info :id))))))

(defun acm-backend-copilot-candidate-doc (candidate)
  (plist-get candidate :documentation))

(defun acm-backend-copilot-clean ()
  (setq-local acm-backend-copilot-items nil)
  (setq-local acm-backend-copilot-cache-candiates nil))

(provide 'acm-backend-copilot)
;;; acm-backend-copilot.el ends here
