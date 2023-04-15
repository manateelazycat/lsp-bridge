;;; acm-backend-codeium.el --- acm codeium support -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defgroup acm-backend-codeium nil
  "ACM codeium support."
  :group 'acm)

(defcustom acm-enable-codeium nil
  "Enable codeium support."
  :type 'boolean
  :group 'acm-backend-codeium)

(defcustom acm-backend-codeium-candidates-number 10
  "Maximal number of codeium candidate of menu."
  :type 'integer
  :group 'acm-backend-codeium)

(defcustom acm-backend-codeium-api-server-host "server.codeium.com"
  "Codeium api server host."
  :type 'string
  :group 'acm-backend-codeium)

(defcustom acm-backend-codeium-api-server-port 443
  "Codeium api server port."
  :type 'integer
  :group 'acm-backend-codeium)

(defcustom acm-backend-codeium-api-key ""
  "Codeium api key."
  :type 'string
  :group 'acm-backend-codeium)

(defcustom acm-backend-codeium-accept nil
  "Send accept request."
  :type 'boolean
  :group 'acm-backend-codeium)

(defvar-local acm-backend-codeium-items nil)

(defun acm-backend-codeium-candidates (_)
  (when acm-backend-codeium-items
    acm-backend-codeium-items))

(defun acm-backend-codeium-candidate-expand (candidate-info _)
  (delete-region (- (point) (length (plist-get candidate-info :old_prefix))) (point))
  (insert (plist-get candidate-info :label))

  (when acm-backend-codeium-accept
    (lsp-bridge-call-async "codeium_completion_accept" (plist-get candidate-info :id))))

(defun acm-backend-codeium-clean ()
  (setq-local acm-backend-codeium-items nil))

(provide 'acm-backend-codeium)
;;; acm-backend-codeium.el ends here
