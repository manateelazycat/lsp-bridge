;;; acm-backend-telega.el --- telega backend for acm -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defgroup acm-backend-telega nil
  "Telega backedn for ACM."
  :group 'acm)

(defcustom acm-enable-telega t
  "If non-nil enable telega username completion."
  :type 'boolean
  :group 'acm-backend-telega)

(defvar-local acm-backend-telega-items nil)

(defun acm-backend-telega-fetch-members ()
  "Fetch the current chat buffer members."
  (let ((members (telega--searchChatMembers (telega-chatbuf--chat (current-buffer)) nil nil :limit (* 20 10000))))
    (when members
      (cl-remove-if (lambda (user) (string-empty-p (plist-get user :username))) members))))

(defun acm-backend-telega-update-items ()
  "Update the optional items."
  ;; Scoped by current line: the @ character must be present before the cursor
  (when (and acm-enable-telega (eq major-mode 'telega-chat-mode))
    (unless acm-backend-telega-items
      (message "Fetch telega userlist...")
      (setq-local acm-backend-telega-items
                  (mapcar (lambda (user)
                            (let ((username (plist-get user :username))
                                  (firstname (plist-get user :first_name)))
                              (list :key username
                                    :icon "at"
                                    :label username
                                    :display-label username
                                    :annotation firstname
                                    :backend "telega")))
                          (acm-backend-telega-fetch-members)))
      (message "Fetch telega userlist done."))))

(defun acm-backend-telega-candidates (keyword)
  (when (and acm-enable-telega
             (eq major-mode 'telega-chat-mode)
             (save-excursion
               (backward-char (length keyword))
               (= (char-before) ?@)
               ))
    (if (string-equal keyword "")
        acm-backend-telega-items
      (let ((candiates (list)))
        (dolist (item acm-backend-telega-items)
          (when (or (string-match keyword (plist-get item :label))
                    (string-match keyword (plist-get item :annotation)))
            (add-to-list 'candiates item)))
        (acm-candidate-sort-by-prefix keyword candiates)))))

;; Update userlist when first switch to telega buffer.
(add-hook 'buffer-list-update-hook #'acm-backend-telega-update-items)

(provide 'acm-backend-telega)
;;; acm-backend-telega.el ends here
