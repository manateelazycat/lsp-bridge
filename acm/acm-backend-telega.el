;;; acm-backend-telega.el --- telega backend for acm -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(defgroup acm-backend-telega nil
  "Telega backedn for ACM."
  :group 'acm)

(defcustom acm-enable-telega t
  "If non-nil enable telega username completion."
  :type 'boolean
  :group 'acm-backend-telega)

(defvar-local acm-backend-telega-items 'fetch
  "List of loaded items.
Number if async request has been made, but request is not yet completed.
`fetch' if members has not yet been fetched.")

(defun acm-backend-telega-enabled-p ()
  "Return non-nil if lsp-bridge is enabled in telega chatbufs."
  (and lsp-bridge-mode acm-enable-telega (derived-mode-p 'telega-chat-mode)))

(defun acm-backend-telega-items-set (members)
  "Callback for the `telega--searchChatMembers'.
Sets `acm-backend-telega-items' for the current chatbuf."
  (cl-assert (acm-backend-telega-enabled-p))
  (setq acm-backend-telega-items
        (mapcar (lambda (user)
                  (let ((username (telega-msg-sender-username user))
                        ;; TODO: support images/faces in the
                        ;; lsp-bridge completion popup, so title with
                        ;; avatars, badges and faces could be used
                        (title (telega-msg-sender-title user
                                 :with-avatar-p nil
                                 :with-badges-p nil
                                 :with-title-faces-p nil)))
                    (list :key username
                          :icon "at"
                          :label username
                          :displayLabel username
                          :annotation title
                          :backend "telega")))
                ;; NOTE: only members having username are
                ;; supported for lsp-bridge completion
                ;; TODO: support for members without username, as in
                ;; `telega-company-username' company backend
                (cl-remove-if-not #'telega-msg-sender-username members)))
  (message "Fetch telega userlist... done."))

(defun acm-backend-telega-update-items (&rest _args)
  "Update the optional items."
  ;; Scoped by current line: the @ character must be present before the cursor
  (when (acm-backend-telega-enabled-p)
    (cond ((version< telega-version "0.8.162")
           (message "telega: Update your telega to support lsp-bridge"))
          ((eq acm-backend-telega-items 'fetch)
           (let ((chat telega-chatbuf--chat))
             (cl-assert chat)
             (setq acm-backend-telega-items
                   ;; NOTE: fetching members for channels is available
                   ;; only for admins (from TDLib docs)
                   (when (telega-chat-match-p chat '(or (not (type channel))
                                                        (me-is-owner or-admin)))
                     (message "Fetch telega userlist...")
                     ;; TODO: possibly use "chatMembersFilterMention"
                     ;; member filter, because chat might be filtering
                     ;; messages by topic, see
                     ;; `telega-company-username' for details
                     (telega--searchChatMembers chat "" nil
                       :callback
                       (lambda (members)
                         (with-telega-chatbuf chat
                           (acm-backend-telega-items-set members))))))))
          )))

(defun acm-backend-telega-candidates (keyword)
  (when (and (acm-backend-telega-enabled-p)
             (save-excursion
               (backward-char (length keyword))
               (= (char-before) ?@)
               ))
    (if (string-equal keyword "")
        acm-backend-telega-items
      (acm-candidate-sort-by-prefix
       keyword
       (seq-filter
        (lambda (item)
          (or (string-match keyword (plist-get item :label))
              (string-match keyword (plist-get item :annotation))))
        acm-backend-telega-items)))))

;; Update userlist when first switch to telega buffer.
(advice-add 'telega-chatbuf--switch-in
            :after #'acm-backend-telega-update-items)

(provide 'acm-backend-telega)
;;; acm-backend-telega.el ends here
