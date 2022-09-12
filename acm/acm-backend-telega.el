(defcustom acm-enable-telega t
  "If non-nil enable telega username completion."
  :type 'boolean)

(defvar-local acm-backend-telega-items nil)

(defun acm-backend-telega-fetch-members ()
  "Fetch the current chat buffer members."
  (let ((members (telega--searchChatMembers (telega-chatbuf--chat (current-buffer)) nil nil :limit (* 20 10000))))
    (when members
      (remove-if (lambda (user) (string-empty-p (plist-get user :username))) members))))

(defun acm-backend-telega-update-items ()
  "Update the optional items."
  ;; Scoped by current line: the @ character must be present before the cursor
  (setq-local acm-backend-telega-items
              (when (save-excursion (search-backward "@" (line-beginning-position) t))
                (let ((at (char-to-string (char-before))))
                  ;; Get data only when you enter @, otherwise return the acquired data directly
                  (if (string= at "@")
                    (mapcar (lambda (user)
                              (let ((username (plist-get user :username))
                                    (firstname (plist-get user :first_name)))
                                (list :key username
                                      :icon "at"
                                      :label username
                                      :display-label username
                                      :annotation firstname
                                      :backend "telega")))
                            (acm-backend-telega-fetch-members))
		    acm-backend-telega-items)))))

(defun acm-backend-telega-candidates (keyword)
  (when (and acm-enable-telega (eq major-mode 'telega-chat-mode))
    (acm-backend-telega-update-items)
    (acm-candidate-sort-by-prefix keyword acm-backend-telega-items)))

(defun acm-backend-telega-clean ()
  (setq-local acm-backend-telega-items nil))

(provide 'acm-backend-telega)
