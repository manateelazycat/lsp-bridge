(defcustom acm-enable-telega t
  nil
  :type 'Boolean)

(defvar-local acm-backend-telega-items nil)

(defun acm-backend-telega-fetch ()
  "获取所需信息"
  (let ((users (list))
	(members (telega--searchChatMembers (telega-chatbuf--chat (current-buffer)) nil nil :limit (* 20 10000))))
    (when members

      (mapcar (lambda (user)
		;; 排除空字符串username
		(unless (string= "" (plist-get user :username))
		  (push user users)))
	      members))
    users))

(defun acm-backend-telega-update-items ()
  "只有当输入@时才获取所有用户名"
  ;; 行前必须得有@
  (if (save-excursion (search-backward "@" (line-beginning-position) t))
      (let ((at (buffer-substring (point) (- (point) 1))))
	;; 限制是在敲下@之后，才会获取数据，再后面都是使用旧的数据了
	(when (string= at "@")
	  (setq acm-backend-telega-items
		(mapcar (lambda (user)
			  (let ((username (plist-get user :username))
				(firstname (plist-get user :first_name)))
			    (list :key username
				:icon "at"
				:label username
				:display-label username
				:annotation firstname
				:backend "telega")))
			(acm-backend-telega-fetch)))))
    ;; 如果根本没有@，那么就不吐补全项
    (setq acm-backend-telega-items nil)))

(defun acm-backend-telega-candidates (keyword)
  (when (and acm-enable-telega (eq major-mode 'telega-chat-mode))
    (acm-backend-telega-update-items)
    (acm-candidate-sort-by-prefix keyword acm-backend-telega-items)
    acm-backend-telega-items))

(provide 'acm-backend-telega)
