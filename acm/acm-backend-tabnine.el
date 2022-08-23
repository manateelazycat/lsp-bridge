;;; acm-backend-tabnine.el --- acm tabnine support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'tabnine-bridge)

(defcustom acm-backend-tabnine-min-length 1
  "Minimum length of tabnine word."
  :type 'integer)

(defun acm-backend-tabnine-candidates (keyword)
  (unless (or (and tabnine-bridge-no-continue
                   tabnine-bridge--calling-continue)
              tabnine-bridge--disabled)
    (tabnine-bridge-query))
  (let* ((candidates (list))
         (bounds (bounds-of-thing-at-point 'symbol))
         (thing (thing-at-point 'symbol))
         (tabcandidates (tabnine-bridge--candidates thing)))
    (setq-local tabnine-bridge--begin-pos (or (car bounds) (point)))
    (when (>= (length keyword) acm-backend-tabnine-min-length)
           (dolist (candidate tabcandidates)
             (let* ((com (concat "  "(get-text-property 0 'annotation candidate)))
                    (detail (concat "  "(get-text-property 0 'detail candidate))))
               ;; (debug)
               (add-to-list 'candidates (list :key candidate
                                         :icon "tabnine"
                                         :label candidate
                                         :display-label candidate
                                         :annotation "Tabnine"
                                         :backend "tabnine")
                       ))
           ))
    candidates))

(defun acm-backend-tabnine-candidate-expand (candidate-info bound-start)
  (when tabnine-bridge-auto-balance
    (let ((label (plist-get candidate-info :label)))
      (delete-region bound-start (point))
      (insert label)))
  )

(provide 'acm-backend-tabnine)
;;; acm-backend-tabnine.el ends here


