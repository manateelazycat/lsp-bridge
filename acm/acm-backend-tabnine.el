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
        (add-to-list 'candidates (list
                                  :key candidate
                                  :icon "tabnine"
                                  :label candidate
                                  :display-label candidate
                                  :annotation (get-text-property 0 'annotation candidate)
                                  :backend "tabnine"
                                  :new_suffix (get-text-property 0 'new_suffix candidate)
                                  :old_suffix (get-text-property 0 'old_suffix candidate)))))
    (sort candidates #'(lambda (c-a c-b)
                         (string> (plist-get c-a :annotation)
                                  (plist-get c-b :annotation)
                                  )))))

(defun acm-backend-tabnine-candidate-expand (candidate-info bound-start)
  ;; Insert TabNine suggestion.
  (delete-region bound-start (point))
  (insert (plist-get candidate-info :label))

  ;; Replace old suffix with new suffix for candidate to auto balance brackets.
  (let ((old_suffix (plist-get candidate-info :old_suffix))
        (new_suffix (plist-get candidate-info :new_suffix)))
    (delete-region (point)
                   (min (+ (point) (length old_suffix))
                        (point-max)))
    (when (stringp new_suffix)
      (save-excursion
        (insert new_suffix)))))

(defun acm-backend-tabnine-start-server ()
  (when (null tabnine-bridge--process)
    (tabnine-bridge-start-process)))

(provide 'acm-backend-tabnine)
;;; acm-backend-tabnine.el ends here
