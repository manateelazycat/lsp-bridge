;;; acm-backend-tabnine.el --- acm tabnine support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defgroup acm-backend-tabnine nil
  "ACM tabnine support."
  :group 'acm)

(defcustom acm-enable-tabnine t
  "Enable tabnine support"
  :type 'boolean
  :group 'acm-backend-tabnine)

(defvar-local acm-backend-tabnine-items nil)

(defun acm-backend-tabnine-candidates (keyword)
  (when acm-backend-tabnine-items
    acm-backend-tabnine-items))

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

(defun acm-backend-tabnine-clean ()
  (setq-local acm-backend-tabnine-items nil))

(provide 'acm-backend-tabnine)
;;; acm-backend-tabnine.el ends here
