;;; acm-backend-tabnine.el --- acm tabnine support -*- lexical-binding: t; no-byte-compile: t; -*-
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
  (acm-with-cache-candidates
   acm-backend-tabnine-cache-candiates
   (when acm-backend-tabnine-items
     acm-backend-tabnine-items)))

(defun acm-backend-tabnine-candidate-expand (candidate-info bound-start &optional preview)
  ;; Insert TabNine suggestion.
  (delete-region bound-start (point))
  (insert (plist-get candidate-info :label))

  ;; Replace old suffix with new suffix for candidate to auto balance brackets.
  (let* ((old_suffix (plist-get candidate-info :old_suffix))
         (new_suffix (plist-get candidate-info :new_suffix))
         (beg (point))
         (end (min (+ (point) (length old_suffix))
                   (point-max))))
    (if preview
        (acm-preview-create-overlay beg end new_suffix)
      (delete-region beg end)
      (when (stringp new_suffix)
        (save-excursion
          (insert new_suffix))))))

(defun acm-backend-tabnine-clean ()
  (setq-local acm-backend-tabnine-items nil)
  (setq-local acm-backend-tabnine-cache-candiates nil))

(provide 'acm-backend-tabnine)
;;; acm-backend-tabnine.el ends here
