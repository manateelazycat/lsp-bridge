;;; acm-backend-tabnine.el --- acm tabnine support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'acm)
(require 'tabnine-bridge)

(defgroup acm-backend-tabnine nil
  "ACM tabnine support."
  :group 'acm)

(defcustom acm-enable-tabnine-helper nil
  "Enable tabnine support"
  :type 'boolean
  :group 'acm-backend-tabnine)

(defcustom acm-backend-tabnine-min-length 1
  "Minimum length of tabnine word."
  :type 'integer
  :group 'acm-backend-tabnine)

(defcustom acm-backend-tabnine-predicate
  #'acm-backend-tabnine-predicate
  "Predicate of `acm-backend-tabnine'."
  :type 'symbol
  :local t
  :group 'acm-backend-tabnine)
(put 'acm-backend-tabnine-predicate 'safe-local-variable 'symbolp)

(with-eval-after-load 'lsp-bridge
  (when acm-enable-tabnine-helper
    (acm-backend-tabnine-start-server)))

(defun acm-bakcend-tabnine-predicate (_)
  acm-enable-tabnine-helper)

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
