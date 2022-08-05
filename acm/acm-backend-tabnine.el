;;; acm-backend-tabnine.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defcustom acm-backend-tabnine-min-length 2
  "Minimum length of tabnine word."
  :type 'integer)

(defun acm-backend-tabnine-candidates (keyword)
  (let* ((candidates (list))
         (test-completes '("test1" "test2" "test3")))
         (when (>= (length keyword) acm-backend-tabnine-min-length)
           (dolist (candidate test-completes)
           (add-to-list 'candidates (list :key candidate
                                         :icon "tabnine"
                                         :label candidate
                                         :display-label candidate
                                         :annotation candidate
                                         :backend "tabnine")
                       )))
    candidates))

(defun test-tabnine ()
  (interactive)
  (message "show tabnine")
  (message (format "%s" (acm-backend-tabnine-candidates "test"))))

(defun acm-backend-tabnine-candidate-expand (candidate-info bound-start)
  (let* ((keyword (acm-get-input-prefix))
         (label (plist-get candidate-info :label)))
    (delete-region bound-start (point))
    (insert (acm-backend-tabnine-convert-candidate keyword label))
    ))

(defun acm-backend-tabnine-convert-candidate (input candidate)
  (cond ((acm-backend-tabnine-upcase-string-p input)
         (upcase candidate))
        ((acm-backend-tabnine-capitalize-string-p input)
         (capitalize candidate))
        (t candidate)))

(defun acm-backend-tabnine-upcase-string-p (str)
  (let ((case-fold-search nil))
    (and (> (length str) 1)
         (string-match-p "\\`[A-Z]*\\'" str))))

(defun acm-backend-tabnine-capitalize-string-p (str)
  (let ((case-fold-search nil))
    (string-match-p "\\`[A-Z][a-z]*\\'" str)))

(provide 'acm-backend-tabnine)
;;; acm-backend-tabnine.el ends here
