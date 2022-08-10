;;; acm-backend-tabnine.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'tabnine-capf)

(defcustom acm-backend-tabnine-min-length 1
  "Minimum length of tabnine word."
  :type 'integer)

(defun acm-backend-tabnine-candidates (keyword)
  (let* ((candidates (list))
         (test-completes '("test1" "test2" "test3"))
         (bounds (bounds-of-thing-at-point 'symbol))
         (thing (thing-at-point 'symbol))
         (tabcandidates (tabnine-capf--candidates thing)))
    ;; (debug)
         (when (>= (length keyword) acm-backend-tabnine-min-length)
           (dolist (candidate tabcandidates)
             (let* ((com (concat "  "(get-text-property 0 'annotation candidate)))
                    (detail (concat "  "(get-text-property 0 'detail candidate))))
               ;; (debug)
               (add-to-list 'candidates (list :key candidate
                                         :icon "tabnine"
                                         :label candidate
                                         :display-label candidate
                                         :annotation com
                                         :backend "tabnine")
                       ))
           ))
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
