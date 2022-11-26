;;; acm-backend-tempel.el -*- lexical-binding: t -*-

(defgroup acm-backend-tempel nil
  "Tempel backend for acm."
  :group 'acm)

(defcustom acm-enable-tempel nil
  "Popup tempel completions when this option is turn on."
  :type 'boolean
  :group 'acm-backend-tempel)

(defcustom acm-backend-tempel-candidates-number 2
  "Maximal number of tempel candidate of menu."
  :type 'integer
  :group 'acm-backend-tempel)

(defun acm-backend-tempel-candidates (keyword)
  (when (and acm-enable-tempel
             (featurep 'tempel))
    (let* ((candidates (list))
           (snippets (cl-loop for template in (tempel--templates)
                              collect (format "%s" (car template))))
           (match-snippets (seq-filter (lambda (s) (acm-candidate-fuzzy-search keyword s)) snippets)))
      (dolist (snippet (cl-subseq match-snippets 0 (min (length match-snippets) acm-backend-tempel-candidates-number)))
        (add-to-list 'candidates (list :key snippet
                                       :icon "snippet"
                                       :label snippet
                                       :display-label snippet
                                       :annotation "Tempel"
                                       :backend "tempel")
                     t))
      (acm-candidate-sort-by-prefix keyword candidates))))

(defun acm-backend-tempel-candidate-expand (candidate-info bound-start)
  (delete-region bound-start (point))
  (tempel-insert (intern-soft (plist-get candidate-info :label))))

(defun acm-backend-tempel-candidate-doc (candidate)
  (let ((snippet
         (alist-get (intern-soft (plist-get candidate :label))
                    (tempel--templates))))
    (mapconcat #'tempel--print-element snippet " ")))

(provide 'acm-backend-tempel)

;;;acm-backend-tempel.el ends here
