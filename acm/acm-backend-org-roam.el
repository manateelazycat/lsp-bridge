;;; acm-backend-org-roam.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defgroup acm-backend-org-roam nil
  "Org roam backend for acm."
  :group 'acm)

(defcustom acm-enable-org-roam nil
  "Popup Org roam completions when this option is turn on."
  :type 'boolean
  :group 'acm-backend-org-roam)

(defcustom acm-backend-org-roam-candidates-number 10
  "Maximal number of Org roam candidate of menu."
  :type 'integer
  :group 'acm-backend-org-roam)

(defun acm-backend-org-roam-candidates (keyword)
  (when (and acm-enable-org-roam
             (featurep 'org-roam))
    (let* ((titles (org-roam--get-titles))
           (match-titles (seq-filter (lambda (s) (acm-candidate-fuzzy-search keyword s)) titles)))
      (acm-candidate-sort-by-prefix
       keyword
       (mapcar
        (lambda (title)
          (list :key title
                :icon "note"
                :label title
                :displayLabel title
                :annotation "Org roam"
                :backend "org-roam"))
        (cl-subseq match-titles 0 (min (length match-titles) acm-backend-org-roam-candidates-number)))))))

(defun acm-backend-org-roam-candidate-expand (candidate-info bound-start)
  (let (roam-p start end)
    (when (org-in-regexp org-roam-bracket-completion-re 1)
      (setq roam-p (not (or (org-in-src-block-p)
                            (string-blank-p (match-string 1))))
            start (match-beginning 2)
            end (match-end 2))
      (delete-region bound-start (point))
      (insert (if roam-p "" "roam:") (plist-get candidate-info :label))
      (forward-char 2))))

(provide 'acm-backend-org-roam)

;;;acm-backend-org-roam.el ends here
