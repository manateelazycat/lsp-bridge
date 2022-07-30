;;; acm-quick-access.el -*- lexical-binding: t; -*-

(defcustom acm-quick-access-keys '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0")
  "Character strings used as a part of quick-access key sequences."
  :type 'listp)

(defcustom acm-quick-access-modifier 'meta
  "Modifier key used for quick-access keys sequences."
  :type '(choice (const :tag "Meta key" meta)
                 (const :tag "Super key" super)
                 (const :tag "Hyper key" hyper)
                 (const :tag "Control key" control)))

(defun acm-keymap--quick-access-modifier ()
  "Return string representation of the `acm-quick-access-modifier'."
  (if-let ((modifier (assoc-default acm-quick-access-modifier
                                    '((meta . "M")
                                      (super . "s")
                                      (hyper . "H")
                                      (control . "C")))))
      modifier
    (warn "acm-quick-access-modifier value unknown: %S"
          acm-quick-access-modifier)
    "M"))

(defun acm-keymap--bind-quick-access (keymap)
  (if acm-enable-quick-access
      (let ((modifier (acm-keymap--quick-access-modifier)))
        (dolist (key acm-quick-access-keys)
          (let ((key-seq (acm-keymap--kbd-quick-access modifier key)))
            (if (lookup-key keymap key-seq)
                (warn "Key sequence %s already bound" (key-description key-seq))
              (define-key keymap key-seq #'acm-complete-quick-access)))))))

(defun acm-keymap--kbd-quick-access (modifier key)
  (kbd (format "%s-%s" modifier key)))

(defun acm-complete-quick-access (row)
  "Insert a candidate visible on a ROW matched by a quick-access key binding.
See `acm-quick-access-keys' for more details."
  (interactive
   (list (let* ((event-type (event-basic-type last-command-event))
                (event-string (if (characterp event-type)
                                  (string event-type)
                                (error "Unexpected input"))))
           (cl-position event-string acm-quick-access-keys :test 'equal))))
  (when row
    (setq-local acm-menu-index row)
    (acm-complete)))

(provide 'acm-quick-access)

;;; acm-quick-access.el ends here
