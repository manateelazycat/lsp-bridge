;;; acm-quick-access.el -*- lexical-binding: t -*-

(defgroup acm-quick-access nil
  "ACM quick access."
  :group 'acm)

(defcustom acm-quick-access-keys '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0")
  "Character strings used as a part of quick-access key sequences."
  :type 'listp
  :group 'acm-quick-access)

(defcustom acm-quick-access-modifier 'meta
  "Modifier key used for quick-access keys sequences."
  :type '(choice (const :tag "Meta key" meta)
                 (const :tag "Super key" super)
                 (const :tag "Hyper key" hyper)
                 (const :tag "Control key" control))
  :group 'acm-quick-access)

(defvar acm-quick-access-init-p nil)

(defun acm-quick-access-init ()
  (when (and (not acm-quick-access-init-p)
             acm-enable-quick-access)
    (acm-keymap--bind-quick-access acm-mode-map)
    (setq acm-quick-access-init-p t)))

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
  (when acm-enable-quick-access
    (let ((modifier (acm-keymap--quick-access-modifier)))
      (dolist (key acm-quick-access-keys)
        (let ((key-seq (acm-keymap--kbd-quick-access modifier key)))
          (unless (lookup-key keymap key-seq)
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
