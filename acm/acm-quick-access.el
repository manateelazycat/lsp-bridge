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

(defcustom acm-quick-access-use-number-select nil
  "Use the number keys to select candidate words, disabled by default.

Enable this option will cause interfering digital insertion sometimes."
  :type 'boolean
  :group 'acm-quick-access)

(defvar acm-quick-access-init-p nil)

(defun acm-quick-access-init ()
  (when (and (not acm-quick-access-init-p)
             acm-enable-quick-access)
    (acm-keymap--bind-quick-access acm-mode-map)
    (setq acm-quick-access-init-p t)))

(defun acm-keymap--quick-access-modifier ()
  "Return string representation of the `acm-quick-access-modifier'."
  (if-let* ((modifier (assoc-default acm-quick-access-modifier
                                    '((meta . "M")
                                      (super . "s")
                                      (hyper . "H")
                                      (control . "C")))))
      modifier
    (warn "acm-quick-access-modifier value unknown: %S"
          acm-quick-access-modifier)
    "M"))

(defun acm-keymap--bind-quick-access (keymap)
  (let ((modifier (acm-keymap--quick-access-modifier)))
    (dolist (key acm-quick-access-keys)
      (let ((key-seq (acm-keymap--kbd-quick-access modifier key)))
        (unless (lookup-key keymap key-seq)
          (define-key keymap key-seq #'acm-complete-quick-access))))))

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

(defun acm-insert-number-or-complete-candiate ()
  (interactive)
  (if acm-quick-access-use-number-select
      (let ((current-char (key-description (this-command-keys-vector)))
            complete-index)
        ;; Only complete candidate when match below rules:
        ;;
        ;; 1. User type number character
        ;; 2. User type number is equal or bigger than candidate length
        ;; 3. First character of rest candidate is not same with user type number
        ;; 4. Character before cursor is not number and equal-sign
        (when (string-match-p "[0-9]" current-char)
          (let* ((current-number (string-to-number current-char)))
            (when (>= (length acm-candidates) current-number)
              (let* (;; Decrease index if user type 1~9, adjust index to 9 if user type 0.
                     (index (if (equal current-number 0) 9 (1- current-number)))
                     (candiate (nth (+ acm-menu-offset index) acm-candidates))
                     (candidate-label (or (plist-get candiate :displayLabel) ""))
                     (prefix (acm-get-input-prefix))
                     (rest (cadr (split-string candidate-label prefix))))
                (unless (or (string-prefix-p current-char rest)
                            (string-match-p "[0-9=]" (string (char-before))))
                  (setq complete-index index))))))

        (if complete-index
            (acm-complete-quick-access complete-index)
          (insert current-char)))
    (self-insert-command 1)))

(provide 'acm-quick-access)

;;; acm-quick-access.el ends here
