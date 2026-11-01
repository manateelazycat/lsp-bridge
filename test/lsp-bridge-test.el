(require 'ert)
(require 'acm-frame)

(ert-deftest acm-frame-uses-parent-minibuffer ()
  "Keep an ACM child frame independent from unrelated minibuffer frames."
  (skip-unless (display-graphic-p))
  (let (provider parent child
        (acm-frame-font acm-frame-font)
        (acm-frame--emacs-frame acm-frame--emacs-frame))
    (unwind-protect
        (progn
          (setq provider
                (make-frame '((name . "acm-test-minibuffer-provider")
                              (visibility . nil)
                              (minibuffer . t)))
                parent
                (make-frame '((name . "acm-test-parent")
                              (visibility . nil)
                              (minibuffer . t))))
          (let ((default-minibuffer-frame provider))
            (with-selected-frame parent
              (setq child (acm-frame-make-frame "acm-test-child" t))))
          (should (eq (frame-parent child) parent))
          (should (eq (window-frame (frame-parameter child 'minibuffer))
                      parent))
          (delete-frame provider t)
          (should-not (frame-live-p provider)))
      (dolist (frame (list child parent provider))
        (when (frame-live-p frame)
          (delete-frame frame t))))))

(defun lsp-bridge-start-test ()
  "Start the LSP bridge test."
  (message "Starting LSP bridge test...")
  (setq lsp-bridge-log-level 'debug)
  (setq lsp-bridge-epc-debug 't)
  (lsp-bridge-start-process)
  (while (not lsp-bridge-epc-process)
    (message "waiting for remote process to start...")
    (with-current-buffer lsp-bridge-name
      (message (buffer-string)))
    (sleep-for 1))
  (lsp-bridge-call-async "start_test")
  (while 't (sit-for 1)))
