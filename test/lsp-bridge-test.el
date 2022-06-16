(defun lsp-bridge-start-test ()
  "Start the LSP bridge test."
  (message "Starting LSP bridge test...")
  (setq lsp-bridge-enable-log 't)
  (setq lsp-bridge-epc-debug 't)
  (lsp-bridge-start-process)
  (while (not lsp-bridge-epc-process)
    (message "waiting for remote process to start...")
    (with-current-buffer lsp-bridge-name
      (message (buffer-string)))
    (sleep-for 1))
  (lsp-bridge-call-async "start_test")
  (while 't (sit-for 1)))
