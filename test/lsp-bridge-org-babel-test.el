;;; lsp-bridge-org-babel-test.el --- Org server lookup tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'lsp-bridge)

(defmacro lsp-bridge-test-with-org-block (language &rest body)
  (declare (indent 1))
  `(let ((lsp-bridge-enable-org-babel t)
         (lsp-bridge-use-wenls-in-org-mode nil)
         (lsp-bridge-use-ds-pinyin-in-org-mode nil)
         (lsp-bridge-org-babel-lang-list '("python"))
         (org-src-lang-modes '(("jupyter-python" . python)))
         (lsp-bridge-python-lsp-server "basedpyright"))
     (with-temp-buffer
       (delay-mode-hooks (org-mode))
       (insert "#+begin_src " ,language "\nimport os\n#+end_src\n")
       (setq buffer-file-name
             (expand-file-name "lsp-bridge-org-babel-test.org" temporary-file-directory))
       (set-buffer-modified-p nil)
       (goto-char (point-min))
       (forward-line 1)
       ,@body)))

(ert-deftest lsp-bridge-org-inline-python-server ()
  (lsp-bridge-test-with-org-block "python"
    (should (equal (lsp-bridge-get-single-lang-server-by-file-mode buffer-file-name)
                   "basedpyright"))))

(ert-deftest lsp-bridge-org-inline-jupyter-python-server ()
  (lsp-bridge-test-with-org-block "jupyter-python"
    (should (equal (lsp-bridge-get-single-lang-server-by-file-mode buffer-file-name)
                   "basedpyright"))))

(ert-deftest lsp-bridge-org-server-from-unrelated-callback-buffer ()
  (lsp-bridge-test-with-org-block "python"
    (let ((filename buffer-file-name))
      (with-temp-buffer
        (should (equal (lsp-bridge-get-single-lang-server-by-file-mode filename)
                       "basedpyright"))))))

(ert-deftest lsp-bridge-org-src-edit-server-still-takes-precedence ()
  (lsp-bridge-test-with-org-block "unsupported-language"
    (let ((filename buffer-file-name)
          (marker (point-marker)))
      (with-temp-buffer
        (setq major-mode 'python-mode)
        (setq-local org-src-mode t)
        (setq-local org-src--beg-marker marker)
        (should (equal (lsp-bridge-get-single-lang-server-by-file-mode filename)
                       "basedpyright"))))))

(ert-deftest lsp-bridge-org-no-server-on-block-boundaries ()
  (lsp-bridge-test-with-org-block "python"
    (goto-char (point-min))
    (should-not (lsp-bridge-get-single-lang-server-by-file-mode buffer-file-name))
    (forward-line 2)
    (should-not (lsp-bridge-get-single-lang-server-by-file-mode buffer-file-name))))

(ert-deftest lsp-bridge-org-disabled-or-unsupported-babel ()
  (lsp-bridge-test-with-org-block "python"
    (let ((lsp-bridge-enable-org-babel nil))
      (should-not (lsp-bridge-get-single-lang-server-by-file-mode buffer-file-name)))
    (let ((lsp-bridge-org-babel-lang-list '("bash")))
      (should-not (lsp-bridge-get-single-lang-server-by-file-mode buffer-file-name)))))

(ert-deftest lsp-bridge-org-non-babel-server-options ()
  (lsp-bridge-test-with-org-block "python"
    (let ((lsp-bridge-use-wenls-in-org-mode t))
      (should (equal (lsp-bridge-get-single-lang-server-by-file-mode buffer-file-name)
                     "wen")))
    (let ((lsp-bridge-use-ds-pinyin-in-org-mode t))
      (should (equal (lsp-bridge-get-single-lang-server-by-file-mode buffer-file-name)
                     "ds-pinyin")))))

(ert-deftest lsp-bridge-org-missing-buffer ()
  (should-not
   (lsp-bridge-get-single-lang-server-by-file-mode
    (expand-file-name "lsp-bridge-no-buffer.org" temporary-file-directory))))

(ert-deftest lsp-bridge-regular-python-server-unchanged ()
  (with-temp-buffer
    (setq buffer-file-name
          (expand-file-name "lsp-bridge-org-babel-test.py" temporary-file-directory))
    (setq major-mode 'python-mode)
    (let ((lsp-bridge-python-lsp-server "basedpyright"))
      (should (equal (lsp-bridge-get-single-lang-server-by-file-mode buffer-file-name)
                     "basedpyright")))))

;;; lsp-bridge-org-babel-test.el ends here
