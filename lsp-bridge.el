;;; lsp-bridge.el --- LSP bridge  -*- lexical-binding: t; -*-

;; Filename: lsp-bridge.el
;; Description: LSP bridge
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.5
;; Last-Updated: Wed May 11 02:44:16 2022 (-0400)
;;           By: Mingde (Matthew) Zeng
;; URL: https://github.com/manateelazycat/lsp-bridge
;; Keywords:
;; Compatibility: emacs-version >= 27
;; Package-Requires: ((emacs "27"))
;;
;; Features that might be required by this library:
;;
;; Please check README
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Lsp-Bridge
;;

;;; Installation:
;;
;; Please check README
;;

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET lsp-bridge RET
;;

;;; Change log:
;;
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'lsp-bridge-epc)
(require 'lsp-bridge-ref)
(require 'posframe)
(require 'markdown-mode)

(defgroup lsp-bridge nil
  "LSP-Bridge group."
  :group 'applications)


(defcustom lsp-bridge-enable-popup-predicates '(lsp-bridge-not-empty-candidates
                                                lsp-bridge-not-only-blank-before-cursor
                                                lsp-bridge-not-match-hide-characters
                                                lsp-bridge-not-match-completion-position
                                                lsp-bridge-not-match-stop-commands
                                                lsp-bridge-is-evil-insert-state
                                                )
  "A list of predicate functions with no argument to enable popup completion in callback."
  :type 'list
  :group 'lsp-bridge)

(defcustom lsp-bridge-flash-line-delay .3
  "How many seconds to flash `lsp-bridge-font-lock-flash' after navigation.

Setting this to nil or 0 will turn off the indicator."
  :type 'number
  :group 'lsp-bridge)

(defcustom lsp-bridge-completion-stop-commands
  '("corfu-complete" "corfu-insert"
    "undo-tree-undo" "undo-tree-redo"
    "kill-region" "delete-block-backward" "company-complete-selection")
  "If last command is match this option, stop popup completion ui."
  :type 'cons
  :group 'lsp-bridge)

(defcustom lsp-bridge-hide-completion-characters '(":" ";" "(" ")" "[" "]" "{" "}" "," "\"")
  "If character before match this option, stop popup completion ui."
  :type 'cons
  :group 'lsp-bridge)

(defcustom lsp-bridge-identify-annotation '("Snippet" "Emmet Abbreviation")
  "LSP Server will return candidates include same 'label',
then candidates have same `label' will filter by completion ui, such as Corfu.

So we will add blank after label if candidate annotation match tihs option,
to avoid completion ui filter candidates."
  :type 'cons
  :group 'lsp-bridge)

(defcustom lsp-bridge-lookup-doc-tooltip " *lsp-bridge-hover*"
  "Buffer for display hover information."
  :type 'string
  :group 'lsp-bridge)

(defcustom lsp-bridge-lookup-doc-tooltip-text-scale 0.5
  "The text scale for lsp-bridge hover tooltip."
  :type 'float
  :group 'lsp-bridge)

(defcustom lsp-bridge-lookup-doc-tooltip-border-width 20
  "The border width of lsp-bridge hover tooltip, in pixels."
  :type 'integer
  :group 'lsp-bridge)

(defcustom lsp-bridge-lookup-doc-tooltip-max-width 150
  "The max width of lsp-bridge hover tooltip."
  :type 'integer
  :group 'lsp-bridge)

(defcustom lsp-bridge-lookup-doc-tooltip-max-height 20
  "The max width of lsp-bridge hover tooltip."
  :type 'integer
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-auto-import nil
  "Whether to enable auto-import."
  :type 'boolean
  :group 'lsp-bridge)

(defcustom lsp-bridge-disable-backup t
  "Default disable backup feature, include `make-backup-files' `auto-save-default' and `create-lockfiles'."
  :type 'boolean
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-signature-help nil
  "Whether to enable signature-help."
  :type 'boolean
  :group 'lsp-bridge)

(defface lsp-bridge-font-lock-flash
  '((t (:inherit highlight)))
  "Face to flash the current line."
  :group 'lsp-bridge)

(defvar lsp-bridge-last-change-command nil)
(defvar lsp-bridge-server nil
  "The LSP-Bridge Server.")

(defvar lsp-bridge-python-file (expand-file-name "lsp_bridge.py" (file-name-directory load-file-name)))

(defvar lsp-bridge-mark-ring nil
  "The list of saved lsp-bridge marks, most recent first.")

(defcustom lsp-bridge-mark-ring-max 16
  "Maximum size of lsp-bridge mark ring.  \
Start discarding off end if gets this big."
  :type 'integer)

(defvar lsp-bridge-server-port nil)

(defun lsp-bridge--start-epc-server ()
  "Function to start the EPC server."
  (unless (process-live-p lsp-bridge-server)
    (setq lsp-bridge-server
          (lsp-bridge-epc-server-start
           (lambda (mngr)
             (let ((mngr mngr))
               (lsp-bridge-epc-define-method mngr 'eval-in-emacs 'lsp-bridge--eval-in-emacs-func)
               (lsp-bridge-epc-define-method mngr 'get-emacs-var 'lsp-bridge--get-emacs-var-func)
               (lsp-bridge-epc-define-method mngr 'get-emacs-vars 'lsp-bridge--get-emacs-vars-func)
               (lsp-bridge-epc-define-method mngr 'get-lang-server 'lsp-bridge--get-lang-server-func)
               (lsp-bridge-epc-define-method mngr 'get-emacs-version 'emacs-version)
               (lsp-bridge-epc-define-method mngr 'is-snippet-support 'lsp-bridge--snippet-expansion-fn)
               ))))
    (if lsp-bridge-server
        (setq lsp-bridge-server-port (process-contact lsp-bridge-server :service))
      (error "[LSP-Bridge] lsp-bridge-server failed to start")))
  lsp-bridge-server)

(defun lsp-bridge--eval-in-emacs-func (sexp-string)
  (eval (read sexp-string)))

(defun lsp-bridge--get-emacs-var-func (var-name)
  (let* ((var-symbol (intern var-name))
         (var-value (symbol-value var-symbol))
         ;; We need convert result of booleanp to string.
         ;; Otherwise, python-epc will convert all `nil' to [] at Python side.
         (var-is-bool (prin1-to-string (booleanp var-value))))
    (list var-value var-is-bool)))

(defun lsp-bridge--get-emacs-vars-func (&rest vars)
  (mapcar #'lsp-bridge--get-emacs-var-func vars))

(defvar lsp-bridge-epc-process nil)

(defvar lsp-bridge-internal-process nil)
(defvar lsp-bridge-internal-process-prog nil)
(defvar lsp-bridge-internal-process-args nil)

(defcustom lsp-bridge-name "*lsp-bridge*"
  "Name of LSP-Bridge buffer."
  :type 'string)

(defcustom lsp-bridge-python-command (if (memq system-type '(cygwin windows-nt ms-dos)) "python.exe" "python3")
  "The Python interpreter used to run lsp_bridge.py."
  :type 'string)

(defcustom lsp-bridge-enable-debug nil
  "If you got segfault error, please turn this option.
Then LSP-Bridge will start by gdb, please send new issue with `*lsp-bridge*' buffer content when next crash."
  :type 'boolean)

(defcustom lsp-bridge-enable-log nil
  "Enable this option to print log message in `*lsp-bridge*' buffer, default only print message header."
  :type 'boolean)

(defcustom lsp-bridge-lang-server-extension-list
  '(
    (("vue") . "volar")
    (("wxml") . "wxml-language-server")
    (("html") . "vscode-html-language-server")
    )
  "The lang server rule for file extension."
  :type 'cons)

(defcustom lsp-bridge-lang-server-mode-list
  '(
    ((c-mode c++-mode) . "clangd")
    (java-mode . "jdtls")
    (python-mode . "pyright")
    (ruby-mode . "solargraph")
    ((rust-mode rustic-mode) . "rust-analyzer")
    (elixir-mode . "elixirLS")
    (go-mode . "gopls")
    (haskell-mode . "hls")
    (lua-mode . "sumneko")
    (dart-mode . "dart-analysis-server")
    (scala-mode . "metals")
    ((js2-mode js-mode rjsx-mode) . "javascript")
    (typescript-tsx-mode . "typescriptreact")
    ((typescript-mode) . "typescript")
    (tuareg-mode . "ocamllsp")
    (erlang-mode . "erlang-ls")
    ((latex-mode Tex-latex-mode texmode context-mode texinfo-mode bibtex-mode) . "texlab")
    ((clojure-mode clojurec-mode clojurescript-mode clojurex-mode) . "clojure-lsp")
    ((sh-mode) . "bash-language-server")
    ((css-mode) . "vscode-css-language-server")
    (elm-mode . "elm-language-server")
    )
  "The lang server rule for file mode."
  :type 'cons)

(defcustom lsp-bridge-default-mode-hooks
  '(c-mode-hook
    c++-mode-hook
    java-mode-hook
    python-mode-hook
    ruby-mode-hook
    lua-mode-hook
    rust-mode-hook
    rustic-mode-hook
    elixir-mode-hook
    go-mode-hook
    haskell-mode-hook
    haskell-literate-mode-hook
    dart-mode-hook
    scala-mode-hook
    typescript-mode-hook
    typescript-tsx-mode-hook
    js2-mode-hook
    js-mode-hook
    rjsx-mode-hook
    tuareg-mode-hook
    latex-mode-hook
    Tex-latex-mode-hook
    texmode-hook
    context-mode-hook
    texinfo-mode-hook
    bibtex-mode-hook
    clojure-mode-hook
    clojurec-mode-hook
    clojurescript-mode-hook
    clojurex-mode-hook
    sh-mode-hook
    web-mode-hook
    css-mode-hook
    elm-mode-hook)
  "The default mode hook to enable lsp-bridge."
  :type 'list)

(defvar lsp-bridge-get-lang-server-by-project nil
  "Get lang server with project path and file path.")

(defmacro lsp-bridge--with-file-buffer (filepath &rest body)
  "Evaluate BODY in buffer with FILEPATH."
  (declare (indent 1))
  `(let ((buffer (lsp-bridge-get-match-buffer ,filepath)))
     (when buffer
       (with-current-buffer buffer
         ,@body))))

(defun lsp-bridge-get-match-buffer (filepath)
  (catch 'find-match
    (dolist (buffer (buffer-list))
      (if-let ((file-name (buffer-file-name buffer)))
          (when (or (string-equal file-name filepath)
                    (string-equal (file-truename file-name) filepath))
            (throw 'find-match buffer))))
    nil))

(defun lsp-bridge--get-lang-server-func (project-path filepath)
  "Get lang server with project path, file path or file extension."
  (let (lang-server-by-project
        lang-server-by-extension)
    ;; Step 1: Search lang server base on project rule provide by `lsp-bridge-get-lang-server-by-project'.
    (when lsp-bridge-get-lang-server-by-project
      (setq lang-server-by-project (funcall lsp-bridge-get-lang-server-by-project project-path filepath)))

    (if lang-server-by-project
        lang-server-by-project
      ;; Step 2: search lang server base on extension rule provide by `lsp-bridge-lang-server-extension-list'.
      (setq lang-server-by-extension (lsp-bridge-get-lang-server-by-extension filepath))
      (if lang-server-by-extension
          lang-server-by-extension
        ;; Step 3: search lang server base on mode rule provide by `lsp-bridge-lang-server-extension-list'.
        (lsp-bridge--with-file-buffer filepath
          (lsp-bridge-get-lang-server-by-mode))))))

(defun lsp-bridge-get-lang-server-by-extension (filepath)
  "Get lang server for file extension."
  (let* ((file-extension (file-name-extension filepath))
         (langserver-info (cl-find-if
                           (lambda (pair)
                             (let ((extension (car pair)))
                               (if (eq (type-of extension) 'string)
                                   (string-equal file-extension extension)
                                 (member file-extension extension))))
                           lsp-bridge-lang-server-extension-list)))
    (if langserver-info
        (cdr langserver-info)
      nil)))

(defun lsp-bridge-get-lang-server-by-mode ()
  "Get lang server for file mode."
  (let ((langserver-info (cl-find-if
                          (lambda (pair)
                            (let ((mode (car pair)))
                              (if (symbolp mode)
                                  (eq major-mode mode)
                                (member major-mode mode))))
                          lsp-bridge-lang-server-mode-list)))
    (if langserver-info
        (cdr langserver-info)
      nil)))

(defun lsp-bridge--auto-tick ()
  "Return the current tick/status of the buffer.
Auto completion is only performed if the tick did not change."
  (list (current-buffer) (buffer-chars-modified-tick) (point)))

(defun lsp-bridge-call-async (method &rest args)
  "Call Python EPC function METHOD and ARGS asynchronously."
  (lsp-bridge-deferred-chain
    (lsp-bridge-epc-call-deferred lsp-bridge-epc-process (read method) args)))

(defun lsp-bridge-call-sync (method &rest args)
  "Call Python EPC function METHOD and ARGS synchronously."
  (lsp-bridge-epc-call-sync lsp-bridge-epc-process (read method) args))

(defvar lsp-bridge-is-starting nil)

(defun lsp-bridge-restart-process ()
  "Stop and restart LSP-Bridge process."
  (interactive)
  (setq lsp-bridge-is-starting nil)

  (lsp-bridge-kill-process)
  (lsp-bridge-start-process)
  (message "[LSP-Bridge] Process restarted."))

(defun lsp-bridge-start-process ()
  "Start LSP-Bridge process if it isn't started."
  (setq lsp-bridge-is-starting t)
  (unless (lsp-bridge-epc-live-p lsp-bridge-epc-process)
    ;; start epc server and set `lsp-bridge-server-port'
    (lsp-bridge--start-epc-server)
    (let* ((lsp-bridge-args (append
                             (list lsp-bridge-python-file)
                             (list (number-to-string lsp-bridge-server-port))
                             )))

      ;; Set process arguments.
      (if lsp-bridge-enable-debug
          (progn
            (setq lsp-bridge-internal-process-prog "gdb")
            (setq lsp-bridge-internal-process-args (append (list "-batch" "-ex" "run" "-ex" "bt" "--args" lsp-bridge-python-command) lsp-bridge-args)))
        (setq lsp-bridge-internal-process-prog lsp-bridge-python-command)
        (setq lsp-bridge-internal-process-args lsp-bridge-args))

      ;; Start python process.
      (let ((process-connection-type (not (lsp-bridge--called-from-wsl-on-windows-p))))
        (setq lsp-bridge-internal-process
              (apply 'start-process
                     lsp-bridge-name lsp-bridge-name
                     lsp-bridge-internal-process-prog lsp-bridge-internal-process-args)))
      (set-process-query-on-exit-flag lsp-bridge-internal-process nil))))

(defun lsp-bridge--called-from-wsl-on-windows-p ()
  "Check whether lsp-bridge is called by Emacs on WSL and is running on Windows."
  (and (eq system-type 'gnu/linux)
       (string-match-p ".exe" lsp-bridge-python-command)))

(defvar lsp-bridge-stop-process-hook nil)

(defun lsp-bridge-kill-process ()
  "Stop LSP-Bridge process and kill all LSP-Bridge buffers."
  (interactive)

  ;; Run stop process hooks.
  (run-hooks 'lsp-bridge-stop-process-hook)

  ;; Kill process after kill buffer, make application can save session data.
  (lsp-bridge--kill-python-process))

(add-hook 'kill-emacs-hook #'lsp-bridge-kill-process)

(defun lsp-bridge--kill-python-process ()
  "Kill LSP-Bridge background python process."
  (interactive)
  (when (lsp-bridge-epc-live-p lsp-bridge-epc-process)
    ;; Cleanup before exit LSP-Bridge server process.
    (lsp-bridge-call-async "cleanup")
    ;; Delete LSP-Bridge server process.
    (lsp-bridge-epc-stop-epc lsp-bridge-epc-process)
    ;; Kill *lsp-bridge* buffer.
    (when (get-buffer lsp-bridge-name)
      (kill-buffer lsp-bridge-name))
    (setq lsp-bridge-epc-process nil)
    (message "[LSP-Bridge] Process terminated.")))

(defun lsp-bridge--first-start (lsp-bridge-epc-port)
  "Call `lsp-bridge--open-internal' upon receiving `start_finish' signal from server."
  ;; Make EPC process.
  (setq lsp-bridge-epc-process (make-lsp-bridge-epc-manager
                                :server-process lsp-bridge-internal-process
                                :commands (cons lsp-bridge-internal-process-prog lsp-bridge-internal-process-args)
                                :title (mapconcat 'identity (cons lsp-bridge-internal-process-prog lsp-bridge-internal-process-args) " ")
                                :port lsp-bridge-epc-port
                                :connection (lsp-bridge-epc-connect "localhost" lsp-bridge-epc-port)
                                ))
  (lsp-bridge-epc-init-epc-layer lsp-bridge-epc-process)
  (setq lsp-bridge-is-starting nil))

(defvar-local lsp-bridge-last-position 0)
(defvar-local lsp-bridge-completion-candidates nil)
(defvar-local lsp-bridge-completion-server-name nil)
(defvar-local lsp-bridge-completion-trigger-characters nil)
(defvar-local lsp-bridge-completion-prefix nil)
(defvar-local lsp-bridge-completion-common nil)
(defvar-local lsp-bridge-completion-position nil)
(defvar-local lsp-bridge-filepath "")
(defvar-local lsp-bridge-prohibit-completion nil)
(defvar-local lsp-bridge-current-tick nil)
(defvar-local lsp-bridge-diagnostic-overlays '())

(defun lsp-bridge-char-before ()
  (let ((prev-char (char-before)))
    (if prev-char (char-to-string prev-char) "")))

(defun lsp-bridge-monitor-post-command ()
  (when (lsp-bridge-epc-live-p lsp-bridge-epc-process)
    (unless (equal (point) lsp-bridge-last-position)
      (lsp-bridge-call-async "change_cursor" lsp-bridge-filepath (lsp-bridge--position))
      (setq-local lsp-bridge-last-position (point))))

  ;; Hide hover tooltip.
  (if (not (string-prefix-p "lsp-bridge-popup-documentation-scroll" (format "%s" this-command)))
      (lsp-bridge-hide-doc-tooltip))

  ;; Hide diagnostic tooltip.
  (unless (member (format "%s" this-command) '("lsp-bridge-jump-to-next-diagnostic"
                                               "lsp-bridge-jump-to-prev-diagnostic"))
    (lsp-bridge-hide-diagnostic-tooltip)))

(defun lsp-bridge-monitor-kill-buffer ()
  (when (lsp-bridge-epc-live-p lsp-bridge-epc-process)
    (lsp-bridge-call-async "close_file" lsp-bridge-filepath)))

(defun lsp-bridge-is-empty-list (list)
  (or (and (eq (length list) 1)
           (string-empty-p (format "%s" (car list))))
      (and (eq (length list) 0))))

(defun lsp-bridge-record-completion-items (filepath common items position server-name completion-trigger-characters)
  (lsp-bridge--with-file-buffer filepath
    ;; Save completion items.
    (setq-local lsp-bridge-completion-common common)
    (setq-local lsp-bridge-completion-position position)
    (setq-local lsp-bridge-completion-server-name server-name)
    (setq-local lsp-bridge-completion-trigger-characters completion-trigger-characters)

    (setq-local lsp-bridge-completion-candidates (make-hash-table :test 'equal))
    (dolist (item items)
      (let* ((item-label (plist-get item :label))
             (item-annotation (plist-get item :annotation))
             (item-key (if (member item-annotation lsp-bridge-identify-annotation)
                           (format "%s " item-label)
                         item-label)))
        (puthash item-key item lsp-bridge-completion-candidates)))

    (if lsp-bridge-prohibit-completion
        (setq lsp-bridge-prohibit-completion nil)

      ;; Try popup completion frame.
      (when (cl-every (lambda (pred)
                        (if (functionp pred) (funcall pred) t))
                      lsp-bridge-enable-popup-predicates)
        (pcase (while-no-input ;; Interruptible capf query
                 (run-hook-wrapped 'completion-at-point-functions #'corfu--capf-wrapper))
          (`(,fun ,beg ,end ,table . ,plist)
           (let ((completion-in-region-mode-predicate
                  (lambda () (eq beg (car-safe (funcall fun)))))
                 (completion-extra-properties plist))
             (setq completion-in-region--data
                   (list (if (markerp beg) beg (copy-marker beg))
                         (copy-marker end t)
                         table
                         (plist-get plist :predicate)))

             ;; Refresh candidates forcibly!!!
             (pcase-let* ((`(,beg ,end ,table ,pred) completion-in-region--data)
                          (pt (- (point) beg))
                          (str (buffer-substring-no-properties beg end)))
               (corfu--update-candidates str pt table (plist-get plist :predicate)))

             ;; Setup hook.
             (corfu--setup)

             ;; When you finger faster than LSP server,
             ;; Corfu will **auto insert** select candidate when lsp-bridge push newest completion data,
             ;; so we need set `corfu-on-exact-match' to quit to prohibit corfu insert select candidate.
             (let ((corfu-on-exact-match 'quit))
               (corfu--update)))))
        ))))

(defun lsp-bridge-not-empty-candidates ()
  "Hide completion if candidates list is empty."
  (not (lsp-bridge-is-empty-list (hash-table-keys lsp-bridge-completion-candidates))))

(defun lsp-bridge-not-match-completion-position ()
  "Hide completion if the position of cursor has changed."
  (equal lsp-bridge-current-tick (lsp-bridge--auto-tick)))

(defun lsp-bridge-not-match-stop-commands ()
  "Hide completion if `lsp-bridge-last-change-command' match commands in `lsp-bridge-completion-stop-commands'."
  (not (member lsp-bridge-last-change-command lsp-bridge-completion-stop-commands)))

(defun lsp-bridge-not-match-hide-characters ()
  "Hide completion if cursor after hide character match `lsp-bridge-hide-completion-characters'."
  (not (member (char-to-string (char-before)) lsp-bridge-hide-completion-characters)))

(defun lsp-bridge-not-only-blank-before-cursor ()
  "Hide completion if only blank before cursor."
  (split-string (buffer-substring-no-properties
                 (max (1- (point)) (line-beginning-position))
                 (point))))

(defun lsp-bridge-is-evil-insert-state ()
  "If `evil' mode is enable, only show completion when evil is in insert mode."
  (or (not (featurep 'evil)) (evil-insert-state-p)))

(defun lsp-bridge-capf ()
  "Capf function similar to eglot's 'eglot-completion-at-point'."
  (let* ((candidates (hash-table-keys lsp-bridge-completion-candidates))
         (bounds (bounds-of-thing-at-point 'symbol))
         (bounds-start (or (car bounds) (point)))
         (bounds-end (or (cdr bounds) (point))))
    (list
     bounds-start
     bounds-end
     candidates

     :company-cache
     t

     :annotation-function
     (lambda (candidate)
       (let* ((annotation (plist-get (gethash candidate lsp-bridge-completion-candidates) :annotation)))
         (setq lsp-bridge-completion-prefix (buffer-substring-no-properties bounds-start (point)))
         (when annotation
           (concat " " (propertize annotation 'face 'font-lock-doc-face)))))

     :company-kind
     (lambda (candidate)
       (when-let* ((kind (plist-get (gethash candidate lsp-bridge-completion-candidates) :kind)))
         (intern (downcase kind))))

     :company-deprecated
     (lambda (candidate)
       (seq-contains-p (plist-get (gethash candidate lsp-bridge-completion-candidates) :tags) 1))

     :exit-function
     (lambda (candidate status)
       (when (memq status '(finished exact))
         ;; Because lsp-bridge will push new candidates when company/lsp-bridge-ui completing.
         ;; We need extract newest candidates when insert, avoid insert old candidate content.
         (let* ((candidate-index (cl-find candidate candidates :test #'string=)))
           (with-current-buffer (if (minibufferp)
                                    (window-buffer (minibuffer-selected-window))
                                  (current-buffer))
             (cond
              ;; Don't expand candidate if the user enters all characters manually.
              ((and (member candidate candidates)
                    (eq this-command 'self-insert-command)))
              ;; Just insert candidate if it has expired.
              ((null candidate-index))
              (t
               (let* ((label (string-trim candidate)) ; we need trim candidate, `lsp-bridge-identify-annotation' will cause insert blank after `label' information
                      (candidate-info (gethash candidate lsp-bridge-completion-candidates))
                      (insert-text (plist-get candidate-info :insertText))
                      (insert-text-format (plist-get candidate-info :insertTextFormat))
                      (text-edit (plist-get candidate-info :textEdit))
                      (new-text (plist-get text-edit :newText))
                      (additionalTextEdits (if lsp-bridge-enable-auto-import
                                               (plist-get candidate-info :additionalTextEdits)
                                             nil))
                      (kind (plist-get candidate-info :kind))
                      (snippet-fn (and (or (eql insert-text-format 2) (string= kind "Snippet")) (lsp-bridge--snippet-expansion-fn)))
                      (completion-start-pos (lsp-bridge--lsp-position-to-point lsp-bridge-completion-position))
                      (delete-start-pos (if text-edit
                                            (lsp-bridge--lsp-position-to-point (plist-get (plist-get text-edit :range) :start))
                                          bounds-start))
                      (range-end-pos (if text-edit
                                         (lsp-bridge--lsp-position-to-point (plist-get (plist-get text-edit :range) :end))
                                       completion-start-pos))
                      (delete-end-pos (+ (point) (- range-end-pos completion-start-pos)))
                      (insert-candidate (or new-text insert-text label)))

                 ;; Insert candidate or expand snippet.
                 (delete-region delete-start-pos delete-end-pos)
                 (funcall (or snippet-fn #'insert) insert-candidate)

                 ;; Do `additionalTextEdits' if return auto-imprt information.
                 (when (cl-plusp (length additionalTextEdits))
                   (lsp-bridge--apply-text-edits additionalTextEdits))
                 ))))))))))

;; Copy from eglot
(defun lsp-bridge--snippet-expansion-fn ()
  "Compute a function to expand snippets.
Doubles as an indicator of snippet support."
  (and (boundp 'yas-minor-mode)
       (symbol-value 'yas-minor-mode)
       'yas-expand-snippet))

;; Copy from eglot
(defun lsp-bridge--apply-text-edits (edits &optional version)
  (let* ((change-group (prepare-change-group)))
    (mapc (pcase-lambda (`(,newText ,beg . ,end))
            (let ((source (current-buffer)))
              (with-temp-buffer
                (insert newText)
                (let ((temp (current-buffer)))
                  (with-current-buffer source
                    (save-excursion
                      (save-restriction
                        (narrow-to-region beg end)
                        (replace-buffer-contents temp))))))))
          (mapcar
           (lambda (edit) ()
             (let* ((range (plist-get edit :range))
                    (newText (plist-get edit :newText)))
               (cons newText
                     (cons
                      (lsp-bridge--lsp-position-to-point (plist-get range :start) 'markers)
                      (lsp-bridge--lsp-position-to-point (plist-get range :end) 'markers)
                      ))))
           (reverse edits)))
    (undo-amalgamate-change-group change-group)))

;; Copy from eglot
(defun lsp-bridge--lsp-position-to-point (pos-plist &optional marker)
  "Convert LSP position POS-PLIST to Emacs point.
If optional MARKER, return a marker instead"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (min most-positive-fixnum
                         (plist-get pos-plist :line)))
      (unless (eobp) ;; if line was excessive leave point at eob
        (let ((tab-width 1)
              (character (plist-get pos-plist :character)))
          (unless (wholenump character)
            (message
             "[LSP Bridge] Caution: LSP server sent invalid character position %s. Using 0 instead."
             character)
            (setq character 0))
          ;; We cannot use `move-to-column' here, because it moves to *visual*
          ;; columns, which can be different from LSP columns in case of
          ;; `whitespace-mode', `prettify-symbols-mode', etc.
          (goto-char (min (+ (line-beginning-position) character)
                          (line-end-position)))))
      (if marker (copy-marker (point-marker)) (point)))))

(defun lsp-bridge--point-position (pos)
  "Get position of POS."
  (save-excursion
    (goto-char pos)
    (lsp-bridge--position)))

(defun lsp-bridge--calculate-column ()
  "Calculate character offset of cursor in current line."
  (/ (- (length (encode-coding-region (line-beginning-position)
                                      (min (point) (point-max)) 'utf-16 t))
        2)
     2))

(defun lsp-bridge--position ()
  "Get position of cursor."
  (list :line (1- (line-number-at-pos)) :character (lsp-bridge--calculate-column)))

(defvar-local lsp-bridge--before-change-begin-pos nil)
(defvar-local lsp-bridge--before-change-end-pos nil)

(defun lsp-bridge-monitor-before-change (begin end)
  (setq lsp-bridge--before-change-begin-pos (lsp-bridge--point-position begin))
  (setq lsp-bridge--before-change-end-pos (lsp-bridge--point-position end)))

(defun lsp-bridge-monitor-after-change (begin end length)
  ;; Record last command to `lsp-bridge-last-change-command'.
  (setq lsp-bridge-last-change-command (format "%s" this-command))

  ;; Send change_file request.
  (when (lsp-bridge-epc-live-p lsp-bridge-epc-process)
    (let ((completion-frame corfu--frame))
      (setq-local lsp-bridge-current-tick (lsp-bridge--auto-tick))
      (lsp-bridge-call-async "change_file"
                             lsp-bridge-filepath
                             lsp-bridge--before-change-begin-pos
                             lsp-bridge--before-change-end-pos
                             length
                             (buffer-substring-no-properties begin end)
                             (lsp-bridge--position)
                             (lsp-bridge-char-before)
                             (buffer-substring-no-properties (line-beginning-position) (point))
                             (and (frame-live-p completion-frame)
                                  (frame-visible-p completion-frame))
                             ))))

(defun lsp-bridge-monitor-after-save ()
  (lsp-bridge-call-async "save_file" lsp-bridge-filepath))

(defalias 'lsp-bridge-find-define #'lsp-bridge-find-def)

(defvar-local lsp-bridge-jump-to-def-in-other-window nil)

(defun lsp-bridge-find-def ()
  (interactive)
  (setq-local lsp-bridge-jump-to-def-in-other-window nil)
  (lsp-bridge-call-async "find_define" lsp-bridge-filepath (lsp-bridge--position)))

(defun lsp-bridge-find-def-other-window ()
  (interactive)
  (setq-local lsp-bridge-jump-to-def-in-other-window t)
  (lsp-bridge-call-async "find_define" lsp-bridge-filepath (lsp-bridge--position)))

(defun lsp-bridge-return-from-def ()
  "Pop off lsp-bridge-mark-ring and jump to the top location."
  (interactive)
  ;; Pop entries that refer to non-existent buffers.
  (while (and lsp-bridge-mark-ring (not (marker-buffer (car lsp-bridge-mark-ring))))
    (setq lsp-bridge-mark-ring (cdr lsp-bridge-mark-ring)))
  (or lsp-bridge-mark-ring
      (error "[LSP-Bridge] No lsp-bridge mark set"))
  (let* ((this-buffer (current-buffer))
         (marker (pop lsp-bridge-mark-ring))
         (buffer (marker-buffer marker))
         (position (marker-position marker)))
    (set-buffer buffer)
    (or (and (>= position (point-min))
             (<= position (point-max)))
        (if widen-automatically
            (widen)
          (error "[LSP-Bridge] mark position is outside accessible part of buffer %s"
                 (buffer-name buffer))))
    (goto-char position)
    (unless (equal buffer this-buffer)
      (switch-to-buffer buffer))))

(defun lsp-bridge-find-impl ()
  (interactive)
  (setq-local lsp-bridge-jump-to-def-in-other-window nil)
  (lsp-bridge-call-async "find_implementation" lsp-bridge-filepath (lsp-bridge--position)))

(defun lsp-bridge-find-impl-other-window ()
  (interactive)
  (setq-local lsp-bridge-jump-to-def-in-other-window t)
  (lsp-bridge-call-async "find_implementation" lsp-bridge-filepath (lsp-bridge--position)))

(defun lsp-bridge-find-references ()
  (interactive)
  (lsp-bridge-call-async "find_references" lsp-bridge-filepath (lsp-bridge--position)))

(defun lsp-bridge-popup-references (references-content references-counter)
  (lsp-bridge-ref-popup references-content references-counter)
  (message "[LSP-Bridge] Found %s references" references-counter))

(defun lsp-bridge-rename ()
  (interactive)
  (lsp-bridge-call-async "prepare_rename" lsp-bridge-filepath (lsp-bridge--position))
  (let ((new-name (read-string "Rename to: " (thing-at-point 'symbol 'no-properties))))
    (lsp-bridge-call-async "rename" lsp-bridge-filepath (lsp-bridge--position) new-name)))

(defun lsp-bridge-rename-highlight (filepath bound-start bound-end)
  (lsp-bridge--with-file-buffer filepath
    (require 'pulse)
    (let ((pulse-iterations 1)
          (pulse-delay lsp-bridge-flash-line-delay))
      (pulse-momentary-highlight-region
       (lsp-bridge--lsp-position-to-point bound-start)
       (lsp-bridge--lsp-position-to-point bound-end)
       'lsp-bridge-font-lock-flash))))

(defun lsp-bridge-lookup-documentation ()
  (interactive)
  (lsp-bridge-call-async "hover" lsp-bridge-filepath (lsp-bridge--position)))

(defun lsp-bridge-show-signature-help-in-minibuffer ()
  (interactive)
  (lsp-bridge-call-async "signature_help" lsp-bridge-filepath (lsp-bridge--position)))

(defun lsp-bridge-rename-file (filepath edits)
  (find-file-noselect filepath)
  (lsp-bridge--with-file-buffer filepath
    (dolist (edit edits)
      (let* ((bound-start (nth 0 edit))
             (bound-end (nth 1 edit))
             (new-text (nth 2 edit))
             (replace-start-pos (lsp-bridge--lsp-position-to-point bound-start))
             (replace-end-pos (lsp-bridge--lsp-position-to-point bound-end)))
        (delete-region replace-start-pos replace-end-pos)
        (goto-char replace-start-pos)
        (insert new-text))))
  (setq lsp-bridge-prohibit-completion t))

(defun lsp-bridge--jump-to-def (filepath position)
  (interactive)
  ;; Record postion.
  (set-marker (mark-marker) (point) (current-buffer))
  (add-to-history 'lsp-bridge-mark-ring (copy-marker (mark-marker)) lsp-bridge-mark-ring-max t)

  ;; Jump to define.
  ;; Show define in other window if `lsp-bridge-jump-to-def-in-other-window' is non-nil.
  (if lsp-bridge-jump-to-def-in-other-window
      (find-file-other-window filepath)
    (find-file filepath))

  (goto-char (lsp-bridge--lsp-position-to-point position))
  (recenter)

  ;; Flash define line.
  (require 'pulse)
  (let ((pulse-iterations 1)
        (pulse-delay lsp-bridge-flash-line-delay))
    (pulse-momentary-highlight-one-line (point) 'lsp-bridge-font-lock-flash)))

(defun lsp-bridge-insert-common-prefix ()
  (interactive)
  (if lsp-bridge-mode
      (if (and (> (length (hash-table-keys lsp-bridge-completion-candidates)) 0)
               lsp-bridge-completion-prefix
               lsp-bridge-completion-common
               (not (string-equal lsp-bridge-completion-common ""))
               (not (string-equal lsp-bridge-completion-common lsp-bridge-completion-prefix)))
          (insert (substring lsp-bridge-completion-common (length lsp-bridge-completion-prefix)))
        (message "Not found common part to insert."))
    (message "Only works for lsp-bridge mode.")))

(defun lsp-bridge-popup-documentation-scroll-up (&optional arg)
  (interactive)
  (posframe-funcall lsp-bridge-lookup-doc-tooltip
                    #'scroll-up-command arg))

(defun lsp-bridge-popup-documentation-scroll-down (&optional arg)
  (interactive)
  (posframe-funcall lsp-bridge-lookup-doc-tooltip
                    #'scroll-down-command arg))

(defun lsp-bridge-frame-background-color ()
  (let* ((theme-mode (format "%s" (frame-parameter nil 'background-mode))))
    (if (string-equal theme-mode "dark") "#191a1b" "#f0f0f0")))

(defun lsp-bridge-popup-documentation (kind name value)
  (with-current-buffer (get-buffer-create lsp-bridge-lookup-doc-tooltip)
    (erase-buffer)
    (text-scale-set lsp-bridge-lookup-doc-tooltip-text-scale)
    (insert (propertize name 'face 'font-lock-function-name-face))
    (insert "\n\n")
    (setq-local markdown-fontify-code-blocks-natively t)
    (insert value)
    (if (fboundp 'gfm-view-mode)
        (let ((view-inhibit-help-message t))
          (gfm-view-mode))
      (gfm-mode))
    ;; avoid 'gfm-view-mode made buffer read only.
    (read-only-mode 0)
    (font-lock-ensure))
  (when (posframe-workable-p)
    (posframe-show lsp-bridge-lookup-doc-tooltip
                   :position (point)
                   :internal-border-width lsp-bridge-lookup-doc-tooltip-border-width
                   :background-color (lsp-bridge-frame-background-color)
                   :max-width lsp-bridge-lookup-doc-tooltip-max-width
                   :max-height lsp-bridge-lookup-doc-tooltip-max-height)))

(defun lsp-bridge-hide-doc-tooltip ()
  (posframe-hide lsp-bridge-lookup-doc-tooltip))

(defun lsp-bridge-hide-diagnostic-tooltip ()
  (posframe-hide lsp-bridge-diagnostic-tooltip))

(defun lsp-bridge-show-signature-help (help)
  (cond
   ;; Trim signature help length make sure `awesome-tray' won't wrap line display.
   ((ignore-errors (require 'awesome-tray))
    (message (substring help
                        0
                        (min (string-width help)
                             (- (awesome-tray-get-frame-width)
                                (string-width (awesome-tray-build-active-info))
                                )))))
   ;; Other minibuffer plugin similar `awesome-tray' welcome to send PR here. ;)
   (t
    (message help))))

(defvar lsp-bridge--last-buffer nil)

(defun lsp-bridge-monitor-window-buffer-change ()
  ;; Hide completion frame when buffer or window changed.
  (unless (eq (current-buffer)
              lsp-bridge--last-buffer)
    (lsp-bridge-hide-doc-tooltip)
    (lsp-bridge-hide-diagnostic-tooltip))

  (unless (or (minibufferp)
              (string-equal (buffer-name) "*Messages*"))
    (setq lsp-bridge--last-buffer (current-buffer))))

(add-hook 'post-command-hook 'lsp-bridge-monitor-window-buffer-change)

(defconst lsp-bridge--internal-hooks
  '((before-change-functions . lsp-bridge-monitor-before-change)
    (after-change-functions . lsp-bridge-monitor-after-change)
    (post-command-hook . lsp-bridge-monitor-post-command)
    (after-save-hook . lsp-bridge-monitor-after-save)
    (kill-buffer-hook . lsp-bridge-monitor-kill-buffer)
    (completion-at-point-functions . lsp-bridge-capf)))

(defvar lsp-bridge-mode-map (make-sparse-keymap))

(defcustom lsp-bridge-org-babel-lang-list '("clojure" "latex" "python")
  "A list of org babel languages in which source code block lsp-bridge will be enabled."
  :type 'list
  :group 'lsp-bridge)

(defcustom lsp-bridge-diagnostics-fetch-idle 1
  "The idle seconds to fetch diagnostics."
  :type 'integer
  :group 'lsp-bridge)

(defface lsp-bridge-diagnostics-error-face
  '((t (:underline (:style wave :color "Red1"))))
  "Face error diagnostic."
  :group 'lsp-bridge)

(defface lsp-bridge-diagnostics-warning-face
  '((t (:underline (:style wave :color "DarkOrange"))))
  "Face warning diagnostic."
  :group 'lsp-bridge)

(defface lsp-bridge-diagnostics-info-face
  '((t (:underline (:style wave :color "ForestGreen"))))
  "Face info diagnostic."
  :group 'lsp-bridge)

(defface lsp-bridge-diagnostics-hint-face
  '((t (:underline (:style wave :color "grey"))))
  "Face hint diagnostic."
  :group 'lsp-bridge)

(defcustom lsp-bridge-diagnostic-tooltip " *lsp-bridge-diagnostic*"
  "Buffer for display diagnostic information."
  :type 'string
  :group 'lsp-bridge)

(defcustom lsp-bridge-diagnostic-tooltip-border-width 20
  "The border width of lsp-bridge diagnostic tooltip, in pixels."
  :type 'integer
  :group 'lsp-bridge)

(defvar lsp-bridge-diagnostics-timer nil)

;;;###autoload
(define-minor-mode lsp-bridge-mode
  "LSP Bridge mode."
  :keymap lsp-bridge-mode-map
  :init-value nil
  (if lsp-bridge-mode
      (lsp-bridge--enable)
    (lsp-bridge--disable)))

(defun lsp-bridge--enable ()
  "Enable LSP Bridge mode."
  (cond
   ((not buffer-file-name)
    (message "[LSP-Bridge] cannot be enabled in non-file buffers.")
    (setq lsp-bridge-mode nil))
   (t
    ;; Disable backup file.
    ;; Please use my another plugin `https://github.com/manateelazycat/auto-save' and use git for file version management.
    (when lsp-bridge-disable-backup
      (setq make-backup-files nil)
      (setq auto-save-default nil)
      (setq create-lockfiles nil))

    ;; When user open buffer by `ido-find-file', lsp-bridge will throw `FileNotFoundError' error.
    ;; So we need save buffer to disk before enable `lsp-bridge-mode'.
    (unless (file-exists-p (buffer-file-name))
      (save-buffer))

    (dolist (hook lsp-bridge--internal-hooks)
      (add-hook (car hook) (cdr hook) nil t))

    (setq lsp-bridge-filepath (file-truename buffer-file-name))
    (setq lsp-bridge-last-position 0)

    (setq-local corfu-auto-prefix 0)
    (setq-local corfu-auto nil)

    (remove-hook 'post-command-hook #'corfu--auto-post-command 'local)

    (advice-add #'corfu--popup-hide :after #'lsp-bridge--completion-hide-advisor)

    (when (featurep 'lsp-bridge-icon)
      (add-to-list 'corfu-margin-formatters #'lsp-bridge-icon-margin-formatter))

    ;; Add fuzzy match.
    (when (functionp 'lsp-bridge-orderless-setup)
      (lsp-bridge-orderless-setup))

    ;; Flag `lsp-bridge-is-starting' make sure only call `lsp-bridge-start-process' once.
    (unless lsp-bridge-is-starting
      (lsp-bridge-start-process)))))

(defun lsp-bridge--disable ()
  "Disable LSP Bridge mode."
  (kill-local-variable 'corfu-auto-prefix)
  (kill-local-variable 'corfu-auto)

  (and corfu-auto (add-hook 'post-command-hook #'corfu--auto-post-command nil 'local))

  (advice-remove #'corfu--popup-hide #'lsp-bridge--completion-hide-advisor)

  (dolist (hook lsp-bridge--internal-hooks)
    (remove-hook (car hook) (cdr hook) t)))

(defun lsp-bridge-turn-off (filepath)
  (lsp-bridge--with-file-buffer filepath
    (lsp-bridge--disable)))

(defun lsp-bridge-diagnostics-fetch ()
  (when (and lsp-bridge-mode
             (process-live-p lsp-bridge-server)
             (buffer-file-name))
    (when (string-equal (buffer-file-name) lsp-bridge-filepath)
      (lsp-bridge-call-async "pull_diagnostics" lsp-bridge-filepath))))

(defun lsp-bridge-diagnostics-render (filepath diagnostics)
  (lsp-bridge--with-file-buffer filepath
    (when lsp-bridge-diagnostic-overlays
      (dolist (diagnostic-overlay lsp-bridge-diagnostic-overlays)
        (delete-overlay diagnostic-overlay)))

    (setq lsp-bridge-diagnostic-overlays nil)

    (dolist (diagnostic diagnostics)
      (let* ((diagnostic-start (lsp-bridge--lsp-position-to-point (plist-get (plist-get diagnostic :range) :start)))
             (diagnostic-end (lsp-bridge--lsp-position-to-point (plist-get (plist-get diagnostic :range) :end)))
             (overlay (if (eq diagnostic-start diagnostic-end)
                          ;; Adjust diagnostic end position if start and end is same position.
                          (make-overlay diagnostic-start (1+ diagnostic-start))
                        (make-overlay diagnostic-start diagnostic-end)))
             (severity (plist-get diagnostic :severity))
             (message (plist-get diagnostic :message))
             (overlay-face (cl-case severity
                             (1 'lsp-bridge-diagnostics-error-face)
                             (2 'lsp-bridge-diagnostics-warning-face)
                             (3 'lsp-bridge-diagnostics-info-face)
                             (4 'lsp-bridge-diagnostics-hint-face))))
        (overlay-put overlay 'face overlay-face)
        (overlay-put overlay 'help-echo message)
        (push  overlay lsp-bridge-diagnostic-overlays)))
    (setq lsp-bridge-diagnostic-overlays (reverse lsp-bridge-diagnostic-overlays))))

(defun lsp-bridge-show-diagnostic-tooltip (diagnostic-overlay)
  (let* ((diagnostic-info (overlay-get diagnostic-overlay 'help-echo))
         (foreground-color (plist-get (face-attribute (overlay-get diagnostic-overlay 'face) :underline) :color)))
    (goto-char (overlay-start diagnostic-overlay))

    (with-current-buffer (get-buffer-create lsp-bridge-diagnostic-tooltip)
      (erase-buffer)
      (insert diagnostic-info))

    (when (posframe-workable-p)
      ;; Perform redisplay make sure posframe can poup to
      (redisplay 'force)
      (sleep-for 0.01)
      (posframe-show lsp-bridge-diagnostic-tooltip
                     :position (point)
                     :internal-border-width lsp-bridge-diagnostic-tooltip-border-width
                     :background-color (lsp-bridge-frame-background-color)
                     :foreground-color foreground-color
                     ))))

(defun lsp-bridge-jump-to-next-diagnostic ()
  (interactive)
  (if (zerop (length lsp-bridge-diagnostic-overlays))
      (message "[LSP-Bridge] No diagnostics.")
    (if-let ((diagnostic-overlay (cl-find-if
                                  (lambda (overlay) (< (point) (overlay-start overlay)))
                                  lsp-bridge-diagnostic-overlays)))
        (lsp-bridge-show-diagnostic-tooltip diagnostic-overlay)
      (message "[LSP-Bridge] Reach last diagnostic."))))

(defun lsp-bridge-jump-to-prev-diagnostic ()
  (interactive)
  (if (zerop (length lsp-bridge-diagnostic-overlays))
      (message "[LSP-Bridge] No diagnostics."))
  (if-let ((diagnostic-overlay (cl-find-if
                                (lambda (overlay) (> (point) (overlay-end overlay)))
                                (reverse lsp-bridge-diagnostic-overlays))))
      (lsp-bridge-show-diagnostic-tooltip diagnostic-overlay)
    (message "[LSP-Bridge] Reach first diagnostic.")))

;;;###autoload
(defun global-lsp-bridge-mode ()
  (interactive)

  (setq corfu-auto t)

  (dolist (hook lsp-bridge-default-mode-hooks)
    (add-hook hook (lambda ()
                     (corfu-mode 1)
                     (lsp-bridge-mode 1)
                     )))

  (setq lsp-bridge-diagnostics-timer
        (run-with-idle-timer lsp-bridge-diagnostics-fetch-idle t #'lsp-bridge-diagnostics-fetch)))

(with-eval-after-load 'evil
  (evil-add-command-properties #'lsp-bridge-find-def :jump t)
  (evil-add-command-properties #'lsp-bridge-find-references :jump t)
  (evil-add-command-properties #'lsp-bridge-find-impl :jump t))

(defun lsp-bridge--rename-file-advisor (orig-fun &optional arg &rest args)
  (when lsp-bridge-mode
    (let ((new-name (expand-file-name (nth 0 args))))
      (lsp-bridge-call-async "rename_file" lsp-bridge-filepath new-name)
      (lsp-bridge-call-async "close_file" lsp-bridge-filepath)
      (set-visited-file-name new-name t t)
      (setq lsp-bridge-filepath new-name)))
  (apply orig-fun arg args))
(advice-add #'rename-file :around #'lsp-bridge--rename-file-advisor)

(defun lsp-bridge--completion-hide-advisor (&rest args)
  (when lsp-bridge-mode
    (lsp-bridge-call-async "completion_hide" lsp-bridge-filepath)))

;; https://tecosaur.github.io/emacs-config/config.html#lsp-support-src
(cl-defmacro lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (cl-check-type lang stringp)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (let ((file-name (->> info caddr (alist-get :file))))
           (unless file-name
             (setq file-name (make-temp-file "babel-lsp-")))
           (setq buffer-file-name file-name)
           (lsp-bridge-mode 1)))
       (put ',intern-pre 'function-documentation
            (format "Enable lsp-bridge-mode in the buffer of org source block (%s)."
                    (upcase ,lang)))
       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))

(with-eval-after-load 'org
  (dolist (lang lsp-bridge-org-babel-lang-list)
    (eval `(lsp-org-babel-enable ,lang))))

(provide 'lsp-bridge)

;;; lsp-bridge.el ends here
