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
(require 'corfu)
(require 'corfu-info)
(require 'corfu-history)
(require 'posframe)
(require 'markdown-mode)

;; Add completion history.
(corfu-history-mode t)

(defgroup lsp-bridge nil
  "LSP-Bridge group."
  :group 'applications)

(defcustom lsp-bridge-flash-line-delay .3
  "How many seconds to flash `lsp-bridge-font-lock-flash' after navigation.

Setting this to nil or 0 will turn off the indicator."
  :type 'number
  :group 'lsp-bridge)

(defcustom lsp-bridge-completion-stop-commands '(corfu-complete corfu-insert)
  "If last command is match this option, stop popup completion ui."
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

(defcustom lsp-bridge-enable-auto-import t
  "Whether to enable auto-import."
  :type 'boolean
  :group 'lsp-bridge)

(defface lsp-bridge-font-lock-flash
  '((t (:inherit highlight)))
  "Face to flash the current line."
  :group 'lsp-bridge)

(defvar lsp-bridge-last-change-command nil)
(defvar lsp-bridge-last-change-tick nil)

(defvar lsp-bridge-server nil
  "The LSP-Bridge Server.")

(defvar lsp-bridge-python-file (expand-file-name "lsp-bridge.py" (file-name-directory load-file-name)))

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
  "The Python interpreter used to run lsp-bridge.py."
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
    (rust-mode . "rust-analyzer")
    (elixir-mode . "elixirLS")
    (go-mode . "gopls")
    (haskell-mode . "hls")
    (lua-mode . "sumneko")
    (dart-mode . "dart-analysis-server")
    (scala-mode . "metals")
    ((js2-mode js-mode rjsx-mode) . "javascript")
    ((typescript-mode typescript-tsx-mode) . "typescript")
    (tuareg-mode . "ocamllsp")
    (erlang-mode . "erlang-ls")
    ((latex-mode Tex-latex-mode texmode context-mode texinfo-mode bibtex-mode) . "texlab")
    ((clojure-mode clojurec-mode clojurescript-mode clojurex-mode) . "clojure-lsp")
    ((sh-mode) . "bash-language-server")
    )
  "The lang server rule for file mode."
  :type 'cons)

(defvar lsp-bridge-get-lang-server-by-project nil
  "Get lang server with project path and file path.")

(defmacro lsp-bridge--with-file-buffer (filepath &rest body)
  "Evaluate BODY in buffer with FILEPATH."
  (declare (indent 1))
  `(let ((buffer (get-file-buffer ,filepath)))
     (when buffer
       (with-current-buffer buffer
         ,@body))))

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
(defvar-local lsp-bridge-completion-items nil)
(defvar-local lsp-bridge-completion-insert-texts nil)
(defvar-local lsp-bridge-completion-prefix nil)
(defvar-local lsp-bridge-completion-common nil)
(defvar-local lsp-bridge-filepath "")

(defun lsp-bridge-char-before ()
  (let ((prev-char (char-before)))
    (if prev-char (char-to-string prev-char) "")))

(defun lsp-bridge-monitor-post-command ()
  (when (lsp-bridge-epc-live-p lsp-bridge-epc-process)
    (unless (equal (point) lsp-bridge-last-position)
      (lsp-bridge-call-async "change_cursor" lsp-bridge-filepath)
      (setq-local lsp-bridge-last-position (point))))

  ;; Hide hover tooltip.
  (lsp-bridge-hide-doc-tooltip))

(defun lsp-bridge-monitor-kill-buffer ()
  (when (lsp-bridge-epc-live-p lsp-bridge-epc-process)
    (lsp-bridge-call-async "close_file" lsp-bridge-filepath)))

(defun lsp-bridge-is-empty-list (list)
  (or (and (eq (length list) 1)
           (string-empty-p (format "%s" (car list))))
      (and (eq (length list) 0))))

(defun lsp-bridge-record-completion-items (filepath prefix common items)
  ;; (message "*** '%s' '%s' '%s'" prefix common items)

  (lsp-bridge--with-file-buffer filepath
    ;; Save completion items.
    (setq-local lsp-bridge-completion-items items)
    (setq-local lsp-bridge-completion-insert-texts (make-hash-table :test 'equal))
    (setq-local lsp-bridge-completion-prefix prefix)
    (setq-local lsp-bridge-completion-common common)

    ;; We build (label . insertText) pair for replace label with insertText when call `exit-function'.
    (dolist (item items)
      (puthash (plist-get item :label) (plist-get item :insertText) lsp-bridge-completion-insert-texts))

    ;; Try popup completion frame.
    (unless (or
             ;; Don't popup completion frame if completion items is empty.
             (lsp-bridge-is-empty-list items)
             ;; If last command is match `lsp-bridge-completion-stop-commands'
             (member lsp-bridge-last-change-command lsp-bridge-completion-stop-commands))

      ;; Try to popup completion frame.
      (when (and (not (lsp-bridge-is-blank-before-cursor-p prefix)) ;hide completion frame if only blank before cursor
                 (not (lsp-bridge-is-at-sentence-ending-p))) ;hide completion if cursor after special chars

        ;; Popup completion frame.
        (corfu--auto-complete lsp-bridge-last-change-tick)
        ))))

(defun lsp-bridge-is-at-sentence-ending-p ()
  (member (char-to-string (char-before)) (list ":" ";" ")" "," "\"")))

(defun lsp-bridge-is-blank-before-cursor-p (prefix)
  (and (not (split-string (buffer-substring-no-properties (line-beginning-position) (point))))
       (string-match "\\s-*" prefix)
       ))

(defun lsp-bridge-capf ()
  "Capf function similar to eglot's 'eglot-completion-at-point'."
  (let* ((candidates
          (mapcar
           (lambda (item)
             (let* ((candidate-label (plist-get item :label)))
               (unless (zerop (length candidate-label))
                 (put-text-property 0 1 'lsp-bridge--lsp-item item candidate-label))
               candidate-label))
           lsp-bridge-completion-items))
         (bounds (bounds-of-thing-at-point 'symbol)))
    (list
     (or (car bounds) (point))
     (or (cdr bounds) (point))
     candidates
     :annotation-function
     (lambda (candidate)
       "Extract annotation from CANDIDATE."
       (let* ((item (get-text-property 0 'lsp-bridge--lsp-item candidate))
              (annotation (plist-get item :annotation)))
         (when annotation
           (concat " "
                   (propertize annotation
                               'face 'font-lock-doc-face)))))
     :company-kind
     (lambda (candidate)
       "Set icon here"
       (when-let* ((item (get-text-property 0 'lsp-bridge--lsp-item candidate))
                   (kind (plist-get item :kind)))
         (intern (downcase kind))))
     :exit-function
     (lambda (candidate status)
       (when (memq status '(finished exact))
         (let ((label candidate)
               (insert-text (gethash candidate lsp-bridge-completion-insert-texts)))
           ;; Remove candidate label and insert candidate insertText
           (delete-region (- (point) (length label)) (point))
           (insert insert-text)

           ;; Do auto-imprt action.
           (with-current-buffer
               (if (minibufferp) (window-buffer (minibuffer-selected-window))
                 (current-buffer))
             ;; When selecting from the *Completions*
             ;; buffer, `candidate' won't have any properties.
             ;; A lookup should fix that (github#148)
             (let* ((item (get-text-property 0 'lsp-bridge--lsp-item (cl-find candidate candidates :test #'string=)))
                    (additionalTextEdits (if lsp-bridge-enable-auto-import (plist-get item :additionalTextEdits) nil)))
               (when (cl-plusp (length additionalTextEdits))
                 (lsp-bridge--apply-text-edits additionalTextEdits))))
           ))))))

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
               (cons newText (lsp-bridge--range-region range 'markers))))
           (reverse edits)))
    (undo-amalgamate-change-group change-group)))

;; Copy from eglot
(defun lsp-bridge--range-region (range &optional markers)
  "Return region (BEG . END) that represents LSP RANGE.
If optional MARKERS, make markers."
  (let* ((beg (lsp-bridge--lsp-position-to-point (plist-get range :start) markers))
         (end (lsp-bridge--lsp-position-to-point (plist-get range :end) markers)))
    (cons beg end)))

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
              (col (plist-get pos-plist :character)))
          (unless (wholenump col)
            (message
             "[LSP Bridge] Caution: LSP server sent invalid character position %s. Using 0 instead."
             col)
            (setq col 0))
          (lsp--bridge-move-to-column col)))
      (if marker (copy-marker (point-marker)) (point)))))

;; Copy from eglot
(defun lsp--bridge-move-to-column (column)
  "Move to COLUMN without closely following the LSP spec."
  ;; We cannot use `move-to-column' here, because it moves to *visual*
  ;; columns, which can be different from LSP columns in case of
  ;; `whitespace-mode', `prettify-symbols-mode', etc.  (github#296,
  ;; github#297)
  (goto-char (min (+ (line-beginning-position) column)
                  (line-end-position))))

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
  (setq lsp-bridge-last-change-command this-command)
  (setq lsp-bridge-last-change-tick (corfu--auto-tick))

  ;; Send change_file request.
  (when (lsp-bridge-epc-live-p lsp-bridge-epc-process)
    (lsp-bridge-call-async "change_file"
                           lsp-bridge-filepath
                           lsp-bridge--before-change-begin-pos
                           lsp-bridge--before-change-end-pos
                           length
                           (buffer-substring-no-properties begin end)
                           (lsp-bridge--position)
                           (lsp-bridge-char-before)
                           (buffer-substring-no-properties (line-beginning-position) (point)))))

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
  (let ((new-name (read-string "Rename to: ")))
    (lsp-bridge-call-async "rename" lsp-bridge-filepath (lsp-bridge--position) new-name)))

(defun lsp-bridge-rename-highlight (filepath line bound-start bound-end)
  (lsp-bridge--with-file-buffer filepath
    (let* ((highlight-line (1+ line))
           (start-pos (lsp-bridge-get-pos buffer highlight-line bound-start))
           (end-pos (lsp-bridge-get-pos buffer highlight-line bound-end)))
      (require 'pulse)
      (let ((pulse-iterations 1)
            (pulse-delay lsp-bridge-flash-line-delay))
        (pulse-momentary-highlight-region start-pos end-pos 'lsp-bridge-font-lock-flash)))))

(defun lsp-bridge-lookup-documentation ()
  (interactive)
  (lsp-bridge-call-async "hover" lsp-bridge-filepath (lsp-bridge--position)))

(defun lsp-bridge-get-pos (buf line column)
  (with-current-buffer buf
    (save-excursion
      (goto-line line)
      (move-to-column column)
      (point))))

(defun lsp-bridge-rename-finish (rename-files counter)
  (save-excursion
    (dolist (filepath rename-files)
      (lsp-bridge--with-file-buffer filepath
        (revert-buffer :ignore-auto :noconfirm))))

  (message "[LSP-Bridge] Rename %s places in %s files." counter (length rename-files)))

(defun lsp-bridge--jump-to-def (filepath row column)
  (interactive)
  ;; Record postion.
  (set-marker (mark-marker) (point) (current-buffer))
  (add-to-history 'lsp-bridge-mark-ring (copy-marker (mark-marker)) lsp-bridge-mark-ring-max t)

  ;; Jump to define.
  ;; Show define in other window if `lsp-bridge-jump-to-def-in-other-window' is non-nil.
  (if lsp-bridge-jump-to-def-in-other-window
      (find-file-other-window filepath)
    (find-file filepath))

  (goto-line (1+ row))
  (move-to-column column)

  ;; Flash define line.
  (require 'pulse)
  (let ((pulse-iterations 1)
        (pulse-delay lsp-bridge-flash-line-delay))
    (pulse-momentary-highlight-one-line (point) 'lsp-bridge-font-lock-flash)))

(defun lsp-bridge-popup-documentation (kind name value)
  (let* ((theme-mode (format "%s" (frame-parameter nil 'background-mode)))
         (background-color (if (string-equal theme-mode "dark")
                               "#191a1b"
                             "#f0f0f0")))
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
      (font-lock-ensure))
    (when (posframe-workable-p)
      (posframe-show lsp-bridge-lookup-doc-tooltip
                     :position (point)
                     :internal-border-width lsp-bridge-lookup-doc-tooltip-border-width
                     :background-color background-color
                     :max-width lsp-bridge-lookup-doc-tooltip-max-width
                     :max-height lsp-bridge-lookup-doc-tooltip-max-height)
      (unwind-protect
          (push (read-event) unread-command-events)
        (progn
          (posframe-delete lsp-bridge-lookup-doc-tooltip)
          (other-frame 0))))))

(defun lsp-bridge-hide-doc-tooltip ()
  (posframe-hide lsp-bridge-lookup-doc-tooltip))

(defvar lsp-bridge--last-buffer nil)

(defun lsp-bridge-monitor-window-buffer-change ()
  ;; Hide completion frame when buffer or window changed.
  (unless (eq (current-buffer)
              lsp-bridge--last-buffer)
    (lsp-bridge-hide-doc-tooltip))

  (unless (or (minibufferp)
              (string-equal (buffer-name) "*Messages*"))
    (setq lsp-bridge--last-buffer (current-buffer))))

;;;###autoload
(add-hook 'post-command-hook 'lsp-bridge-monitor-window-buffer-change)

(defconst lsp-bridge--internal-hooks
  '((before-change-functions . lsp-bridge-monitor-before-change)
    (after-change-functions . lsp-bridge-monitor-after-change)
    (post-command-hook . lsp-bridge-monitor-post-command)
    (after-save-hook . lsp-bridge-monitor-after-save)
    (kill-buffer-hook . lsp-bridge-monitor-kill-buffer)
    (completion-at-point-functions . lsp-bridge-capf)))

(defvar lsp-bridge-mode-map (make-sparse-keymap))

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
    ;; When user open buffer by `ido-find-file', lsp-bridge will throw `FileNotFoundError' error.
    ;; So we need save buffer to disk before enable `lsp-bridge-mode'.
    (unless (file-exists-p (buffer-file-name))
      (save-buffer))

    (dolist (hook lsp-bridge--internal-hooks)
      (add-hook (car hook) (cdr hook) nil t))

    (setq lsp-bridge-filepath buffer-file-name)
    (setq lsp-bridge-last-position 0)
    (setq-local corfu-auto-prefix 0)

    ;; Add fuzzy match.
    (when (functionp 'lsp-bridge-orderless-setup)
      (lsp-bridge-orderless-setup))

    ;; Flag `lsp-bridge-is-starting' make sure only call `lsp-bridge-start-process' once.
    (unless lsp-bridge-is-starting
      (lsp-bridge-start-process)
      (message "[LSP-Bridge] Start lsp server")))))

(defun lsp-bridge--disable ()
  "Disable LSP Bridge mode."
  (dolist (hook lsp-bridge--internal-hooks)
    (remove-hook (car hook) (cdr hook) t)))

(provide 'lsp-bridge)

;;; lsp-bridge.el ends here
