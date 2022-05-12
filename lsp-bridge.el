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

(defcustom lsp-bridge-completion-stop-commands '(corfu-complete)
  "If last command is match this option, stop popup completion ui."
  :type 'cons
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
               (lsp-bridge-epc-define-method mngr 'get-lang-server 'lsp-bridge--get-lang-server-by-file-func)
               ))))
    (if lsp-bridge-server
        (setq lsp-bridge-server-port (process-contact lsp-bridge-server :service))
      (error "[LSP-Bridge] lsp-bridge-server failed to start")))
  lsp-bridge-server)

(when noninteractive
  ;; Start "event loop".
  (cl-loop repeat 600
           do (sleep-for 0.1)))

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

(defcustom lsp-bridge-lang-server-list
  '(
    ((c-mode c++-mode) . "clangd")
    (java-mode . "jdtls")
    (python-mode . "pyright")
    (ruby-mode . "solargraph")
    (rust-mode . "rust-analyzer")
    (elixir-mode . "elixirLS")
    (go-mode . "gopls")
    (haskell-mode . "hls")
    (dart-mode . "dart_analysis_server")
    (scala-mode . "metals")
    ((js2-mode js-mode rjsx-mode) . "javascript")
    ((typescript-mode typescript-tsx-mode) . "typescript")
    (tuareg-mode . "ocamllsp")
    (erlang-mode . "erlang_ls")
    ((latex-mode Tex-latex-mode texmode context-mode texinfo-mode bibtex-mode) . "texlab")
    ((clojure-mode clojurec-mode clojurescript-mode clojurex-mode) . "clojure-lsp")
    ((sh-mode) . "bash-language-server")
    )
  "The lang server rule for file mode."
  :type 'cons)

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
(defvar-local lsp-bridge-completion-prefix nil)
(defvar-local lsp-bridge-completion-common nil)
(defvar-local lsp-bridge-filepath "")

(defmacro lsp-bridge--with-file-buffer (filepath &rest body)
  "Evaluate BODY in buffer with FILEPATH."
  (declare (indent 1))
  `(let ((buffer (get-file-buffer ,filepath)))
     (when buffer
       (with-current-buffer buffer
         ,@body))))


(defun lsp-bridge-get-lang-server ()
  "Get lang server for current buffer."
  (let ((langserver-info (cl-find-if (lambda (pair)
                                       (let ((mode (car pair)))
                                         (if (symbolp mode)
                                             (eq major-mode mode)
                                           (member major-mode mode)))) lsp-bridge-lang-server-list)))
    (if langserver-info
        (cdr langserver-info)
      nil)))

(defun lsp-bridge--get-lang-server-by-file-func (filepath)
  "Get lang server for FILEPATH."
  (lsp-bridge--with-file-buffer filepath
    (lsp-bridge-get-lang-server)))

(defun lsp-bridge-char-before ()
  (let ((prev-char (char-before)))
    (if prev-char (char-to-string prev-char) "")))

(defun lsp-bridge-monitor-post-command ()
  (when (lsp-bridge-epc-live-p lsp-bridge-epc-process)
    (unless (equal (point) lsp-bridge-last-position)
      (lsp-bridge-call-async "change_cursor" lsp-bridge-filepath)
      (setq-local lsp-bridge-last-position (point)))))

(defun lsp-bridge-monitor-kill-buffer ()
  (when (lsp-bridge-epc-live-p lsp-bridge-epc-process)
    (lsp-bridge-call-async "close_file" lsp-bridge-filepath)))

(defun lsp-bridge-is-empty-list (list)
  (and (eq (length list) 1)
       (string-empty-p (format "%s" (car list)))))

(defun lsp-bridge-record-completion-items (filepath prefix common items kinds annotions)
  ;; (message "*** '%s' '%s' '%s'" prefix common items)

  (lsp-bridge--with-file-buffer filepath
    ;; Save completion items.
    (setq-local lsp-bridge-completion-items items)
    (setq-local lsp-bridge-completion-prefix prefix)
    (setq-local lsp-bridge-completion-common common)

    ;; Try popup completion frame.
    (unless (or
             ;; Don't popup completion frame if completion items is empty.
             (lsp-bridge-is-empty-list items)
             ;; If last command is match `lsp-bridge-completion-stop-commands'
             (member lsp-bridge-last-change-command lsp-bridge-completion-stop-commands))
      ;; Add kind and annotion information in completion item text.
      (when (eq (length items) (length kinds))
        (cl-mapcar (lambda (item value)
                     (put-text-property 0 1 'kind value item)) items kinds))
      (when (eq (length items) (length annotions))
        (cl-mapcar (lambda (item value)
                     (put-text-property 0 1 'annotation value item)) items annotions))

      ;; Try to popup completion frame.
      (when (and (not (lsp-bridge-is-blank-before-cursor-p prefix)) ;hide completion frame if only blank before cursor
                 (not (lsp-bridge-is-at-sentence-ending-p))) ;hide completion if cursor after special chars

        ;; Popup completion frame.
        (corfu--auto-complete lsp-bridge-last-change-tick)
        ))))

(defun lsp-bridge-is-at-sentence-ending-p ()
  (member (char-to-string (char-before)) (list ":" ";" ")")))

(defun lsp-bridge-is-blank-before-cursor-p (prefix)
  (and (not (split-string (buffer-substring-no-properties (line-beginning-position) (point))))
       (string-equal prefix "")))

(defun lsp-bridge-capf ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (list (or (car bounds) (point))
          (or (cdr bounds) (point))
          lsp-bridge-completion-items
          :exclusive 'no
          :company-kind
          (lambda (candidate)
            "Set icon here"
            (let ((kind (get-text-property 0 'kind candidate)))
              (and kind
                   (intern (downcase kind)))))
          :annotation-function
          (lambda (candidate)
            "Extract annotation from CANDIDATE."
            (concat "   " (get-text-property 0 'annotation candidate))))))

(defun lsp-bridge-point-row (pos)
  (save-excursion
    (goto-char pos)
    (line-number-at-pos)))

(defun lsp-bridge-point-character (pos)
  (save-excursion
    (goto-char pos)
    (- (point) (line-beginning-position))))

(defvar-local lsp-bridge--before-change-begin-pos 0)
(defvar-local lsp-bridge--before-change-end-pos 0)
(defvar-local lsp-bridge--before-change-end-pos-row 0)
(defvar-local lsp-bridge--before-change-end-pos-character 0)

(defun lsp-bridge-monitor-before-change (begin end)
  (setq lsp-bridge--before-change-begin-pos begin)
  (setq lsp-bridge--before-change-end-pos end)
  (setq lsp-bridge--before-change-end-pos-row (lsp-bridge-point-row end))
  (setq lsp-bridge--before-change-end-pos-character (lsp-bridge-point-character end)))

(defun lsp-bridge-monitor-after-change (begin end length)
  ;; Record last command to `lsp-bridge-last-change-command'.
  (setq lsp-bridge-last-change-command this-command)
  (setq lsp-bridge-last-change-tick (corfu--auto-tick))

  ;; Send change_file request.
  (when (lsp-bridge-epc-live-p lsp-bridge-epc-process)
    (cond
     ;; Add operation.
     ((zerop length)
      (lsp-bridge-call-async "change_file"
                             lsp-bridge-filepath
                             (lsp-bridge-point-row begin) (lsp-bridge-point-character begin)
                             (lsp-bridge-point-row begin) (lsp-bridge-point-character begin)
                             0
                             (buffer-substring-no-properties begin end)
                             (line-number-at-pos) (current-column)
                             (lsp-bridge-char-before)
                             (buffer-substring-no-properties (line-beginning-position) (point))))
     ;; Delete operation.
     ((eq begin end)
      (lsp-bridge-call-async "change_file"
                             lsp-bridge-filepath
                             (lsp-bridge-point-row begin) (lsp-bridge-point-character begin)
                             lsp-bridge--before-change-end-pos-row lsp-bridge--before-change-end-pos-character
                             length
                             ""
                             (line-number-at-pos) (current-column)
                             (lsp-bridge-char-before)
                             (buffer-substring-no-properties (line-beginning-position) (point))))
     ;; Change operation.
     (t
      (lsp-bridge-call-async "change_file"
                             lsp-bridge-filepath
                             (lsp-bridge-point-row begin) (lsp-bridge-point-character begin)
                             lsp-bridge--before-change-end-pos-row lsp-bridge--before-change-end-pos-character
                             length
                             (buffer-substring-no-properties begin end)
                             (line-number-at-pos) (current-column)
                             (lsp-bridge-char-before)
                             (buffer-substring-no-properties (line-beginning-position) (point)))))))

(defalias 'lsp-bridge-find-define #'lsp-bridge-find-def)

(defun lsp-bridge-find-def ()
  (interactive)
  (lsp-bridge-call-async "find_define" lsp-bridge-filepath (line-number-at-pos) (current-column)))

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

(defun lsp-bridge-find-references ()
  (interactive)
  (lsp-bridge-call-async "find_references" lsp-bridge-filepath (line-number-at-pos) (current-column)))

(defun lsp-bridge-popup-references (references-content references-counter)
  (lsp-bridge-ref-popup references-content references-counter)
  (message "Found %s references" references-counter))

(defun lsp-bridge-rename ()
  (interactive)
  (lsp-bridge-call-async "prepare_rename" lsp-bridge-filepath (line-number-at-pos) (current-column))
  (let ((new-name (read-string "Rename to: ")))
    (lsp-bridge-call-async "rename" lsp-bridge-filepath (line-number-at-pos) (current-column) new-name)))

(defun lsp-bridge-rename-highlight (filepath line bound-start bound-end)
  (lsp-bridge--with-file-buffer filepath
    (let* ((highlight-line (1+ line))
           (start-pos (lsp-bridge-get-pos buffer highlight-line bound-start))
           (end-pos (lsp-bridge-get-pos buffer highlight-line bound-end)))
      (require 'pulse)
      (let ((pulse-iterations 1)
            (pulse-delay lsp-bridge-flash-line-delay))
        (pulse-momentary-highlight-region start-pos end-pos 'lsp-bridge-font-lock-flash)))))

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
  (message "jumping to def")
  (set-marker (mark-marker) (point) (current-buffer))
  (add-to-history 'lsp-bridge-mark-ring (copy-marker (mark-marker)) lsp-bridge-mark-ring-max t)
  (find-file filepath)
  (goto-line (1+ row))
  (move-to-column column)
  (require 'pulse)
  (let ((pulse-iterations 1)
        (pulse-delay lsp-bridge-flash-line-delay))
    (pulse-momentary-highlight-one-line (point) 'lsp-bridge-font-lock-flash)))

(defconst lsp-bridge--internal-hooks
  '((before-change-functions . lsp-bridge-monitor-before-change)
    (after-change-functions . lsp-bridge-monitor-after-change)
    (post-command-hook . lsp-bridge-monitor-post-command)
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
   ((not (lsp-bridge-get-lang-server))
    (message "[LSP-Bridge] doesn't support the language of the current buffer.")
    (setq lsp-bridge-mode nil))
   (t
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
      (message "[LSP-Bridge] Configuring LSP \"%s\" server for %s" (lsp-bridge-get-lang-server) (file-name-nondirectory lsp-bridge-filepath))))))

(defun lsp-bridge--disable ()
  "Disable LSP Bridge mode."
  (dolist (hook lsp-bridge--internal-hooks)
    (remove-hook (car hook) (cdr hook) t)))

(provide 'lsp-bridge)

;;; lsp-bridge.el ends here
