;;; lsp-bridge.el --- LSP bridge  -*- lexical-binding: t; -*-

;; Filename: lsp-bridge.el
;; Description: LSP bridge
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.5
;; Last-Updated: Sun Nov 21 04:35:02 2022 (-0500)
;;           By: Mingde (Matthew) Zeng
;; URL: https://github.com/manateelazycat/lsp-bridge
;; Keywords:
;; Compatibility: emacs-version >= 27
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
(require 'lbcf)

(require 'lsp-bridge-epc)

(defgroup lsp-bridge nil
  "LSPBRIDGE group."
  :group 'applications)

(defcustom lsp-bridge-flash-line-delay .3
  "How many seconds to flash `lsp-bridge-font-lock-flash' after navigation.

Setting this to nil or 0 will turn off the indicator."
  :type 'number
  :group 'lsp-bridge)

(defface lsp-bridge-font-lock-flash
  '((t (:inherit highlight)))
  "Face to flash the current line."
  :group 'lsp-bridge)

(defvar lsp-bridge-server nil
  "The LSPBRIDGE Server.")

(defvar lsp-bridge-python-file (expand-file-name "lsp-bridge.py" (file-name-directory load-file-name)))

(defvar lsp-bridge-server-port nil)

(defun lsp-bridge--start-epc-server ()
  "Function to start the EPC server."
  (unless (process-live-p lsp-bridge-server)
    (setq lsp-bridge-server
          (lsp-bridge-epc-server-start
           (lambda (mngr)
             (let ((mngr mngr))
               (lsp-bridge-epc-define-method mngr 'eval-in-emacs 'eval-in-emacs-func)
               (lsp-bridge-epc-define-method mngr 'get-emacs-var 'lsp-bridge--get-emacs-var-func)
               (lsp-bridge-epc-define-method mngr 'get-emacs-vars 'lsp-bridge--get-emacs-vars-func)
               (lsp-bridge-epc-define-method mngr 'get-lang-server 'lsp-bridge--get-lang-server-by-file-func)
               ))))
    (if lsp-bridge-server
        (setq lsp-bridge-server-port (process-contact lsp-bridge-server :service))
      (error "[LSPBRIDGE] lsp-bridge-server failed to start")))
  lsp-bridge-server)

(when noninteractive
  ;; Start "event loop".
  (cl-loop repeat 600
           do (sleep-for 0.1)))

(defun eval-in-emacs-func (&rest args)
  (apply (read (car args))
         (mapcar
          (lambda (arg)
            (let ((arg (lsp-bridge--decode-string arg)))
              (cond ((string-prefix-p "'" arg) ;; single quote
                     (read (substring arg 1)))
                    ((and (string-prefix-p "(" arg)
                          (string-suffix-p ")" arg)) ;; list
                     (split-string (substring arg 1 -1) " "))
                    (t arg))))
          (cdr args))))

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
  "Name of LSPBRIDGE buffer."
  :type 'string)

(defcustom lsp-bridge-python-command (if (memq system-type '(cygwin windows-nt ms-dos)) "python.exe" "python3")
  "The Python interpreter used to run lsp-bridge.py."
  :type 'string)

(defcustom lsp-bridge-enable-debug nil
  "If you got segfault error, please turn this option.
Then LSPBRIDGE will start by gdb, please send new issue with `*lsp-bridge*' buffer content when next crash."
  :type 'boolean)

(defcustom lsp-bridge-enable-log nil
  "Enable this option to print log message in `*lsp-bridge*' buffer, default only print message header."
  :type 'boolean)

(defcustom lsp-bridge-lang-server-list
  '(
    (c-mode . "clangd")
    (c++-mode . "clangd")
    (python-mode . "pyright")
    (ruby-mode . "solargraph")
    (rust-mode . "rust-analyzer")
    (elixir-mode . "elixirLS")
    (go-mode . "gopls")
    (haskell-mode . "hls")
    (dart-mode . "dart_analysis_server")
    (scala-mode . "metals")
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

(defun lsp-bridge-restart-process ()
  "Stop and restart LSPBRIDGE process."
  (interactive)
  (setq lsp-bridge-is-starting nil)

  (lsp-bridge-kill-process)
  (lsp-bridge-start-process)
  (message "LSPBRIDGE process restarted."))

(defvar lsp-bridge-is-starting nil)

(defun lsp-bridge-start-process ()
  "Start LSPBRIDGE process if it isn't started."
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
  "Stop LSPBRIDGE process and kill all LSPBRIDGE buffers."
  (interactive)

  ;; Run stop process hooks.
  (run-hooks 'lsp-bridge-stop-process-hook)

  ;; Kill process after kill buffer, make application can save session data.
  (lsp-bridge--kill-python-process))

(defun lsp-bridge--kill-python-process ()
  "Kill LSPBRIDGE background python process."
  (interactive)
  (when (lsp-bridge-epc-live-p lsp-bridge-epc-process)
    ;; Cleanup before exit LSPBRIDGE server process.
    (lsp-bridge-call-async "cleanup")
    ;; Delete LSPBRIDGE server process.
    (lsp-bridge-epc-stop-epc lsp-bridge-epc-process)
    ;; Kill *lsp-bridge* buffer.
    (when (get-buffer lsp-bridge-name)
      (kill-buffer lsp-bridge-name))
    (setq lsp-bridge-epc-process nil)
    (message "[LSPBRIDGE] Process terminated.")))

(defun lsp-bridge--decode-string (str)
  "Decode string STR with UTF-8 coding using Base64."
  (decode-coding-string (base64-decode-string str) 'utf-8))

(defun lsp-bridge--first-start (lsp-bridge-epc-port)
  "Call `lsp-bridge--open-internal' upon receiving `start_finish' signal from server.

WEBENGINE-INCLUDE-PRIVATE-CODEC is only useful when app-name is video-player."
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

(defun lsp-bridge-enable ()
  (lsp-bridge-open-file (buffer-file-name)))

(defun lsp-bridge-open-file (filename)
  (dolist (buffer (buffer-list))
    (when (string-equal (buffer-file-name buffer) filename)
      (with-current-buffer buffer
        (let ((lang-server (lsp-bridge-get-lang-server)))
          (if lang-server
              (progn
                (setq-local lsp-bridge-flag t)
                (setq-local lsp-bridge-last-position 0)
                (setq-local lsp-bridge-completion-items nil)
                (setq-local lsp-bridge-completion-prefix nil)
                (setq-local lsp-bridge-completion-common nil)
                (setq-local lsp-bridge-filepath filename)

                (add-hook 'before-change-functions #'lsp-bridge-monitor-before-change nil t)
                (add-hook 'after-change-functions #'lsp-bridge-monitor-after-change nil t)
                (add-hook 'pre-command-hook #'lsp-bridge-monitor-pre-command nil t)
                (add-hook 'post-command-hook #'lsp-bridge-monitor-post-command nil t)
                (add-hook 'kill-buffer-hook #'lsp-bridge-monitor-kill-buffer nil t)

                (add-function :after after-focus-change-function 'lbcf-hide)

                ;; Flag `lsp-bridge-is-starting' make sure only call `lsp-bridge-start-process' once.
                (unless lsp-bridge-is-starting
                  (lsp-bridge-start-process)))
            (message "lsp-bridge not support %s now." (prin1 major-mode))))))))

(defun lsp-bridge-get-lang-server ()
  (let ((langserver-info (assoc major-mode lsp-bridge-lang-server-list)))
    (if langserver-info
        (cdr langserver-info)
      nil)))

(defun lsp-bridge--get-lang-server-by-file-func (filepath)
  (let (lang-server)
    (dolist (buffer (buffer-list))
      (when (string-equal (buffer-file-name buffer) filepath)
        (with-current-buffer buffer
          (setq lang-server (lsp-bridge-get-lang-server)))))
    lang-server))

(defun lsp-bridge-char-before ()
  (let ((prev-char (char-before)))
    (if prev-char (char-to-string prev-char) "")))

(defun lsp-bridge-monitor-post-command ()
  (when (lsp-bridge-epc-live-p lsp-bridge-epc-process)
    (unless (equal (point) lsp-bridge-last-position)
      (let* ((last-pos-line (lsp-bridge-point-row lsp-bridge-last-position))
             (last-pos-column (lsp-bridge-point-column lsp-bridge-last-position))
             (current-pos-line (lsp-bridge-point-row (point)))
             (current-pos-column (lsp-bridge-point-column (point))))
        (cond
         ;; Hide completion frame when user move cursor to different line.
         ((not (eq last-pos-line current-pos-line))
          (lbcf-hide))
         ;; Hide completion frame when user move cursor backward.
         ((and (eq last-pos-line current-pos-line)
               (< current-pos-column last-pos-column))
          (lbcf-hide))))

      (lsp-bridge-call-async "change_cursor" lsp-bridge-filepath)
      (setq-local lsp-bridge-last-position (point))))

  ;; Hide completion frame when user press Ctrl + G.
  (when (string-equal (format "%s" this-command) "keyboard-quit")
    (lbcf-hide)))

(defun lsp-bridge-monitor-kill-buffer ()
  (when (lsp-bridge-epc-live-p lsp-bridge-epc-process)
    (lsp-bridge-call-async "close_file" lsp-bridge-filepath)))

(defun lsp-bridge-record-completion-items (filepath prefix common items)
  (dolist (buffer (buffer-list))
    (when (string-equal (buffer-file-name buffer) filepath)
      (setq-local lsp-bridge-completion-items items)
      (setq-local lsp-bridge-completion-prefix prefix)
      (setq-local lsp-bridge-completion-common common)
      (cond
       ;; Show completion frame when receive completion items.
       ((and (>= (length items) 1)      ; items is more than one
             (not (string-equal (car items) "")) ; not empty items list
             (not (string-equal prefix (nth 0 items)))) ; prefix is not same last item
        (lbcf-show lsp-bridge-completion-items))
       ;; Otherwise hide completion frame.
       (t
        (lbcf-hide))))))

(defvar lsp-bridge--last-buffer nil)

(defun lsp-bridge-monitor-window-buffer-change ()
  ;; Hide completion frame when buffer or window changed.
  (unless (eq (current-buffer)
              lsp-bridge--last-buffer)
    (lbcf-hide))

  (unless (or (minibufferp)
              (string-equal (buffer-name) "*Messages*"))
    (setq lsp-bridge--last-buffer (current-buffer))))

;;;###autoload
(add-hook 'post-command-hook 'lsp-bridge-monitor-window-buffer-change)

(defvar lsp-bridge-mode-map
  '(
    ("TAB" . lsp-bridge-complete-selection)
    ("M-h" . lsp-bridge-complete-selection)
    ("M-H" . lsp-bridge-complete-common)
    ("M-n" . lsp-bridge-select-next)
    ("M-p" . lsp-bridge-select-previous)
    ("M-," . lsp-bridge-select-last)
    ("M-." . lsp-bridge-select-first)
    ))

(defun lsp-bridge-monitor-pre-command ()
  ;; Intercept keys if it match `lsp-bridge-mode-map'.
  (when (and lbcf--frame
             (frame-visible-p lbcf--frame))
    (let ((key-name (key-description (this-command-keys-vector))))
      (dolist (key-info lsp-bridge-mode-map)
        (let ((name (car key-info))
              (func (cdr key-info)))
          (when (string-equal key-name name)
            (setq this-command 'ignore)
            (funcall func)))))))

(defun lsp-bridge-complete-selection ()
  (interactive)
  (insert (string-remove-prefix lsp-bridge-completion-prefix (lbcf-get-select-item))))

(defun lsp-bridge-complete-common ()
  (interactive)
  (if (string-equal lsp-bridge-completion-prefix lsp-bridge-completion-common)
      (message "No common part found.")
    (insert (string-remove-prefix lsp-bridge-completion-prefix lsp-bridge-completion-common))))

(defun lsp-bridge-select-next ()
  (interactive)
  (lbcf-select-next))

(defun lsp-bridge-select-previous ()
  (interactive)
  (lbcf-select-prev))

(defun lsp-bridge-select-last ()
  (interactive)
  (lbcf-select-first))

(defun lsp-bridge-select-first ()
  (interactive)
  (lbcf-select-last))

(defun lsp-bridge-point-row (pos)
  (save-excursion
    (goto-char pos)
    (line-number-at-pos)))

(defun lsp-bridge-point-column (pos)
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun lsp-bridge-point-character (pos)
  (save-excursion
    (goto-char pos)
    (- (point) (line-beginning-position))))

(defun lsp-bridge-monitor-before-change (begin end)
  (setq-local lsp-bridge--before-change-begin-pos begin)
  (setq-local lsp-bridge--before-change-end-pos end)
  (setq-local lsp-bridge--before-change-end-pos-row (lsp-bridge-point-row end))
  (setq-local lsp-bridge--before-change-end-pos-character (lsp-bridge-point-character end)))

(defun lsp-bridge-monitor-after-change (begin end length)
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
                             (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
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
                             (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
     ;; Change operation.
     (t
      (lsp-bridge-call-async "change_file"
                             lsp-bridge-filepath
                             (lsp-bridge-point-row begin) (lsp-bridge-point-character begin)
                             lsp-bridge--before-change-end-pos-row lsp-bridge--before-change-end-pos-character
                             length
                             (buffer-substring-no-properties begin end)
                             (line-number-at-pos)
                             (current-column)
                             (lsp-bridge-char-before)
                             (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))))

(defun lsp-bridge-find-define ()
  (interactive)
  (lsp-bridge-call-async "find_define" lsp-bridge-filepath (line-number-at-pos) (current-column)))

(defun lsp-bridge-find-references ()
  (interactive)
  (lsp-bridge-call-async "find_references" lsp-bridge-filepath (line-number-at-pos) (current-column)))

(defun lsp-bridge-rename ()
  (interactive)
  (lsp-bridge-call-async "prepare_rename" lsp-bridge-filepath (line-number-at-pos) (current-column))
  (let ((new-name (read-string "Rename to: ")))
    (lsp-bridge-call-async "rename" lsp-bridge-filepath (line-number-at-pos) (current-column) new-name)))

(defun lsp-bridge-rename-highlight (filepath line bound-start bound-end)
  (dolist (buf (buffer-list))
    (when (string-equal (buffer-file-name buf) filepath)
      (with-current-buffer buf
        (let* ((highlight-line (+ 1 (string-to-number line)))
               (start-pos (lsp-bridge-get-pos buf highlight-line (string-to-number bound-start)))
               (end-pos (lsp-bridge-get-pos buf highlight-line (string-to-number bound-end))))
          (require 'pulse)
          (let ((pulse-iterations 1)
                (pulse-delay lsp-bridge-flash-line-delay))
            (pulse-momentary-highlight-region start-pos end-pos 'lsp-bridge-font-lock-flash)))))))

(defun lsp-bridge-get-pos (buf line column)
  (with-current-buffer buf
    (save-excursion
      (goto-line line)
      (move-to-column column)
      (point))))

(defun lsp-bridge-rename-finish (rename-files counter)
  (save-excursion
    (dolist (filepath rename-files)
      (dolist (buf (buffer-list))
        (when (string-equal (buffer-file-name buf) filepath)
          (with-current-buffer buf
            (revert-buffer :ignore-auto :noconfirm))))))

  (message "Rename %s places in %s files." counter (length rename-files)))

(defun lsp-bridge-jump-to-define (filepath row column)
  (interactive)
  (find-file filepath)
  (goto-line (+ (string-to-number row) 1))
  (move-to-column (string-to-number column))
  (require 'pulse)
  (let ((pulse-iterations 1)
        (pulse-delay lsp-bridge-flash-line-delay))
    (pulse-momentary-highlight-one-line (point) 'lsp-bridge-font-lock-flash)))

(provide 'lsp-bridge)

;;; lsp-bridge.el ends here
