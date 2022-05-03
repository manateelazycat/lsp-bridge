;;; lsp-bridge.el --- LSP bridge  -*- lexical-binding: t; -*-

;; Filename: lsp-bridge.el
;; Description: LSP bridge
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.5
;; Last-Updated: Sun Nov 21 04:35:02 2021 (-0500)
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
;; 2021/11/13
;;      * First released.
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

(defgroup lsp-bridge nil
  "LSPBRIDGE group."
  :group 'applications)

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

(defvar lsp-bridge--first-start-args nil)

(defvar lsp-bridge--first-start-callback nil)

(defcustom lsp-bridge-name "*lsp-bridge*"
  "Name of LSPBRIDGE buffer."
  :type 'string)

(defcustom lsp-bridge-python-command (if (memq system-type '(cygwin windows-nt ms-dos)) "python.exe" "python3")
  "The Python interpreter used to run lsp-bridge.py."
  :type 'string)

(defcustom lsp-bridge-proxy-host ""
  "Proxy Host used by LSPBRIDGE Browser."
  :type 'string)

(defcustom lsp-bridge-proxy-port ""
  "Proxy Port used by LSPBRIDGE Browser."
  :type 'string)

(defcustom lsp-bridge-proxy-type ""
  "Proxy Type used by LSPBRIDGE Browser.  The value is either \"http\" or \"socks5\"."
  :type 'string)

(defcustom lsp-bridge-popup-pos "point-bottom-right"
  "The position to display the poweb popup.

Available options:
- \"point-bottom\": bottom of the cursor
- \"point-bottom-right\": bottom-right of the cursor
- \"top-left\": top-left of the screen
- \"top-right\": top-right of the screen
- \"bottom-left\": bottom-left: of the screen
- \"bottom-right\": bottom-right of the screen"
  :type 'string)

(defcustom lsp-bridge-enable-debug nil
  "If you got segfault error, please turn this option.
Then LSPBRIDGE will start by gdb, please send new issue with `*lsp-bridge*' buffer content when next crash."
  :type 'boolean)

(defcustom lsp-bridge-start-python-process-when-require t
  "Start LSPBRIDGE python process when require `lsp-bridge', default is turn on.

Turn on this option will improve start speed."
  :type 'boolean)

(defun lsp-bridge-call-async (method &rest args)
  "Call Python EPC function METHOD and ARGS asynchronously."
  (lsp-bridge-deferred-chain
    (lsp-bridge-epc-call-deferred lsp-bridge-epc-process (read method) args)))

(defun lsp-bridge-call-sync (method &rest args)
  "Call Python EPC function METHOD and ARGS synchronously."
  (lsp-bridge-epc-call-sync lsp-bridge-epc-process (read method) args))

(defun lsp-bridge--follow-system-dpi ()
  (if (and (getenv "WAYLAND_DISPLAY") (not (string= (getenv "WAYLAND_DISPLAY") "")))
      (progn
        ;; We need manually set scale factor when at Gnome/Wayland environment.
        ;; It is important to set QT_AUTO_SCREEN_SCALE_FACTOR=0
        ;; otherwise Qt which explicitly force high DPI enabling get scaled TWICE.
        (setenv "QT_AUTO_SCREEN_SCALE_FACTOR" "0")
        ;; Set LSPBRIDGE application scale factor.
        (setenv "QT_SCALE_FACTOR" "1")
        ;; Force xwayland to ensure SWay works.
        (setenv "QT_QPA_PLATFORM" "xcb"))
    (setq process-environment
          (seq-filter
           (lambda (var)
             (and (not (string-match-p "QT_SCALE_FACTOR" var))
                  (not (string-match-p "QT_SCREEN_SCALE_FACTOR" var))))
           process-environment))))

(defun lsp-bridge-restart-process ()
  "Stop and restart LSPBRIDGE process."
  (interactive)
  (lsp-bridge-kill-process)
  (lsp-bridge-start-process)
  (message "Lsp-Bridge process restarted."))

(defun lsp-bridge-start-process ()
  "Start LSPBRIDGE process if it isn't started."
  (unless (lsp-bridge-epc-live-p lsp-bridge-epc-process)
    ;; start epc server and set `lsp-bridge-server-port'
    (lsp-bridge--start-epc-server)
    (let* ((lsp-bridge-args (append
                             (list lsp-bridge-python-file)
                             (list (number-to-string lsp-bridge-server-port))
                             )))

      ;; Folow system DPI.
      (lsp-bridge--follow-system-dpi)

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

(run-with-idle-timer
 1 nil
 #'(lambda ()
     ;; Start LSPBRIDGE python process when load `lsp-bridge'.
     ;; It will improve start speed.
     (when lsp-bridge-start-python-process-when-require
       (lsp-bridge-start-process))))

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
    (message "[LSPBRIDGE] Process terminated.")))

(defun lsp-bridge--decode-string (str)
  "Decode string STR with UTF-8 coding using Base64."
  (decode-coding-string (base64-decode-string str) 'utf-8))

(defun lsp-bridge--encode-string (str)
  "Encode string STR with UTF-8 coding using Base64."
  (base64-encode-string (encode-coding-string str 'utf-8)))

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
  (when lsp-bridge--first-start-args
    (funcall lsp-bridge--first-start-callback lsp-bridge--first-start-args))
  (setq lsp-bridge--first-start-args nil))

(defun lsp-bridge-get-cursor-coordinate ()
  (if (derived-mode-p 'eaf-mode)
      (mouse-absolute-pixel-position)
    (window-absolute-pixel-position)))

(defun lsp-bridge-get-cursor-x-offset ()
  (if (derived-mode-p 'eaf-mode)
      30
    0))

(defun lsp-bridge-get-cursor-y-offset ()
  (if (derived-mode-p 'eaf-mode)
      30
    (line-pixel-height)))

(defun lsp-bridge-start (first-start-callback args)
  (setq lsp-bridge--first-start-callback first-start-callback)

  (if (lsp-bridge-epc-live-p lsp-bridge-epc-process)
      (funcall first-start-callback args)

    (setq lsp-bridge--first-start-args args)
    (lsp-bridge-start-process)))

(defun lsp-bridge-get-theme-mode ()
  (format "%s" (frame-parameter nil 'background-mode)))

(defun lsp-bridge-get-theme-background ()
  (lsp-bridge-color-name-to-hex (face-attribute 'default :background)))

(defun lsp-bridge-color-int-to-hex (int)
  (substring (format (concat "%0" (int-to-string 4) "X") int) (- 2)))

(defun lsp-bridge-color-name-to-hex (color)
  (let ((components (x-color-values color)))
    (concat "#"
            (lsp-bridge-color-int-to-hex (nth 0 components))
            (lsp-bridge-color-int-to-hex (nth 1 components))
            (lsp-bridge-color-int-to-hex (nth 2 components)))))

(provide 'lsp-bridge)

;;; lsp-bridge.el ends here
