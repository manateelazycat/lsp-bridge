;; tabnine-capf.el --- A lsp-bridge backend for TabNine ;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2022 Tommy Xiang, John Gong
;;
;; Author: Tommy Xiang <tommyx058@gmail.com>
;;         John Gong <gjtzone@hotmail.com>
;; Keywords: convenience
;; Version: 0.0.1
;; URL: Code is copy from https://github.com/50ways2sayhard/tabnine-capf/
;; Package-Requires: ((emacs "25"))
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;; Commentary:
;;
;; Description:
;;
;; A lsp-bridge verison of `company-tabnine`.
;;
;; Installation:
;;
;; 1. Run M-x tabnine-bridge-install-binary to install the TabNine binary for your system.
;;
;; Usage:
;;
;; See M-x customize-group RET tabnine-bridge RET for customizations.
;;
;;

;;; Code:

;;
;; Dependencies
;;

(require 'cl-lib)

;;
;; Constants
;;

;; tmp file put in tabnine-bridge-binaries-folder directory
(defconst tabnine-bridge--version-tempfile "version")

;;
;; Customization
;;

(defgroup tabnine-bridge nil
  "Options for tabnine-bridge. This Code is copy from tabnine-capf"
  :group 'lsp-bridge
  :prefix "tabnine-bridge-")

(defcustom tabnine-bridge-max-num-results 10
  "Maximum number of results to show."
  :group 'tabnine-bridge
  :type 'integer)

(defcustom tabnine-bridge-context-radius 3000
  "The number of chars before point to send for completion.

Note that setting this too small will cause TabNine to not be able to read the entire license activation key."
  :group 'tabnine-bridge
  :type 'integer)

(defcustom tabnine-bridge-context-radius-after 1000
  "The number of chars after point to send for completion."
  :group 'tabnine-bridge
  :type 'integer)

(defcustom tabnine-bridge-binaries-folder "~/.TabNine"
  "Path to TabNine binaries folder.
`tabnine-bridge-install-binary' will use this directory."
  :group 'tabnine-bridge
  :type 'string)

(defcustom tabnine-bridge-install-static-binary (file-exists-p "/etc/nixos/hardware-configuration.nix")
  "Whether to install the musl-linked static binary instead of
the standard glibc-linked dynamic binary.
Only useful on GNU/Linux.  Automatically set if NixOS is detected."
  :group 'tabnine-bridge
  :type 'boolean)

;;
;; Global methods
;;

(defun tabnine-bridge--get-target ()
  "Return TabNine's system configuration.  Used for finding the correct binary."
  (let* ((system-architecture (car (split-string system-configuration "-")))
         (tabnine-architecture
          (cond
           ((or (string= system-architecture "aarch64")
                (and (eq system-type 'darwin)
                     (string= system-architecture "x86_64")
                     ;; Detect AArch64 running x86_64 Emacs
                     (string= (shell-command-to-string "arch -arm64 uname -m") "arm64\n")))
            "aarch64")
           ((or (string= system-architecture "arm")
                (and (eq system-type 'darwin)
                     (string= system-architecture "x86_64")
                     ;; Detect AArch64 running x86_64 Emacs
                     (string= (shell-command-to-string "arch -arm64 uname -m") "arm64\n")))
            "aarch64")
           ((string= system-architecture "x86_64")
            "x86_64")
           ((string-match system-architecture "i.86")
            "i686")
           (t
            (error "Unknown or unsupported architecture %s" system-architecture))))

         (os
          (cond
           ((or (eq system-type 'ms-dos)
                (eq system-type 'windows-nt)
                (eq system-type 'cygwin))
            "pc-windows-gnu")
           ((or (eq system-type 'darwin))
            "apple-darwin")
           (tabnine-bridge-install-static-binary
            "unknown-linux-musl")
           (t
            "unknown-linux-gnu"))))

    (concat tabnine-architecture "-" os)))

(defun tabnine-bridge--get-exe ()
  "Return TabNine's binary file name.  Used for finding the correct binary."
  (cond
   ((or (eq system-type 'ms-dos)
        (eq system-type 'windows-nt)
        (eq system-type 'cygwin))
    "TabNine.exe")
   (t
    "TabNine")))

;;
;; Interactive functions
;;

(defun tabnine-bridge-install-binary ()
  "Install TabNine binary into `tabnine-bridge-binaries-folder'."
  (interactive)
  (let ((version-tempfile (concat
                           (file-name-as-directory
                            tabnine-bridge-binaries-folder)
                           tabnine-bridge--version-tempfile))
        (target (tabnine-bridge--get-target))
        (exe (tabnine-bridge--get-exe))
        (binaries-dir tabnine-bridge-binaries-folder))
    (message version-tempfile)
    (message "Getting current version...")
    (make-directory (file-name-directory version-tempfile) t)
    (url-copy-file "https://update.tabnine.com/bundles/version" version-tempfile t)
    (let ((version (string-trim (with-temp-buffer (insert-file-contents version-tempfile) (buffer-string)))))
      (when (= (length version) 0)
        (error "TabNine installation failed.  Please try again"))
      (message "Current version is %s" version)
      (let* ((url (concat "https://update.tabnine.com/bundles/" version "/" target "/TabNine.zip"))
             (version-directory (file-name-as-directory
                                 (concat
                                  (file-name-as-directory
                                   (concat (file-name-as-directory binaries-dir) version)))))
             (target-directory (file-name-as-directory (concat version-directory target) ))
             (bundle-path (concat version-directory (format "%s.zip" target)))
             (target-path (concat target-directory exe)))
        (message "Installing at %s. Downloading %s ..." target-path url)
        (make-directory target-directory t)
        (url-copy-file url bundle-path t)
        (condition-case ex
            (let ((default-directory target-directory))
              (if (or (eq system-type 'ms-dos)
                      (eq system-type 'windows-nt)
                      (eq system-type 'cygwin))
                  (shell-command (format "tar -xf %s" (expand-file-name bundle-path)))
                (shell-command (format "unzip -o %s -d %s"
                                       (expand-file-name bundle-path)
                                       (expand-file-name target-directory)))))
          ('error
           (error "Unable to unzip automatically. Please go to [%s] and unzip the content of [%s] into [%s/]."
                  (expand-file-name version-directory)
                  (file-name-nondirectory bundle-path)
                  (file-name-sans-extension (file-name-nondirectory bundle-path)))))
        (mapc (lambda (filename)
                (set-file-modes (concat target-directory filename) (string-to-number "744" 8)))
              (cl-remove-if '(lambda (it) (member it '("." "..")) (directory-files target-directory))))
        (delete-file bundle-path)
        (delete-file version-tempfile)
        (message "TabNine installation complete.")))))

(provide 'tabnine-bridge)

;;; tabnine-bridge.el ends here
