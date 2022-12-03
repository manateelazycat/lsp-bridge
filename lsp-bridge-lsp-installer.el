;;; lsp-bridge-lsp-installer.el --- LSP server installer   -*- lexical-binding: t -*-

;; Filename: lsp-bridge-lsp-installer.el
;; Description: LSP server installer 
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-09-27 09:28:09
;; Version: 0.1
;; Last-Updated: 2022-09-27 09:28:09
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/lsp-bridge-lsp-installer
;; Keywords: 
;; Compatibility: GNU Emacs 28.1
;;
;; Features that might be required by this library:
;;
;; 
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
;; LSP server installer 
;; 

;;; Installation:
;;
;; Put lsp-bridge-lsp-installer.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lsp-bridge-lsp-installer)
;;
;; No need more.

;;; Customize:
;;
;; 
;;
;; All of the above can customize by:
;;      M-x customize-group RET lsp-bridge-lsp-installer RET
;;

;;; Change log:
;;
;; 2022/09/27
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

;;; Require
(require 'cl-lib)
;;; Code:

(defun lsp-bridge-install-omnisharp ()
  (interactive)
  (let* ((to-append (if (string= lsp-bridge-csharp-lsp-server "omnisharp-dotnet")
			(if (eq system-type 'windows-nt)
			    "omnisharp-win-x64-net6.0.zip"
			  "omnisharp-linux-x64-net6.0.zip")
		      "omnisharp-mono.zip"))
	(url (concat "https://github.com/OmniSharp/omnisharp-roslyn/releases/latest/download/" to-append))
        (down-des (if (eq system-type 'windows-nt)
                       (substitute-in-file-name (concat "\\$USERPROFILE\\AppData\\Local\\Temp\\" to-append))
                    "/tmp/omnisharp-mono.zip"))
        (install-des (if (eq system-type 'windows-nt)
                         (expand-file-name (format "%s.cache/omnisharp/" user-emacs-directory))
                       "~/.emacs.d/.cache/omnisharp/"
                       (format "%s.cache/omnisharp/" user-emacs-directory))))
    (url-copy-file url down-des 1)
    (unless (file-directory-p install-des)
      (make-directory install-des t))
    (call-process-shell-command (format "%s -xf %s -C %s" "tar" down-des install-des))
    ))

(defconst tabnine-bridge--version-tempfile "version")

(defcustom tabnine-bridge-binaries-folder (expand-file-name "~/.TabNine")
  "Path to TabNine binaries folder.
`lsp-bridge-install-tabnine' will use this directory."
  :type 'string)

(defcustom tabnine-bridge-install-static-binary (file-exists-p "/etc/nixos/hardware-configuration.nix")
  "Whether to install the musl-linked static binary instead of
the standard glibc-linked dynamic binary.
Only useful on GNU/Linux.  Automatically set if NixOS is detected."
  :type 'boolean)

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

(defun lsp-bridge-install-tabnine ()
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
        (dolist (filename (directory-files target-directory))
          (unless (member filename '("." ".."))
            (set-file-modes (concat target-directory filename) (string-to-number "744" 8))))
        (delete-file bundle-path)
        (delete-file version-tempfile)
        (message "TabNine installation complete.")))))

(provide 'lsp-bridge-lsp-installer)

;;; lsp-bridge-lsp-installer.el ends here
