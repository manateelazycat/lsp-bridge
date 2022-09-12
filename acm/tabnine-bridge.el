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
(require 'json)
(require 'dash)

;;
;; Constants
;;

(defconst tabnine-bridge--process-name "tabnine-bridge--process")
(defconst tabnine-bridge--hooks-alist nil)
(defconst tabnine-bridge--protocol-version "1.0.14")

;; tmp file put in tabnine-bridge-binaries-folder directory
(defconst tabnine-bridge--version-tempfile "version")

;;
;; Macros
;;

(defmacro tabnine-bridge--with-destructured-candidate
    (candidate &rest body)
  (declare (indent 1) (debug t))
  `(let-alist ,candidate
     (setq type (tabnine-bridge--kind-to-type .kind))
     (propertize
      .new_prefix
      'old_suffix .old_suffix
      'new_suffix .new_suffix
      'kind .kind
      'type type
      'detail .detail
      'annotation (concat (or .detail "") " " (or type "")))
     ,@body))

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

(defcustom tabnine-bridge-max-restart-count 10
  "Maximum number of times TabNine can consecutively restart.
This may be due to errors in or automatic server updates.
Any successful completion will reset the consecutive count."
  :group 'tabnine-bridge
  :type 'integer)

(defcustom tabnine-bridge-wait 0.25
  "Number of seconds to wait for TabNine to respond."
  :group 'tabnine-bridge
  :type 'float)

(defcustom tabnine-bridge-always-trigger t
  "Whether to overload company's minimum prefix length.
This allows completion to trigger on as much as possible.
Default is t (strongly recommended)."
  :group 'tabnine-bridge
  :type 'boolean)

(defcustom tabnine-bridge-no-continue nil
  "Whether to make company reset idle timer on all keystrokes.
Only useful when `company-idle-delay' is not 0.
Doing so improves performance by reducing number of calls to the completer,
at the cost of less responsive completions."
  :group 'tabnine-bridge
  :type 'boolean)

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

(defcustom tabnine-bridge-log-file-path nil
  "If non-nil, next TabNine restart will write debug log to this path."
  :group 'tabnine-bridge
  :type 'string)

(defcustom tabnine-bridge-use-native-json t
  "Whether to use native JSON when possible."
  :group 'tabnine-bridge
  :type 'boolean)

;;
;; Faces
;;

;;
;; Variables
;;

(defvar tabnine-bridge-executable-args nil
  "Arguments passed to TabNine.")

(defvar tabnine-bridge--process nil
  "TabNine server process.")

(defvar tabnine-bridge--restart-count 0
  "Number of times TabNine server has restarted abnormally.
Resets every time successful completion is returned.")

(defvar tabnine-bridge--response nil
  "Temporarily stored TabNine server responses.")

(defvar tabnine-bridge--disabled nil
  "Variable to temporarily disable tabnine-bridge and pass control to next backend.")

(defvar tabnine-bridge--calling-continue nil
  "Flag for when `company-continue' is being called.")

(defvar tabnine-bridge--response-chunks nil
  "The string to store response chunks from TabNine server.")

;;
;; Major mode definition
;;

;;
;; Global methods
;;

(defun tabnine-bridge--error-no-binaries ()
  "Signal error for when TabNine binary is not found."
  (error "No TabNine binaries found.  Run M-x tabnine-bridge-install-binary to download binaries"))

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

(defun tabnine-bridge--executable-path ()
  "Find and return the path of the latest TabNine binary for the current system."
  (let ((parent tabnine-bridge-binaries-folder))
    (if (file-directory-p parent)
        (let* ((children (->> (directory-files parent)
                              (--remove (member it '("." "..")))
                              (--filter (file-directory-p
                                         (expand-file-name
                                          it
                                          (file-name-as-directory
                                           parent))))
                              (--filter (ignore-errors (version-to-list it)))
                              (-non-nil)))
               (sorted (nreverse (sort children #'version<)))
               (target (tabnine-bridge--get-target))
               (filename (tabnine-bridge--get-exe)))
          (cl-loop
           for ver in sorted
           for fullpath = (expand-file-name (format "%s/%s/%s"
                                                    ver target filename)
                                            parent)
           if (and (file-exists-p fullpath)
                   (file-regular-p fullpath))
           return fullpath
           finally do (tabnine-bridge--error-no-binaries)))
      (tabnine-bridge--error-no-binaries))))

(defun tabnine-bridge-start-process ()
  "Start TabNine process."
  (tabnine-bridge-kill-process)
  (let ((process-connection-type nil))
    (setq tabnine-bridge--process
          (make-process
           :name tabnine-bridge--process-name
           :command (append
                     (cons (tabnine-bridge--executable-path)
                           (when tabnine-bridge-log-file-path
                             (list
                              "--log-file-path"
                              (expand-file-name
                               tabnine-bridge-log-file-path))))
                     (list "--client" "emacs")
                     tabnine-bridge-executable-args)
           :coding 'utf-8
           :connection-type 'pipe
           :filter #'tabnine-bridge--process-filter
           :sentinel #'tabnine-bridge--process-sentinel
           :noquery t)))
  ;; hook setup
  (message "TabNine server started.")
  (dolist (hook tabnine-bridge--hooks-alist)
    (add-hook (car hook) (cdr hook))))

(defun tabnine-bridge-kill-process ()
  "Kill TabNine process."
  (interactive)
  (when tabnine-bridge--process
    (let ((process tabnine-bridge--process))
      (setq tabnine-bridge--process nil) ; this happens first so sentinel don't catch the kill
      (delete-process process)))
  ;; hook remove
  (dolist (hook tabnine-bridge--hooks-alist)
    (remove-hook (car hook) (cdr hook))))

(defun tabnine-bridge-send-request (request)
  "Send REQUEST to TabNine server.  REQUEST needs to be JSON-serializable object."
  (when (null tabnine-bridge--process)
    (tabnine-bridge-start-process))
  (when tabnine-bridge--process
    ;; TODO make sure utf-8 encoding works
    (let ((encoded (concat
                    (if (and tabnine-bridge-use-native-json
                             (fboundp 'json-serialize))
                        (json-serialize request
                                        :null-object nil
                                        :false-object json-false)
                      (let ((json-null nil)
                            (json-encoding-pretty-print nil))
                        (json-encode-list request)))
                    "\n")))
      (setq tabnine-bridge--response nil)
      (process-send-string tabnine-bridge--process encoded)
      (accept-process-output tabnine-bridge--process tabnine-bridge-wait))))

(defun tabnine-bridge--make-request (method)
  "Create request body for method METHOD and parameters PARAMS."
  (cond
   ((eq method 'autocomplete)
    (let* ((buffer-min 1)
           (buffer-max (1+ (buffer-size)))
           (before-point
            (max (point-min) (- (point) tabnine-bridge-context-radius)))
           (after-point
            (min (point-max) (+ (point) tabnine-bridge-context-radius-after))))

      (list
       :version tabnine-bridge--protocol-version
       :request
       (list :Autocomplete
             (list
              :before (buffer-substring-no-properties before-point (point))
              :after (buffer-substring-no-properties (point) after-point)
              :filename (or (buffer-file-name) nil)
              :region_includes_beginning (if (= before-point buffer-min)
                                             t json-false)

              :region_includes_end (if (= after-point buffer-max)
                                       t json-false)
              :max_num_results tabnine-bridge-max-num-results)))))

   ((eq method 'prefetch)
    (list
     :version tabnine-bridge--protocol-version
     :request
     (list :Prefetch
           (list
            :filename (or (buffer-file-name) nil)
            ))))
   ((eq method 'getidentifierregex)
    (list
     :version tabnine-bridge--protocol-version
     :request
     (list :GetIdentifierRegex
           (list
            :filename (or (buffer-file-name) nil)
            ))))))

(defun tabnine-bridge-query ()
  "Query TabNine server for auto-complete."
  (let ((request (tabnine-bridge--make-request 'autocomplete)))
    (tabnine-bridge-send-request request)
    ))

(defun tabnine-bridge--decode (msg)
  "Decode TabNine server response MSG, and return the decoded object."
  (if (and tabnine-bridge-use-native-json
           (fboundp 'json-parse-string))
      (ignore-errors
        (json-parse-string msg :object-type 'alist))
    (let ((json-array-type 'list)
          (json-object-type 'alist))
      (json-read-from-string msg))))

(defun tabnine-bridge--process-sentinel (process event)
  "Sentinel for TabNine server process.
PROCESS is the process under watch, EVENT is the event occurred."
  (when (and tabnine-bridge--process
             (memq (process-status process) '(exit signal)))

    (message "TabNine process %s received event %s."
             (prin1-to-string process)
             (prin1-to-string event))

    (if (>= tabnine-bridge--restart-count
            tabnine-bridge-max-restart-count)
        (progn
          (message "TabNine process restart limit reached.")
          (setq tabnine-bridge--process nil))

      (message "Restarting TabNine process.")
      (tabnine-bridge-start-process)
      (setq tabnine-bridge--restart-count
            (1+ tabnine-bridge--restart-count)))))

(defun tabnine-bridge--process-filter (process output)
  "Filter for TabNine server process.
PROCESS is the process under watch, OUTPUT is the output received."
  (push output tabnine-bridge--response-chunks)
  (when (string-suffix-p "\n" output)
    (let ((response
           (mapconcat #'identity
                      (nreverse tabnine-bridge--response-chunks)
                      nil)))
      (setq tabnine-bridge--response
            (tabnine-bridge--decode response)
            tabnine-bridge--response-chunks nil))))

(defun tabnine-bridge--kind-to-type (kind)
  (pcase kind
    (1 "Text")
    (2 "Method")
    (3 "Function")
    (4 "Constructor")
    (5 "Field")
    (6 "Variable")
    (7 "Class")
    (8 "Interface")
    (9 "Module")
    (10 "Property" )
    (11 "Unit" )
    (12 "Value" )
    (13 "Enum")
    (14 "Keyword" )
    (15 "Snippet")
    (16 "Color")
    (17 "File")
    (18 "Reference")
    (19 "Folder")
    (20 "EnumMember")
    (21 "Constant")
    (22 "Struct")
    (23 "Event")
    (24 "Operator")
    (25 "TypeParameter")))

(defun tabnine-bridge--construct-candidate-generic (candidate)
  "Generic function to construct completion string from a CANDIDATE."
  (tabnine-bridge--with-destructured-candidate candidate))

(defun tabnine-bridge--construct-candidates (results construct-candidate-fn)
  "Use CONSTRUCT-CANDIDATE-FN to construct a list of candidates from RESULTS."
  (let ((completions (mapcar construct-candidate-fn results)))
    (when completions
      (setq tabnine-bridge--restart-count 0))
    completions))

(defun tabnine-bridge--get-candidates (response)
  "Get candidates for RESPONSE."
  (tabnine-bridge--construct-candidates
   (alist-get 'results response)
   #'tabnine-bridge--construct-candidate-generic))

(defun tabnine-bridge--candidates (prefix)
  "Candidates-command handler for the company backend for PREFIX.

Return completion candidates.  Must be called after `tabnine-bridge-query'."
  (tabnine-bridge--get-candidates tabnine-bridge--response))

;;
;; Interactive functions
;;

(defun tabnine-bridge-restart-server ()
  "Start/Restart TabNine server."
  (interactive)
  (tabnine-bridge-start-process))

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
              (--remove (member it '("." "..")) (directory-files target-directory)))
        (delete-file bundle-path)
        (delete-file version-tempfile)
        (message "TabNine installation complete.")))))

(defvar-local tabnine-bridge--begin-pos nil)

(defun acm-backend-tabnine-clean ()
  (setq-local tabnine-bridge--begin-pos nil))

(provide 'tabnine-bridge)

;;; tabnine-bridge.el ends here
