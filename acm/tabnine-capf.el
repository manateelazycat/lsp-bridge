;; tabnine-capf.el --- A company-mode backend for TabNine ;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2022 Tommy Xiang, John Gong
;;
;; Author: Tommy Xiang <tommyx058@gmail.com>
;;         John Gong <gjtzone@hotmail.com>
;; Keywords: convenience
;; Version: 0.0.1
;; URL: https://github.com/50ways2sayhard/tabnine-capf/
;; Package-Requires: ((emacs "25") (cl-lib "0.5") (dash "2.16.0") (s "1.12.0") (unicode-escape "1.1"))
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
;; A capf verison of `company-tabnine`.
;;
;; Installation:
;;
;; 1. Add `tabnine-completion-at-point` to `completion-at-point-functions`
;;    (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)
;; 2. Run M-x tabnine-capf-install-binary to install the TabNine binary for your system.
;;
;; Usage:
;;
;; See M-x customize-group RET tabnine-capf RET for customizations.
;;
;;

;;; Code:

;;
;; Dependencies
;;

(require 'cl-lib)
(require 'dash)
(require 'json)
(require 's)
(require 'unicode-escape)
(require 'url)

;;
;; Constants
;;

(defconst tabnine-capf--process-name "tabnine-capf--process")
(defconst tabnine-capf--buffer-name "*tabnine-capf-log*")
(defconst tabnine-capf--hooks-alist nil)
(defconst tabnine-capf--protocol-version "1.0.14")

;; tmp file put in tabnine-capf-binaries-folder directory
(defconst tabnine-capf--version-tempfile "version")

;; current don't know how to use Prefetch and GetIdentifierRegex
(defconst tabnine-capf--method-autocomplete "Autocomplete")
(defconst tabnine-capf--method-prefetch "Prefetch")
(defconst tabnine-capf--method-getidentifierregex "GetIdentifierRegex")

;;
;; Macros
;;

(defmacro tabnine-capf-with-disabled (&rest body)
  "Run BODY with `tabnine-capf' temporarily disabled.
Useful when binding keys to temporarily query other completion backends."
  `(let ((tabnine-capf--disabled t))
     ,@body))

(defmacro tabnine-capf--with-destructured-candidate
    (candidate &rest body)
  (declare (indent 1) (debug t))
  `(let-alist ,candidate
     (setq type (tabnine-capf--kind-to-type .kind))
     (propertize
      .new_prefix
      'old_suffix .old_suffix
      'new_suffix .new_suffix
      'kind .kind
      'type type
      'detail .detail
      'annotation
      (concat (or .detail "") " " (or type "")))
     ,@body))

(defun tabnine-capf--filename-completer-p (extra-info)
  "Check whether candidate's EXTRA-INFO indicates a filename completion."
  (-contains? '("[File]" "[Dir]" "[File&Dir]") extra-info))

(defun tabnine-capf--identifier-completer-p (extra-info)
  "Check if candidate's EXTRA-INFO indicates a identifier completion."
  (s-equals? "[ID]" extra-info))

;;
;; Customization
;;

(defgroup tabnine-capf nil
  "Options for tabnine-capf."
  :link '(url-link :tag "Github" "https://github.com/50ways2sayhard/tabnine-capf")
  :group 'company
  :prefix "tabnine-capf-")

(defcustom tabnine-capf-max-num-results 10
  "Maximum number of results to show."
  :group 'tabnine-capf
  :type 'integer)

(defcustom tabnine-capf-context-radius 3000
  "The number of chars before point to send for completion.

Note that setting this too small will cause TabNine to not be able to read the entire license activation key."
  :group 'tabnine-capf
  :type 'integer)

(defcustom tabnine-capf-context-radius-after 1000
  "The number of chars after point to send for completion."
  :group 'tabnine-capf
  :type 'integer)

(defcustom tabnine-capf-max-restart-count 10
  "Maximum number of times TabNine can consecutively restart.
This may be due to errors in or automatic server updates.
Any successful completion will reset the consecutive count."
  :group 'tabnine-capf
  :type 'integer)

(defcustom tabnine-capf-wait 0.25
  "Number of seconds to wait for TabNine to respond."
  :group 'tabnine-capf
  :type 'float)

(defcustom tabnine-capf-always-trigger t
  "Whether to overload company's minimum prefix length.
This allows completion to trigger on as much as possible.
Default is t (strongly recommended)."
  :group 'tabnine-capf
  :type 'boolean)

(defcustom tabnine-capf-no-continue nil
  "Whether to make company reset idle timer on all keystrokes.
Only useful when `company-idle-delay' is not 0.
Doing so improves performance by reducing number of calls to the completer,
at the cost of less responsive completions."
  :group 'tabnine-capf
  :type 'boolean)

(defcustom tabnine-capf-binaries-folder "~/.TabNine"
  "Path to TabNine binaries folder.
`tabnine-capf-install-binary' will use this directory."
  :group 'tabnine-capf
  :type 'string)

(defcustom tabnine-capf-install-static-binary (file-exists-p "/etc/nixos/hardware-configuration.nix")
  "Whether to install the musl-linked static binary instead of
the standard glibc-linked dynamic binary.
Only useful on GNU/Linux.  Automatically set if NixOS is detected."
  :group 'tabnine-capf
  :type 'boolean)

(defcustom tabnine-capf-log-file-path nil
  "If non-nil, next TabNine restart will write debug log to this path."
  :group 'tabnine-capf
  :type 'string)

(defcustom tabnine-capf-auto-balance t
  "Whether TabNine should insert balanced parentheses upon completion."
  :group 'tabnine-capf
  :type 'boolean)

;; (defcustom tabnine-capf-async t
;;   "Whether or not to use async operations to fetch data."
;;   :group 'tabnine-capf
;;   :type 'boolean)

(defcustom tabnine-capf-show-annotation t
  "Whether to show an annotation inline with the candidate."
  :group 'tabnine-capf
  :type 'boolean)

(defcustom tabnine-capf-auto-fallback t
  "Whether to automatically fallback to other backends when TabNine has no candidates."
  :group 'tabnine-capf
  :type 'boolean)

(defcustom tabnine-capf-use-native-json t
  "Whether to use native JSON when possible."
  :group 'tabnine-capf
  :type 'boolean)

(defcustom tabnine-capf-insert-arguments t
  "When non-nil, insert function arguments as a template after completion.
Only supported by modes in `tabnine-capf--extended-features-modes'"
  :group 'tabnine-capf
  :type 'boolean)


;;
;; Faces
;;

;;
;; Variables
;;

(defvar tabnine-capf-executable-args nil
  "Arguments passed to TabNine.")

(defvar tabnine-capf--process nil
  "TabNine server process.")

(defvar tabnine-capf--restart-count 0
  "Number of times TabNine server has restarted abnormally.
Resets every time successful completion is returned.")

(defvar tabnine-capf--response nil
  "Temporarily stored TabNine server responses.")

(defvar tabnine-capf--disabled nil
  "Variable to temporarily disable tabnine-capf and pass control to next backend.")

(defvar tabnine-capf--calling-continue nil
  "Flag for when `company-continue' is being called.")

(defvar tabnine-capf--response-chunks nil
  "The string to store response chunks from TabNine server.")

;;
;; Major mode definition
;;

;;
;; Global methods
;;

(defun tabnine-capf--prefix-candidate-p (candidate prefix)
  "Return t if CANDIDATE string begins with PREFIX."
  (let ((insertion-text (cdr (assq 'insertion_text candidate))))
    (s-starts-with? prefix insertion-text t)))

(defun tabnine-capf--error-no-binaries ()
  "Signal error for when TabNine binary is not found."
  (error "No TabNine binaries found.  Run M-x tabnine-capf-install-binary to download binaries"))

(defun tabnine-capf--get-target ()
  "Return TabNine's system configuration.  Used for finding the correct binary."
  (let* ((system-architecture (car (s-split "-" system-configuration)))
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
           (tabnine-capf-install-static-binary
            "unknown-linux-musl")
           (t
            "unknown-linux-gnu"))))

    (concat tabnine-architecture "-" os)))

(defun tabnine-capf--get-exe ()
  "Return TabNine's binary file name.  Used for finding the correct binary."
  (cond
   ((or (eq system-type 'ms-dos)
        (eq system-type 'windows-nt)
        (eq system-type 'cygwin))
    "TabNine.exe")
   (t
    "TabNine")))

(defun tabnine-capf--executable-path ()
  "Find and return the path of the latest TabNine binary for the current system."
  (let ((parent tabnine-capf-binaries-folder))
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
               (target (tabnine-capf--get-target))
               (filename (tabnine-capf--get-exe)))
          (cl-loop
           for ver in sorted
           for fullpath = (expand-file-name (format "%s/%s/%s"
                                                    ver target filename)
                                            parent)
           if (and (file-exists-p fullpath)
                   (file-regular-p fullpath))
           return fullpath
           finally do (tabnine-capf--error-no-binaries)))
      (tabnine-capf--error-no-binaries))))

(defun tabnine-capf-start-process ()
  "Start TabNine process."
  (tabnine-capf-kill-process)
  (let ((process-connection-type nil))
    (setq tabnine-capf--process
          (make-process
           :name tabnine-capf--process-name
           :command (append
                     (cons (tabnine-capf--executable-path)
                           (when tabnine-capf-log-file-path
                             (list
                              "--log-file-path"
                              (expand-file-name
                               tabnine-capf-log-file-path))))
                     (list "--client" "emacs")
                     tabnine-capf-executable-args)
           :coding 'utf-8
           :connection-type 'pipe
           :filter #'tabnine-capf--process-filter
           :sentinel #'tabnine-capf--process-sentinel
           :noquery t)))
  ;; hook setup
  (message "TabNine server started.")
  (dolist (hook tabnine-capf--hooks-alist)
    (add-hook (car hook) (cdr hook))))

(defun tabnine-capf-kill-process ()
  "Kill TabNine process."
  (interactive)
  (when tabnine-capf--process
    (let ((process tabnine-capf--process))
      (setq tabnine-capf--process nil) ; this happens first so sentinel don't catch the kill
      (delete-process process)))
  ;; hook remove
  (dolist (hook tabnine-capf--hooks-alist)
    (remove-hook (car hook) (cdr hook))))

(defun tabnine-capf-send-request (request)
  "Send REQUEST to TabNine server.  REQUEST needs to be JSON-serializable object."
  (when (null tabnine-capf--process)
    (tabnine-capf-start-process))
  (when tabnine-capf--process
    ;; TODO make sure utf-8 encoding works
    (let ((encoded (concat
                    (if (and tabnine-capf-use-native-json
                             (fboundp 'json-serialize))
                        (json-serialize request
                                        :null-object nil
                                        :false-object json-false)
                      (let ((json-null nil)
                            (json-encoding-pretty-print nil))
                        (json-encode-list request)))
                    "\n")))
      (setq tabnine-capf--response nil)
      (process-send-string tabnine-capf--process encoded)
      (accept-process-output tabnine-capf--process tabnine-capf-wait))))

(defun tabnine-capf--make-request (method)
  "Create request body for method METHOD and parameters PARAMS."
  (cond
   ((eq method 'autocomplete)
    (let* ((buffer-min 1)
           (buffer-max (1+ (buffer-size)))
           (before-point
            (max (point-min) (- (point) tabnine-capf-context-radius)))
           (after-point
            (min (point-max) (+ (point) tabnine-capf-context-radius-after))))

      (list
       :version tabnine-capf--protocol-version
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
              :max_num_results tabnine-capf-max-num-results)))))

   ((eq method 'prefetch)
    (list
     :version tabnine-capf--protocol-version
     :request
     (list :Prefetch
           (list
            :filename (or (buffer-file-name) nil)
            ))))
   ((eq method 'getidentifierregex)
    (list
     :version tabnine-capf--protocol-version
     :request
     (list :GetIdentifierRegex
           (list
            :filename (or (buffer-file-name) nil)
            ))))))

(defun tabnine-capf-query ()
  "Query TabNine server for auto-complete."
  (let ((request (tabnine-capf--make-request 'autocomplete)))
    (tabnine-capf-send-request request)
    ))

(defun tabnine-capf--decode (msg)
  "Decode TabNine server response MSG, and return the decoded object."
  (if (and tabnine-capf-use-native-json
           (fboundp 'json-parse-string))
      (ignore-errors
        (json-parse-string msg :object-type 'alist))
    (let ((json-array-type 'list)
          (json-object-type 'alist))
      (json-read-from-string msg))))

(defun tabnine-capf--process-sentinel (process event)
  "Sentinel for TabNine server process.
PROCESS is the process under watch, EVENT is the event occurred."
  (when (and tabnine-capf--process
             (memq (process-status process) '(exit signal)))

    (message "TabNine process %s received event %s."
             (prin1-to-string process)
             (prin1-to-string event))

    (if (>= tabnine-capf--restart-count
            tabnine-capf-max-restart-count)
        (progn
          (message "TabNine process restart limit reached.")
          (setq tabnine-capf--process nil))

      (message "Restarting TabNine process.")
      (tabnine-capf-start-process)
      (setq tabnine-capf--restart-count
            (1+ tabnine-capf--restart-count)))))

(defun tabnine-capf--process-filter (process output)
  "Filter for TabNine server process.
PROCESS is the process under watch, OUTPUT is the output received."
  (push output tabnine-capf--response-chunks)
  (when (s-ends-with-p "\n" output)
    (let ((response
           (mapconcat #'identity
                      (nreverse tabnine-capf--response-chunks)
                      nil)))
      (setq tabnine-capf--response
            (tabnine-capf--decode response)
            tabnine-capf--response-chunks nil))))

(defun tabnine-capf--prefix ()
  "Prefix-command handler for the company backend."
  (if (or (and tabnine-capf-no-continue
               tabnine-capf--calling-continue)
          tabnine-capf--disabled)
      nil
    (tabnine-capf-query)
    (let ((prefix
           (and tabnine-capf--response
                (> (length (alist-get 'results tabnine-capf--response)) 0)
                (alist-get 'old_prefix tabnine-capf--response))))
      (unless (or prefix
                  tabnine-capf-auto-fallback)
        (setq prefix 'stop))
      (if (and prefix
               tabnine-capf-always-trigger)
          (cons prefix t)
        prefix))))

(defun tabnine-capf--annotation(candidate)
  "Fetch the annotation text-property from a CANDIDATE string."
  (when tabnine-capf-show-annotation
    (-if-let (annotation (get-text-property 0 'annotation candidate))
        annotation
      (let ((kind (get-text-property 0 'kind candidate))
            ;; (return-type (get-text-property 0 'return_type candidate))
            (params (get-text-property 0 'params candidate)))
        (when kind
          (concat params
                  ;; (when (s-present? return-type)
                  ;;   (s-prepend " -> " return-type))
                  (when (s-present? kind)
                    (format " [%s]" kind))))))))

(defun tabnine-capf--kind-to-type (kind)
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

(defun tabnine-capf--construct-candidate-generic (candidate)
  "Generic function to construct completion string from a CANDIDATE."
  (tabnine-capf--with-destructured-candidate candidate))

(defun tabnine-capf--construct-candidates (results construct-candidate-fn)
  "Use CONSTRUCT-CANDIDATE-FN to construct a list of candidates from RESULTS."
  (let ((completions (mapcar construct-candidate-fn results)))
    (when completions
      (setq tabnine-capf--restart-count 0))
    completions))

(defun tabnine-capf--get-candidates (response)
  "Get candidates for RESPONSE."
  (tabnine-capf--construct-candidates
   (alist-get 'results response)
   #'tabnine-capf--construct-candidate-generic))

(defun tabnine-capf--candidates (prefix)
  "Candidates-command handler for the company backend for PREFIX.

Return completion candidates.  Must be called after `tabnine-capf-query'."
  (tabnine-capf--get-candidates tabnine-capf--response))

(defun tabnine-capf--meta (candidate)
  "Return meta information for CANDIDATE.  Currently used to display user messages."
  (if (null tabnine-capf--response)
      nil
    (let ((meta (get-text-property 0 'meta candidate)))
      (if (stringp meta)
          (let ((meta-trimmed (s-trim meta)))
            meta-trimmed)

        (let ((messages (alist-get 'user_message tabnine-capf--response)))
          (when messages
            (s-join " " messages)))))))

(defun tabnine-capf--post-completion (candidate)
  "Replace old suffix with new suffix for CANDIDATE."
  (when tabnine-capf-auto-balance
    (let ((old_suffix (get-text-property 0 'old_suffix candidate))
          (new_suffix (get-text-property 0 'new_suffix candidate)))
      (delete-region (point)
                     (min (+ (point) (length old_suffix))
                          (point-max)))
      (when (stringp new_suffix)
        (save-excursion
          (insert new_suffix))))))

;;
;; Interactive functions
;;

(defun tabnine-capf-restart-server ()
  "Start/Restart TabNine server."
  (interactive)
  (tabnine-capf-start-process))

(defun tabnine-capf-install-binary ()
  "Install TabNine binary into `tabnine-capf-binaries-folder'."
  (interactive)
  (let ((version-tempfile (concat
                           (file-name-as-directory
                            tabnine-capf-binaries-folder)
                           tabnine-capf--version-tempfile))
        (target (tabnine-capf--get-target))
        (exe (tabnine-capf--get-exe))
        (binaries-dir tabnine-capf-binaries-folder))
    (message version-tempfile)
    (message "Getting current version...")
    (make-directory (file-name-directory version-tempfile) t)
    (url-copy-file "https://update.tabnine.com/bundles/version" version-tempfile t)
    (let ((version (s-trim (with-temp-buffer (insert-file-contents version-tempfile) (buffer-string)))))
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

(defvar-local tabnine-capf--begin-pos nil)

;;;###autoload
(defun tabnine-completion-at-point ()
  "TabNine Completion at point function."
  (unless (or (and tabnine-capf-no-continue
                   tabnine-capf--calling-continue)
              tabnine-capf--disabled)
    (tabnine-capf-query))
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (thing (thing-at-point 'symbol))
         (candidates (tabnine-capf--candidates thing))
         (get-candidates (lambda () candidates)))
    (setq-local tabnine-capf--begin-pos (or (car bounds) (point)))
    (list
     (or (car bounds) (point))
     (or (cdr bounds) (point))
     candidates
     :exclusive 'no
     :company-kind (lambda (_) (intern "tabnine"))
     :annotation-function
     (lambda (candidate)
       "Extract integer from company-tabnine's CANDIDATE."
       (concat "  "(get-text-property 0 'annotation candidate)))
     :exit-function
     (lambda (candidate status)
       "Post-completion function for tabnine."
       (let ((item (cl-find candidate (funcall get-candidates) :test #'string=)))
         (tabnine-capf--post-completion item)
         )
       )
     )))

(defun tabnine-test-at-point ()
  "TabNine Completion at point function."
  (interactive)
  (unless (or (and tabnine-capf-no-continue
                   tabnine-capf--calling-continue)
              tabnine-capf--disabled)
    (tabnine-capf-query))
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (thing (thing-at-point 'symbol))
         (candidates (tabnine-capf--candidates thing))
         (get-candidates (lambda () candidates)))
    (setq-local tabnine-capf--begin-pos (or (car bounds) (point)))
    (debug)
    "haha"))
;;
;; Advices
;;


;;
;; Hooks
;;

(provide 'tabnine-capf)

;;; tabnine-capf.el ends here
