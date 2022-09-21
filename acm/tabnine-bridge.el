;; tabnine-bridge.el --- A lsp-bridge backend for TabNine ;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2022 Tommy Xiang, John Gong
;;
;; Author: Tommy Xiang <tommyx058@gmail.com>
;;         John Gong <gjtzone@hotmail.com>
;; Keywords: convenience
;; Version: 0.0.1
;; Package-Requires: ((emacs "25") (cl-lib "0.5") (dash "2.16.0"))
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
;; A bridge verison of `company-tabnine`.
;;
;; Installation:
;;
;; 1. Add `tabnine-completion-at-point` to `completion-at-point-functions`
;;    (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)
;; 2. Run M-x tabnine-bridge-install-binary to install the TabNine binary for your system.
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

;;  TODO:
;; [  ] make the tabnine-bridge show up automatically
;; [  ] start the tabnine more naturally
;; [  ] check whether prefix influences the results
;; [  ] refactor the code, especially the name of variables


(require 'cl-lib)
(require 'url)
(require 'dash)
(add-to-list 'load-path (expand-file-name "tabnine" (file-name-directory load-file-name)))
(require 'tabnine-epc)

;;
;; Constants
;;

(defconst tabnine-bridge--hooks-alist
  ;; '((after-change-functions . tabnine-bridge-query))
  nil
  )

(defconst tabnine-bridge-python-file (expand-file-name "tabnine/tabnine.py" (file-name-directory load-file-name)))

;;
;; Customization
;;

(defgroup tabnine-bridge nil
  "Options for tabnine-bridge."
  :group 'company
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

(defcustom tabnine-bridge-auto-balance t
  "Whether TabNine should insert balanced parentheses upon completion."
  :group 'tabnine-bridge
  :type 'boolean)

(defcustom tabnine-bridge-show-annotation t
  "Whether to show an annotation inline with the candidate."
  :group 'tabnine-bridge
  :type 'boolean)

;;
;; Faces
;;

;;
;; Variables
;;

(defvar tabnine-bridge--process nil
  "TabNine server process.")

(defvar tabnine-bridge--response nil
  "Temporarily stored TabNine server responses.")

(defvar tabnine-bridge--disabled nil
  "Variable to temporarily disable tabnine-bridge and pass control to next backend.")

;;
;; Major mode definition
;;

;;
;; Global methods
;;

;;;###autoload
(defun tabnine-bridge-install-binary ()
  "Install Tabnine binary."
  (interactive)
  (when (tabnine-epc:live-p tabnine-bridge--process)
    (tabnine-epc:call-deferred tabnine-bridge--process 'install_tabnine nil)))

(defun tabnine-bridge-start-process ()
  "Start TabNine process."
  (setq tabnine-bridge--disabled t)
  (tabnine-bridge-kill-process)
  (let ((process-connection-type nil))
    (setq tabnine-bridge--process
          (tabnine-epc:start-epc "python3"
                                 (list tabnine-bridge-python-file)))
    (tabnine-epc:define-method tabnine-bridge--process
                               'tabnine-bridge-callback
                               #'tabnine-bridge-callback))
  ;; hook setup
  (message "TabNine server started.")
  (dolist (hook tabnine-bridge--hooks-alist)
    (add-hook (car hook) (cdr hook)))
  (setq tabnine-bridge--disabled nil))

(defun tabnine-bridge-callback (&rest args)
  "Callback from python."
  (setq tabnine-bridge--response args))

(defun tabnine-bridge-kill-process ()
  "Kill TabNine process."
  (interactive)
  (when tabnine-bridge--process
    (let ((process tabnine-bridge--process))
      (setq tabnine-bridge--process nil)
      (tabnine-epc:stop-epc process)))
  ;; hook remove
  (dolist (hook tabnine-bridge--hooks-alist)
    (remove-hook (car hook) (cdr hook)))
  (setq tabnine-bridge--disabled t))

(defun tabnine-bridge-send-request (request)
  "Send REQUEST to TabNine server.  REQUEST needs to be JSON-serializable object."
  (when (null tabnine-bridge--process)
    (tabnine-bridge-start-process))
  (let* ((before (plist-get request :before))
         (after (plist-get request :after))
         (filename (plist-get request :filename))
         (region_includes_beginning (plist-get request :region_includes_beginning))
         (region_includes_end (plist-get request :region_includes_end))
         (max_num_results (plist-get request :max_num_results)))
    (tabnine-epc:call-deferred tabnine-bridge--process 'complete (list before after filename
                                                                     region_includes_beginning region_includes_end
                                                                     max_num_results))))

(defun tabnine-bridge--make-request ()
  "Create request body for method METHOD and parameters PARAMS."
  (let* ((buffer-min 1)
         (buffer-max (1+ (buffer-size)))
         (before-point
          (max (point-min) (- (point) tabnine-bridge-context-radius)))
         (after-point
          (min (point-max) (+ (point) tabnine-bridge-context-radius-after))))

    (list
     :before (buffer-substring-no-properties before-point (point))
     :after (buffer-substring-no-properties (point) after-point)
     :filename (or (buffer-file-name) nil)
     :region_includes_beginning (if (= before-point buffer-min)
                                    t nil)
     :region_includes_end (if (= after-point buffer-max)
                              t nil)
     :max_num_results tabnine-bridge-max-num-results)))

(defun tabnine-bridge-query ()
  "Query TabNine server for auto-complete."
  (interactive)
  (unless tabnine-bridge--disabled
    ;; (setq tabnine-bridge-last-change-tick (corfu--auto-tick))
    (let ((request (tabnine-bridge--make-request)))
      (tabnine-bridge-send-request request))))

(defun tabnine-bridge--annotation(candidate)
  "Fetch the annotation text-property from a CANDIDATE string."
  (when tabnine-bridge-show-annotation
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

(defun tabnine-bridge--candidates ()
  "Candidates-command handler for the company backend for PREFIX.

Return completion candidates.  Must be called after `tabnine-bridge-query'."
  (mapcar
   (lambda (item)
     (propertize
      (plist-get item :new_prefix)
      'old_suffix (plist-get item :old_suffix)
      'new_suffix (plist-get item :new_suffix)
      'annotation (or (plist-get item :detail) "")
      ))
   (plist-get tabnine-bridge--response :results)))

(defun tabnine-bridge--post-completion (candidate)
  "Replace old suffix with new suffix for CANDIDATE."
  (when tabnine-bridge-auto-balance
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

(defun tabnine-bridge-restart-server ()
  "Start/Restart TabNine server."
  (interactive)
  (tabnine-bridge-start-process))

(provide 'tabnine-bridge)

;;; tabnine-bridge.el ends here
