;;; lsp-bridge-code-action.el --- Code action for lsp-bridge   -*- lexical-binding: t; -*-

;; Filename: lsp-bridge-code-action.el
;; Description: Code action for lsp-bridge
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-12-14 18:10:57
;; Version: 0.1
;; Last-Updated: 2022-12-14 18:10:57
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/lsp-bridge-code-action
;; Keywords:
;; Compatibility: GNU Emacs 28.2
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
;; Code action for lsp-bridge
;;

;;; Installation:
;;
;; Put lsp-bridge-code-action.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lsp-bridge-code-action)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET lsp-bridge-code-action RET
;;

;;; Change log:
;;
;; 2022/12/14
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
(require 'acm)
(require 'acm-frame)
(require 'lsp-bridge-call-hierarchy)

;;; Code:

(defcustom lsp-bridge-code-action-preview-delay 0.5
  "The delay to show code action diff preview.

Default is 0.5 second, preview window will stick if this value too small."
  :type 'float
  :group 'lsp-bridge)

(defvar-local lsp-bridge-code-action-notify nil)

(defvar lsp-bridge-code-action--current-buffer nil)
(defvar lsp-bridge-code-action-popup-maybe-preview-timer nil)
(defvar lsp-bridge-code-action--oldfile nil)
(defvar lsp-bridge-code-action--preview-alist nil)
(defvar lsp-bridge-code-action--preview-index nil)

(defun lsp-bridge-code-action-get-range ()
  (cond ((region-active-p)
         (cons (region-beginning) (region-end)))
        (t
         (let ((overlay (lsp-bridge-diagnostic-overlay-at-point)))
           (if overlay
               (cons (overlay-start overlay) (overlay-end overlay))
             (cons (point) (point)))))))

(defun lsp-bridge-code-action (&optional action-kind)
  "Send code action to LSP server to fixes code or problems.

Default request all kind code-action.

You can send speical kind of code-action, parameter `action-kind' can use one of below:

'quickfix'
'refactor'
'refactor.extract'
'refactor.inline'
'refactor.rewrite'
'source'
'source.organizeImports'
'source.fixAll'

Please read https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#codeActionKind for detail explain.
"
  (interactive)
  (when (lsp-bridge-has-lsp-server-p)
    (let ((range (lsp-bridge-code-action-get-range)))
      (lsp-bridge-call-file-api "try_code_action"
                                (lsp-bridge--point-position (car range))
                                (lsp-bridge--point-position (cdr range))
                                action-kind)

      (setq-local lsp-bridge-code-action-notify t)
      )))

(defun lsp-bridge-code-action-popup-select ()
  (interactive)
  (lsp-bridge-code-action-popup-quit)
  (lsp-bridge-code-action--fix-do
   (cdr (nth lsp-bridge-call-hierarchy--index lsp-bridge-call-hierarchy--popup-response))))

(defun lsp-bridge-code-action-popup-quit ()
  (interactive)
  (acm-cancel-timer lsp-bridge-code-action-popup-maybe-preview-timer)

  (acm-frame-delete-frame lsp-bridge-call-hierarchy--frame)
  (kill-buffer "*lsp-bridge-code-action-menu*")

  (advice-remove 'lsp-bridge-call-hierarchy-maybe-preview #'lsp-bridge-code-action-popup-maybe-preview)
  (advice-remove 'lsp-bridge-call-hierarchy-select #'lsp-bridge-code-action-popup-select)
  (advice-remove 'lsp-bridge-call-hierarchy-quit #'lsp-bridge-code-action-popup-quit)
  (select-window (get-buffer-window lsp-bridge-code-action--current-buffer)))

(defun lsp-bridge-code-action-popup-maybe-preview-show (&optional buffer-content)
  (when-let* ((pos (frame-position lsp-bridge-call-hierarchy--frame))
              (call-frame-height (frame-height lsp-bridge-call-hierarchy--frame))
              (call-frame-width (frame-width lsp-bridge-call-hierarchy--frame))
              (width 0) (height 0)
              (window-min-height 0) (window-min-width 0)
              (live-p (frame-live-p lsp-bridge-call-hierarchy--frame)))
    (with-current-buffer  "*lsp-bridge-code-action-preview*"
      (read-only-mode -1)

      (cond
       (buffer-content
        (delete-region (point-min) (point-max))
        (insert buffer-content))
       (t
        (goto-char (point-min))
        ;; delete info lines
        (kill-line 3)
        ;; delete finish
        (goto-char (point-min))
        (when (search-forward "Diff finished." nil t)
          (beginning-of-line)
          (kill-line 1))
        (push (cons lsp-bridge-code-action--preview-index (buffer-string))
              lsp-bridge-code-action--preview-alist)))

      ;; get width and height
      (goto-char (point-min))
      (while (not (eobp))
        (cl-incf height)
        (setq width (max (- (line-end-position)
                            (line-beginning-position))
                         width))
        (forward-line))
      (setq width (min width 80))
      ;; show top
      (goto-char (point-min))
      ;; truncate lines
      (setq-local cursor-type nil)
      (setq-local mode-line-format nil)
      (setq-local truncate-lines t))

    (acm-frame-set-frame-size lsp-bridge-call-hierarchy--frame (max width call-frame-width)
                              (+ height (length lsp-bridge-call-hierarchy--popup-response)))
    (acm-frame-adjust-frame-pos lsp-bridge-call-hierarchy--frame)
    (select-frame-set-input-focus lsp-bridge-call-hierarchy--frame)

    (let ((menu-window (get-buffer-window "*lsp-bridge-code-action-menu*"
                                          lsp-bridge-call-hierarchy--frame))
          (frame-window-list (window-list lsp-bridge-call-hierarchy--frame))
          (nodiff (= height 0)))
      (if nodiff
          ;; for code action based on execute_command, we are not support preview
          (delete-other-windows menu-window)
        ;; split preview window
        (when (eq (length frame-window-list) 1)
          (split-window (selected-window) height 'below)
          (setq frame-window-list (window-list lsp-bridge-call-hierarchy--frame)))
        ;; set preview buffer
        (dolist (window frame-window-list)
          (unless (eq window menu-window)
            (set-window-buffer window "*lsp-bridge-code-action-preview*")))
        (select-window menu-window)
        (set-window-text-height menu-window
                                (length lsp-bridge-call-hierarchy--popup-response))))))

(defun lsp-bridge-code-action-popup-maybe-preview-do ()
  (let* ((buffer (buffer-name lsp-bridge-code-action--current-buffer))
         (buffer-content (with-current-buffer buffer (buffer-string)))
         (recentf-keep '(".*" . nil)) ;; not push temp file in recentf-list
         (recentf-exclude '(".*"))
         (action (cdr (nth lsp-bridge-call-hierarchy--index
                           lsp-bridge-call-hierarchy--popup-response)))
         (cache-buffer-string (alist-get lsp-bridge-call-hierarchy--index
                                         lsp-bridge-code-action--preview-alist)))

    (setq lsp-bridge-code-action--preview-index lsp-bridge-call-hierarchy--index)
    (if cache-buffer-string
        ;; use cache
        (lsp-bridge-code-action-popup-maybe-preview-show cache-buffer-string)
      ;; diff
      (with-temp-buffer
        (insert buffer-content)
        (lsp-bridge-code-action--fix-do action (current-buffer))
        (diff-no-select lsp-bridge-code-action--current-buffer (current-buffer)
                        nil nil (get-buffer-create "*lsp-bridge-code-action-preview*")))

      (if-let ((proc (get-buffer-process "*lsp-bridge-code-action-preview*")))
          (set-process-sentinel
           proc (lambda (proc _msg)
                  (with-current-buffer (process-buffer proc)
                    (diff-sentinel (process-exit-status proc))
                    (lsp-bridge-code-action-popup-maybe-preview-show))))
        (lsp-bridge-code-action-popup-maybe-preview-show)))))

(defun lsp-bridge-code-action-popup-maybe-preview ()
  (acm-cancel-timer lsp-bridge-code-action-popup-maybe-preview-timer)
  (setq lsp-bridge-code-action-popup-maybe-preview-timer
        (run-with-idle-timer lsp-bridge-code-action-preview-delay nil #'lsp-bridge-code-action-popup-maybe-preview-do)))

(defun lsp-bridge-code-action-popup-menu (menu-items default-action)
  (let ((recentf-keep '(".*" . nil)) ;; not push temp file in recentf-list
        (recentf-exclude '(".*"))
        (menu-lenght (length menu-items))
        (menu-buffer (get-buffer-create "*lsp-bridge-code-action-menu*"))
        (menu-width 0)
        (cursor (acm-frame-get-popup-position (point))))
    ;; prepare for previewing
    (setq lsp-bridge-code-action--current-buffer (current-buffer))

    (setq lsp-bridge-code-action--oldfile (make-temp-file
                                           (buffer-name) nil nil (buffer-string)))
    (setq lsp-bridge-code-action--preview-alist '())

    ;; reuse hierarchy popup keymap and mode here
    (setq lsp-bridge-call-hierarchy--popup-response menu-items)

    (acm-frame-create-frame-if-not-exist lsp-bridge-call-hierarchy--frame
                                         menu-buffer "code action" 0 nil)

    (with-current-buffer menu-buffer
      (cl-loop for i from 0 to (1- (length menu-items))
               do (let* ((title (car (nth i menu-items)))
                         (format-line (format "%d. %s\n" (1+ i) title))
                         (line-width (length format-line)))
                    (insert format-line)
                    (setq menu-width (max line-width menu-width))))
      (lsp-bridge-call-hierarchy-mode)
      (goto-char (point-min))
      (setq-local cursor-type nil)
      (setq-local truncate-lines t)
      (setq-local mode-line-format nil))

    (acm-frame-set-frame-position lsp-bridge-call-hierarchy--frame (car cursor) (+ (cdr cursor) (line-pixel-height)))
    (acm-frame-set-frame-size lsp-bridge-call-hierarchy--frame menu-width menu-lenght)

    (advice-add 'lsp-bridge-call-hierarchy-maybe-preview :override #'lsp-bridge-code-action-popup-maybe-preview)
    (advice-add 'lsp-bridge-call-hierarchy-select :override #'lsp-bridge-code-action-popup-select)
    (advice-add 'lsp-bridge-call-hierarchy-quit :override #'lsp-bridge-code-action-popup-quit)

    (lsp-bridge-code-action-popup-maybe-preview)
    ))


(defun lsp-bridge-code-action--fix-do (action &optional temp-buffer)
  (let* ((command (plist-get action :command))
         (edit (plist-get action :edit))
         (arguments (plist-get action :arguments)))
    (cond (edit
           (lsp-bridge-workspace-apply-edit edit temp-buffer))
          (arguments
           (dolist (argument arguments)
             (lsp-bridge-workspace-apply-edit argument temp-buffer)))
          (command
           (let (arguments)
             ;; Pick command and arguments.
             (cond ((consp command)
                    (setq arguments (plist-get command :arguments))
                    (setq command (plist-get command :command)))
                   ((stringp command)
                    (setq arguments (plist-get action :arguments))))

             (if (member command lsp-bridge-apply-edit-commands)
                 ;; Apply workspace edit if command match `lsp-bridge-apply-edit-commands'.
                 (dolist (argument arguments)
                   (lsp-bridge-workspace-apply-edit argument temp-buffer))
               ;; Otherwise send `workspace/executeCommand' request to LSP server.
               ;; TODO execute_command with temp-buffer
               (lsp-bridge-call-file-api "execute_command" command)))))
    (unless temp-buffer
      (message "[LSP-BRIDGE] Execute code action '%s'" (plist-get action :title)))))

(defun lsp-bridge-code-action--fix (actions action-kind)
  (let* ((menu-items
          (or
           (cl-remove-if #'null (mapcar #'(lambda (action)
                                         (when (or (not action-kind)
                                                   (equal action-kind (plist-get action :kind)))
                                           (cons (plist-get action :title) action)))
                                     actions))
           (apply #'error
                  (if action-kind
                      (format "No '%s' code action here" action-kind)
                    "No code actions here"))))
         (preferred-action (cl-find-if
                            (lambda (menu-item)
                              (plist-get (cdr menu-item) :isPreferred))
                            menu-items))
         (default-action (car (or preferred-action (car menu-items))))
         (action (if (and action-kind (null (cadr menu-items)))
                     (cdr (car menu-items)) nil)))
    (cond
     (action
      (lsp-bridge-code-action--fix-do action))
     ((posframe-workable-p) ;;posframe
      (lsp-bridge-code-action-popup-menu menu-items default-action))
     (t
      (let ((select-name
             (completing-read
              (format "[LSP-BRIDGE] Pick an action (default: %s): " default-action)
              menu-items nil t nil nil default-action)))
        (lsp-bridge-code-action--fix-do (cdr (assoc select-name menu-items))))))))

(provide 'lsp-bridge-code-action)

;;; lsp-bridge-code-action.el ends here
