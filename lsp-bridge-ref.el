;;; lsp-bridge-ref.el --- Show references for lsp-bridge

;; Filename: lsp-bridge-ref.el
;; Description: Search and refacotry code with rg
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-08-26 14:22:12
;; Version: 5.6
;; Last-Updated: 2020-05-04 17:52:55
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/lsp-bridge-ref.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `cl-lib' `subr-x' `grep'
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
;;

;;; Installation:
;;
;;

;;; Customize:
;;
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

;;; Require
(require 'cl-lib)
(require 'subr-x)
(require 'grep)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Group ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup lsp-bridge-ref nil
  "Poup bottom window to show code references."
  :group 'lsp-bridge)

(defcustom lsp-bridge-ref-buffer "*lsp-bridge-ref*"
  "The buffer name of code references."
  :type 'string
  :group 'lsp-bridge-ref)

(defcustom lsp-bridge-ref-temp-buffer " *lsp-bridge-ref temp* "
  "The buffer name of clone temp buffer"
  :type 'string
  :group 'lsp-bridge-ref)

(defcustom lsp-bridge-ref-mode-hook '()
  "lsp-bridge-ref mode hook."
  :type 'hook
  :group 'lsp-bridge-ref)

(defcustom lsp-bridge-ref-flash-line-delay .3
  "How many seconds to flash `lsp-bridge-ref-font-lock-flash' after navigation.

Setting this to nil or 0 will turn off the indicator."
  :type 'number
  :group 'lsp-bridge-ref)

(defcustom lsp-bridge-ref-kill-temp-buffer-p t
  "Default this option is true, it will kill temp buffer when quit lsp-bridge-ref buffer.

A buffer will killed if it is open by lsp-bridge-ref and not edit by lsp-bridge-ref.

A buffer won't kill is it open before lsp-bridge-ref command start.
A buffer won't kill if buffer content is change by lsp-bridge-ref.

Anyway, you can set this option with nil if you don't like lsp-bridge-ref kill any buffer."
  :type 'boolean
  :group 'lsp-bridge-ref)

(defcustom lsp-bridge-ref-show-function-name-p t
  "View the function name when navigate in match line.

Default is enable, set this variable to nil if you don't like this feature."
  :type 'boolean
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-header-line-text
  '((t (:foreground "Green3" :bold t)))
  "Face for header line text."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-header-line-edit-mode
  '((t (:foreground "Gold" :bold t)))
  "Face for header line edit mode."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-command
  '((t (:foreground "Gray30" :bold t)))
  "Face for filepath."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-file
  '((t (:foreground "DodgerBlue" :bold t)))
  "Face for filepath."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-line-number
  '((t (:foreground "gray35")))
  "Face for line number."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-column-number
  '((t (:foreground "gray35")))
  "Face for column number."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-position-splitter
  '((t (:foreground "gray25")))
  "Face for position splitter."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-match
  '((t (:foreground "Gold3" :bold t)))
  "Face for keyword match."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-diagnostic
  '((t (:foreground "Red3" :bold t)))
  "Face for keyword diagnostic."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-mark-changed
  '((t (:foreground "White" :background "#007aff" :bold t)))
  "Face for keyword match."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-mark-deleted
  '((t (:foreground "#ff3b30" :bold t)))
  "Face for keyword match."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-flash
  '((t (:inherit highlight)))
  "Face to flash the current line."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-function-location
  '((t (:foreground "Gold" :bold t)))
  "Face for show function location."
  :group 'lsp-bridge-ref)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar lsp-bridge-ref-temp-visit-buffers nil
  "The temp visit buffers use to kill temp buffer after quit lsp-bridge-ref.")

(defvar lsp-bridge-ref-window-configuration-before-search nil
  "Save window configuration before search,
used to restore window configuration after finish search.")

(defvar lsp-bridge-ref-buffer-point-before-search nil
  "Save buffer point before search,
used to restore buffer point after finish search.")

(defvar lsp-bridge-ref-window-configuration-before-apply nil
  "Save window configuration before apply changed,
used to restore window configuration after apply changed.")

(defvar lsp-bridge-ref-hit-count 0
  "Search keyword hit counter.")

(defvar lsp-bridge-ref-regexp-file "^[/\\~].*\\|^[a-z]:.*"
  "Regexp to match filename.")

(defvar lsp-bridge-ref-regexp-split-line "\n\n"
  "Regexp to match empty line between two files.")

(defvar lsp-bridge-ref-regexp-position "^\\([0-9][0-9]*\\):\\([0-9][0-9]*\\):"
  "Regexp to match line/column string.")

(defvar lsp-bridge-ref-changed-lines nil
  "The list that record the changed lines.")

(defvar lsp-bridge-ref-read-input-history nil)

(defvar lsp-bridge-ref-files-history nil "History for files args.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; lsp-bridge-ref mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar lsp-bridge-ref-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") 'lsp-bridge-ref-beginning-of-line)
    (define-key map (kbd "<tab>") 'lsp-bridge-ref-jump-next-keyword)
    (define-key map (kbd "<backtab>") 'lsp-bridge-ref-jump-prev-keyword)

    (define-key map (kbd "j") 'lsp-bridge-ref-jump-next-keyword)
    (define-key map (kbd "k") 'lsp-bridge-ref-jump-prev-keyword)
    (define-key map (kbd "h") 'lsp-bridge-ref-jump-next-file)
    (define-key map (kbd "l") 'lsp-bridge-ref-jump-prev-file)
    (define-key map (kbd "i") 'lsp-bridge-ref-insert-current-line)

    (define-key map (kbd "SPC") 'lsp-bridge-ref-open-file)
    (define-key map (kbd "RET") 'lsp-bridge-ref-open-file-and-stay)
    (define-key map (kbd "C-m") 'lsp-bridge-ref-open-file-and-stay)

    (define-key map (kbd "e") 'lsp-bridge-ref-switch-to-edit-mode)

    (define-key map (kbd "r") 'lsp-bridge-ref-replace-all-matches)

    (define-key map (kbd "f") 'lsp-bridge-ref-filter-match-results)
    (define-key map (kbd "F") 'lsp-bridge-ref-filter-mismatch-results)

    (define-key map (kbd "x") 'lsp-bridge-ref-filter-match-files)
    (define-key map (kbd "X") 'lsp-bridge-ref-filter-mismatch-files)
    (define-key map (kbd "u") 'lsp-bridge-ref-unfilter)

    (define-key map (kbd "D") 'lsp-bridge-ref-remove-line-from-results)

    (define-key map (kbd "q") 'lsp-bridge-ref-quit)
    map)
  "Keymap used by `lsp-bridge-ref-mode'.")

(defvar lsp-bridge-ref-mode-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") 'lsp-bridge-ref-beginning-of-line)

    (define-key map (kbd "C-c C-j") 'lsp-bridge-ref-jump-next-keyword)
    (define-key map (kbd "C-c C-k") 'lsp-bridge-ref-jump-prev-keyword)
    (define-key map (kbd "C-c C-h") 'lsp-bridge-ref-jump-next-file)
    (define-key map (kbd "C-c C-l") 'lsp-bridge-ref-jump-prev-file)
    (define-key map (kbd "C-c <C-return>") 'lsp-bridge-ref-open-file)
    (define-key map (kbd "C-c C-v") 'lsp-bridge-ref-switch-to-view-mode)

    (define-key map (kbd "C-c C-d") 'lsp-bridge-ref-delete-line)
    (define-key map (kbd "C-c C-D") 'lsp-bridge-ref-delete-all-lines)
    (define-key map (kbd "C-c C-r") 'lsp-bridge-ref-recover-line)
    (define-key map (kbd "C-c C-R") 'lsp-bridge-ref-recover-buffer)
    (define-key map (kbd "C-c C-q") 'lsp-bridge-ref-quit)
    (define-key map (kbd "C-c C-c") 'lsp-bridge-ref-apply-changed)
    map)
  "Edit keymap used by `lsp-bridge-ref-mode'.")

(define-derived-mode lsp-bridge-ref-mode text-mode "lsp-bridge-ref"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'lsp-bridge-ref-mode)
  (setq mode-name "lsp-bridge-ref")
  (read-only-mode 1)
  (lsp-bridge-ref-highlight-keywords)
  (use-local-map lsp-bridge-ref-mode-map)
  (run-hooks 'lsp-bridge-ref-mode-hook)
  )

(defun lsp-bridge-ref-highlight-keywords ()
  "Highlight keywords."
  ;; Add keywords for highlight.
  (font-lock-add-keywords
   nil
   '(
     ("^rg\\s-.*" . 'lsp-bridge-ref-font-lock-command)
     ("^\\([1-9][0-9]*\\)\\(:\\)\\([0-9][0-9]*\\)\\(:\\)" 2 'lsp-bridge-ref-font-lock-position-splitter)
     ("^\\([1-9][0-9]*\\)\\(:\\)\\([0-9][0-9]*\\)\\(:\\)" 1 'lsp-bridge-ref-font-lock-line-number)
     ("^\\([1-9][0-9]*\\)\\(:\\)\\([0-9][0-9]*\\)\\(:\\)" 3 'lsp-bridge-ref-font-lock-column-number)
     ("^\\([1-9][0-9]*\\)\\(:\\)\\([0-9][0-9]*\\)\\(:\\)" 4 'lsp-bridge-ref-font-lock-position-splitter)
     ("^[/\\~].*\\|^[a-z]:.*" . 'lsp-bridge-ref-font-lock-file)
     ))
  ;; NOTE:
  ;; Because search line maybe just contains *half* of string/comment that make rest content of buffer mark as string.
  ;; So we need turn off comment/string font-lock through set `font-lock-keywords-only'.
  (set (make-local-variable 'font-lock-keywords-only) t)
  ;; Enable font lock.
  (font-lock-mode 1))

(defun lsp-bridge-ref-update-header-line ()
  (setq header-line-format (concat
                            (propertize (format "%s mode" (lsp-bridge-ref-search-mode lsp-bridge-ref-cur-search)) 'font-lock-face 'lsp-bridge-ref-font-lock-match)
                            (propertize (format " %s matches" lsp-bridge-ref-hit-count) 'font-lock-face 'lsp-bridge-ref-font-lock-header-line-text)
                            (propertize " [ " 'font-lock-face 'lsp-bridge-ref-font-lock-line-number)
                            (propertize "Nav " 'font-lock-face 'lsp-bridge-ref-font-lock-header-line-text)
                            (propertize
                             (substitute-command-keys
                              "\\<lsp-bridge-ref-mode-map> \\[lsp-bridge-ref-jump-next-keyword] / \\[lsp-bridge-ref-jump-prev-keyword] / \\[lsp-bridge-ref-jump-next-file] / \\[lsp-bridge-ref-jump-prev-file]" t)
                             'font-lock-face 'lsp-bridge-ref-font-lock-header-line-edit-mode)
                            (propertize "  Replace " 'font-lock-face 'lsp-bridge-ref-font-lock-header-line-text)
                            (propertize
                             (substitute-command-keys
                              "\\<lsp-bridge-ref-mode-map> \\[lsp-bridge-ref-replace-all-matches]" t)
                             'font-lock-face 'lsp-bridge-ref-font-lock-header-line-edit-mode)
                            (propertize "  Edit " 'font-lock-face 'lsp-bridge-ref-font-lock-header-line-text)
                            (propertize
                             (substitute-command-keys "\\<lsp-bridge-ref-mode-map> \\[lsp-bridge-ref-switch-to-edit-mode]" t)
                             'font-lock-face 'lsp-bridge-ref-font-lock-header-line-edit-mode)
                            (propertize "  Filter files: " 'font-lock-face 'lsp-bridge-ref-font-lock-header-line-text)
                            (propertize
                             (substitute-command-keys "\\<lsp-bridge-ref-mode-map> \\[lsp-bridge-ref-filter-match-files] / \\[lsp-bridge-ref-filter-mismatch-files] / \\[lsp-bridge-ref-unfilter]" t)
                             'font-lock-face 'lsp-bridge-ref-font-lock-header-line-edit-mode)
                            (propertize "  Filter regex: " 'font-lock-face 'lsp-bridge-ref-font-lock-header-line-text)
                            (propertize
                             (substitute-command-keys "\\<lsp-bridge-ref-mode-map> \\[lsp-bridge-ref-filter-match-results] / \\[lsp-bridge-ref-filter-mismatch-results]" t)
                             'font-lock-face 'lsp-bridge-ref-font-lock-header-line-edit-mode)
                            (propertize "  Remove " 'font-lock-face 'lsp-bridge-ref-font-lock-header-line-text)
                            (propertize
                             (substitute-command-keys "\\<lsp-bridge-ref-mode-map> \\[lsp-bridge-ref-remove-line-from-results]" t)
                             'font-lock-face 'lsp-bridge-ref-font-lock-header-line-edit-mode)
                            (propertize " ]" 'font-lock-face 'lsp-bridge-ref-font-lock-line-number)
                            )))

(cl-defstruct (lsp-bridge-ref-search (:constructor lsp-bridge-ref-search-create)
                                     (:constructor lsp-bridge-ref-search-new (pattern dir))
                                     (:copier nil))
  keyword           ; search keyword
  dir               ; base directory
  globs             ; filename only match these globs will be searched
  file-exclude ; toggle exclude files, t means filename NOT match the globs will be searched
  literal      ; literal patterh (t or nil)
  case-sensitive                        ; case-sensitive (t or nil)
  no-ignore                             ; toggle no-ignore (t or nil)
  no-node                               ; toggle no-node (t or nil)
  mode                                  ; view or edit mode
  )

(defvar lsp-bridge-ref-cur-search (lsp-bridge-ref-search-create)
  "Stores parameters of last search.
Becomes buffer local in `lsp-bridge-ref-mode' buffers.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utils functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lsp-bridge-ref-popup (references-content references-counter)
  "Rerun rg with customized arguments. This function will give
user more freedom to use rg with special arguments."
  (interactive)
  ;; Save window configuration before do search.
  ;; Just save when `lsp-bridge-ref-window-configuration-before-search' is nil
  ;; Or current buffer is not `lsp-bridge-ref-buffer' (that mean user not quit lsp-bridge-ref and search again in other place).
  (when (or (not lsp-bridge-ref-window-configuration-before-search)
            (not (string-equal (buffer-name) lsp-bridge-ref-buffer)))
    (setq lsp-bridge-ref-window-configuration-before-search (current-window-configuration))
    (setq lsp-bridge-ref-buffer-point-before-search (point)))

  ;; Init edit mode.
  (setf (lsp-bridge-ref-search-mode lsp-bridge-ref-cur-search) "View")

  ;; Reset visit temp buffers.
  (setq lsp-bridge-ref-temp-visit-buffers nil)
  ;; Reset hit count.
  (setq lsp-bridge-ref-hit-count references-counter)
  ;; Erase or create search result.
  (get-buffer-create lsp-bridge-ref-buffer)
  (with-current-buffer lsp-bridge-ref-buffer
    (let ((inhibit-read-only t))
      ;; Switch to `lsp-bridge-ref-mode' first, otherwise `erase-buffer' will cause "save-excursion: end of buffer" error.
      (lsp-bridge-ref-mode)

      ;; Erase buffer content.
      (read-only-mode -1)
      (erase-buffer)))
  (setq lsp-bridge-ref-changed-lines nil)

  ;; Run search command.
  (with-current-buffer lsp-bridge-ref-buffer
    (insert references-content)

    ;; "^\033\\[95m\\(.*?\\)\033\\[0m$"

    ;; Highlight file path.
    (goto-char (point-min))
    (while (re-search-forward "^\033\\[95m\\(.*?\\)\033\\[0m$" nil t)
      (replace-match (concat (propertize (match-string 1)
                                         'face nil 'font-lock-face 'lsp-bridge-ref-font-lock-file))
                     t t))

    ;; Highlight references.
    (goto-char (point-min))
    (while (re-search-forward "\033\\[94m\\(.*?\\)\033\\[0m" nil t)
      (replace-match (concat (propertize (match-string 1)
                                         'face nil 'font-lock-face 'lsp-bridge-ref-font-lock-match))
                     t t))

    ;; Highlight diagnostics.
    (goto-char (point-min))
    (while (re-search-forward "\\[93m\\(.*?\n?.*?\\)\\[0m" nil t)
      (replace-match (concat (propertize (match-string 1)
                                         'face nil 'font-lock-face 'lsp-bridge-ref-font-lock-diagnostic))
                     t t))

    (lsp-bridge-ref-update-header-line)

    (read-only-mode 1))

  ;; Pop search buffer.
  (delete-other-windows)
  ;; Set `window-resize-pixelwise' with non-nil will cause `split-window' failed.
  (let (window-resize-pixelwise)
    (split-window nil (* 0.618 (window-pixel-height)) nil t))
  (other-window 1)
  (switch-to-buffer lsp-bridge-ref-buffer)
  (goto-char (point-min)))

(defun lsp-bridge-ref-find-next-position (regexp)
  (save-excursion
    (end-of-line)
    (search-forward-regexp regexp nil t)))

(defun lsp-bridge-ref-get-match-file ()
  (save-excursion
    (search-backward-regexp lsp-bridge-ref-regexp-file nil t)
    (string-remove-suffix "\n" (thing-at-point 'line))))

(defun lsp-bridge-ref-get-match-line ()
  (beginning-of-line)
  (string-to-number (thing-at-point 'symbol)))

(defun lsp-bridge-ref-get-match-column ()
  (search-forward ":")
  (string-to-number (thing-at-point 'symbol)))

(defun lsp-bridge-ref-get-match-buffer (filepath)
  (catch 'find-match
    (dolist (buffer (buffer-list))
      (when (string-equal (buffer-file-name buffer) filepath)
        (throw 'find-match buffer)))
    nil))

(defun lsp-bridge-ref-current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun lsp-bridge-ref-get-line-content (buffer line)
  (with-current-buffer buffer
    (save-excursion
      (goto-line line)
      (beginning-of-line)
      (search-forward-regexp lsp-bridge-ref-regexp-position nil t)
      (setq start (point))
      (end-of-line)
      (setq end (point))
      (buffer-substring start end))
    ))

(defun lsp-bridge-ref-get-row-column-position ()
  (let* ((search-bound
          (save-excursion
            (end-of-line)
            (point)))
         (row-column-position
          (save-excursion
            (beginning-of-line)
            (search-forward-regexp lsp-bridge-ref-regexp-position search-bound t))))
    row-column-position))

(defun lsp-bridge-ref-after-change-function (beg end leng-before)
  ;; NOTE:
  ;; We should use `save-match-data' wrap function that hook in `after-change-functions'.
  ;; Otherwise will got error: "replace-match-maybe-edit: Match data clobbered by buffer modification hooks"
  (save-match-data
    (let* ((change-line (save-excursion
                          (goto-char beg)
                          (line-number-at-pos)))
           change-line-content
           original-line-content)
      (setq changed-line-content (lsp-bridge-ref-get-line-content lsp-bridge-ref-buffer change-line))
      (setq original-line-content (lsp-bridge-ref-get-line-content lsp-bridge-ref-temp-buffer change-line))
      (if (string-equal changed-line-content original-line-content)
          (progn
            (setq lsp-bridge-ref-changed-lines (remove change-line lsp-bridge-ref-changed-lines))
            (lsp-bridge-ref-mark-position-clear change-line))
        (add-to-list 'lsp-bridge-ref-changed-lines change-line)
        (if (string-equal changed-line-content "")
            (lsp-bridge-ref-mark-position-deleted change-line)
          (lsp-bridge-ref-mark-position-changed change-line)))
      )))

(defun lsp-bridge-ref-mark-position-clear (line)
  (save-excursion
    (goto-line line)
    (beginning-of-line)
    (forward-char)
    (dolist (overlay (overlays-at (point)))
      (when (or (string-equal (overlay-get overlay 'overlay-type) "changed")
                (string-equal (overlay-get overlay 'overlay-type) "deleted"))
        (delete-overlay overlay)
        ))))

(defun lsp-bridge-ref-mark-position (line type face)
  (save-excursion
    (lsp-bridge-ref-mark-position-clear line)
    ;; Create mark changed overlay if not exists.
    (let (start end)
      (save-excursion
        (beginning-of-line)
        (setq start (point))
        (end-of-line)
        (setq end (point))
        (setq changed-overlay (make-overlay start end))
        (overlay-put changed-overlay 'overlay-type type)
        (overlay-put changed-overlay 'face face)
        ))))

(defun lsp-bridge-ref-mark-position-changed (line)
  (lsp-bridge-ref-mark-position line "changed" 'lsp-bridge-ref-font-lock-mark-changed))

(defun lsp-bridge-ref-mark-position-deleted (line)
  (lsp-bridge-ref-mark-position line "deleted" 'lsp-bridge-ref-font-lock-mark-deleted))

(defun lsp-bridge-ref-kill-temp-buffer ()
  (when (get-buffer lsp-bridge-ref-temp-buffer)
    (kill-buffer lsp-bridge-ref-temp-buffer)
    (setq lsp-bridge-ref-changed-lines nil)))

(defun lsp-bridge-ref-clone-to-temp-buffer ()
  (lsp-bridge-ref-kill-temp-buffer)
  (with-current-buffer lsp-bridge-ref-buffer
    (add-hook 'kill-buffer-hook 'lsp-bridge-ref-kill-temp-buffer nil t)
    (generate-new-buffer lsp-bridge-ref-temp-buffer)
    (append-to-buffer lsp-bridge-ref-temp-buffer (point-min) (point-max))
    ))

(defun lsp-bridge-ref-switch-to-view-mode ()
  (interactive)
  (with-current-buffer lsp-bridge-ref-buffer
    ;; Do clean work.
    (dolist (line lsp-bridge-ref-changed-lines)
      (lsp-bridge-ref-mark-position-clear line))
    (setq lsp-bridge-ref-changed-lines nil)
    (lsp-bridge-ref-kill-temp-buffer)
    (remove-hook 'after-change-functions 'lsp-bridge-ref-after-change-function t)
    ;; Switch to view mode.
    (read-only-mode 1)
    (use-local-map lsp-bridge-ref-mode-map)
    (kill-local-variable 'query-replace-skip-read-only)
    (setf (lsp-bridge-ref-search-mode lsp-bridge-ref-cur-search) "View")
    (lsp-bridge-ref-update-header-line)
    ))

(defun lsp-bridge-ref-filter-results (match-regexp)
  (let ((filter-regexp (read-string
                        (format (if match-regexp
                                    "Filter result match regexp: "
                                  "Filter result not match regexp: ")))))
    (save-excursion
      (with-current-buffer lsp-bridge-ref-buffer
        (setq remove-counter 0)
        (goto-char (point-min))
        (while (setq start (search-forward-regexp lsp-bridge-ref-regexp-position nil t))
          (setq line-content (lsp-bridge-ref-get-line-content lsp-bridge-ref-buffer (line-number-at-pos)))
          (if match-regexp
              (unless (string-match filter-regexp line-content)
                (lsp-bridge-ref-remove-line-from-results)
                (setq remove-counter (+ 1 remove-counter))
                )
            (when (string-match filter-regexp line-content)
              (lsp-bridge-ref-remove-line-from-results)
              (setq remove-counter (+ 1 remove-counter))
              )))
        (if match-regexp
            (message (format "[LSP-Bridge] Remove %s lines not match regexp '%s'." remove-counter filter-regexp))
          (message (format "[LSP-Bridge] Remove %s lines match regexp '%s'." remove-counter filter-regexp)))
        )))
  ;; Update hit number in header line.
  (lsp-bridge-ref-update-header-line-hits))

(defun lsp-bridge-ref-file-extension (file)
  (string-join (cdr (split-string (file-name-nondirectory file) "\\.")) "."))

(defun lsp-bridge-ref-filter-files (match-files)
  (let (file-extensions start end)
    (save-excursion
      (goto-char (point-min))
      (while (setq end (search-forward-regexp lsp-bridge-ref-regexp-file nil t))
        (beginning-of-line)
        (setq start (point))
        (setq filename (buffer-substring-no-properties start end))
        (end-of-line)
        (add-to-list 'file-extensions (lsp-bridge-ref-file-extension filename))))
    (if (< (length file-extensions) 2)
        (message (format "[LSP-Bridge] Has one type files now."))
      (setq filter-extension (ido-completing-read (if match-files
                                                      "Only display file suffix with: "
                                                    "Remove file suffix with: ")
                                                  file-extensions))
      (save-excursion
        (with-current-buffer lsp-bridge-ref-buffer
          (setq remove-counter 0)
          (goto-char (point-min))
          (while (setq end (search-forward-regexp lsp-bridge-ref-regexp-file nil t))
            (beginning-of-line)
            (setq start (point))
            (setq file-extension (lsp-bridge-ref-file-extension (buffer-substring-no-properties start end)))
            (if match-files
                (if (string-equal file-extension filter-extension)
                    (end-of-line)
                  (lsp-bridge-ref-remove-lines-under-file))
              (if (string-equal file-extension filter-extension)
                  (lsp-bridge-ref-remove-lines-under-file)
                (end-of-line))))
          ))))
  ;; Update hit number in header line.
  (lsp-bridge-ref-update-header-line-hits))

(defun lsp-bridge-ref-remove-lines-under-file ()
  (let (start end)
    (save-excursion
      (with-current-buffer lsp-bridge-ref-buffer
        (read-only-mode -1)
        (beginning-of-line)
        (setq start (point))
        (when (search-forward-regexp lsp-bridge-ref-regexp-split-line nil t)
          (setq end (point))
          (kill-region start end))
        (read-only-mode 1)))))

(defun lsp-bridge-ref-update-header-line-hits ()
  (setq lsp-bridge-ref-hit-count (lsp-bridge-ref-stat-hits))
  (lsp-bridge-ref-update-header-line))

(defun lsp-bridge-ref-stat-hits ()
  (let ((hit-count 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((plist (text-properties-at (point)))
              (next-change
               (or (next-property-change (point) (current-buffer))
                   (point-max))))
          (dolist (property plist)
            (when (string-equal (format "%s" property) "lsp-bridge-ref-font-lock-match")
              (setq hit-count (+ hit-count 1))))
          (goto-char next-change)))
      hit-count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'lsp-bridge-ref-search-input-in-project 'lsp-bridge-ref-search-project)

(defun lsp-bridge-ref-replace-all-matches ()
  (interactive)
  (save-excursion
    (let (changed-line-number)
      (let ((inhibit-message t)) ; don't flush to echo area when apply changed, optimise for lsp-bridge-ref
        (with-current-buffer lsp-bridge-ref-buffer
          (let* ((search-keyword (lsp-bridge-ref-search-keyword lsp-bridge-ref-cur-search))
                 (replace-text (read-string (format "Replace '%s' all matches with: " search-keyword) search-keyword)))
            (lsp-bridge-ref-switch-to-edit-mode)
            (if (lsp-bridge-ref-search-literal lsp-bridge-ref-cur-search)
                (query-replace search-keyword replace-text nil (point-min) (point-max))
              (query-replace-regexp search-keyword replace-text nil (point-min) (point-max)))
            (setq changed-line-number (length lsp-bridge-ref-changed-lines))
            (lsp-bridge-ref-apply-changed)
            (lsp-bridge-ref-switch-to-view-mode)
            (setf (lsp-bridge-ref-search-keyword lsp-bridge-ref-cur-search) replace-text)
            )))
      (message "[LSP-Bridge] Replace %s lines" changed-line-number))))

(defun lsp-bridge-ref-filter-match-results ()
  (interactive)
  (lsp-bridge-ref-filter-results t))

(defun lsp-bridge-ref-filter-mismatch-results ()
  (interactive)
  (lsp-bridge-ref-filter-results nil))

(defun lsp-bridge-ref-filter-match-files ()
  (interactive)
  (lsp-bridge-ref-filter-files t))

(defun lsp-bridge-ref-filter-mismatch-files ()
  (interactive)
  (lsp-bridge-ref-filter-files nil))

(defun lsp-bridge-ref-unfilter ()
  (interactive)
  (save-excursion
    (with-current-buffer lsp-bridge-ref-buffer
      (let ((inhibit-read-only t))
        (lsp-bridge-ref-mode) ; switch to `lsp-bridge-ref-mode' first, otherwise `erase-buffer' will cause "save-excursion: end of buffer" error.
        (read-only-mode -1)
        (erase-buffer)
        (insert (with-current-buffer lsp-bridge-ref-temp-buffer
                  (buffer-substring (point-min) (point-max))))
        (read-only-mode 1)
        )
      ;; Update hit number in header line.
      (lsp-bridge-ref-update-header-line-hits))))

(defun lsp-bridge-ref-remove-line-from-results ()
  (interactive)
  (save-excursion
    (with-current-buffer lsp-bridge-ref-buffer
      (when (lsp-bridge-ref-get-row-column-position)
        (read-only-mode -1)
        (beginning-of-line)
        (kill-line)
        (kill-line)
        (read-only-mode 1)
        ))))

(defun lsp-bridge-ref-jump-next-keyword ()
  (interactive)
  (let* ((next-position (lsp-bridge-ref-find-next-position lsp-bridge-ref-regexp-position)))
    (if next-position
        (progn
          (goto-char next-position)
          (lsp-bridge-ref-open-file))
      (message "[LSP-Bridge] Reach to last line."))))

(defun lsp-bridge-ref-jump-prev-keyword ()
  (interactive)
  (let ((prev-match-pos
         (if (save-excursion (search-backward-regexp lsp-bridge-ref-regexp-position nil t))
             (let* ((first-search-line
                     (save-excursion
                       (search-backward-regexp lsp-bridge-ref-regexp-position nil t)
                       (line-number-at-pos))))
               (if (equal first-search-line (line-number-at-pos))
                   ;; Search previous again if first search is same line of point.
                   (save-excursion
                     (beginning-of-line)
                     (search-backward-regexp lsp-bridge-ref-regexp-position nil t))
                 (save-excursion (search-backward-regexp lsp-bridge-ref-regexp-position nil t)))
               )
           nil)))
    (if prev-match-pos
        (progn
          (goto-char prev-match-pos)
          (lsp-bridge-ref-open-file))
      (message "[LSP-Bridge] Reach to first line."))))

(defun lsp-bridge-ref-jump-next-file ()
  (interactive)
  (let*  ((next-position (lsp-bridge-ref-find-next-position lsp-bridge-ref-regexp-file)))
    (if next-position
        (progn
          (goto-char next-position)
          (forward-line)
          (lsp-bridge-ref-open-file))
      (message "[LSP-Bridge] Reach to last file."))))

(defun lsp-bridge-ref-jump-prev-file ()
  (interactive)
  (let ((prev-match-pos
         (if (save-excursion (search-backward-regexp lsp-bridge-ref-regexp-file nil t))
             (let* ((first-search-line
                     (save-excursion
                       (search-backward-regexp lsp-bridge-ref-regexp-file nil t)
                       (line-number-at-pos)))
                    (prev-empty-line
                     (save-excursion
                       (search-backward-regexp lsp-bridge-ref-regexp-split-line)
                       (line-number-at-pos))))
               (if (and (> first-search-line prev-empty-line)
                        (not (lsp-bridge-ref-current-line-empty-p)))
                   ;; Search filename previous again if first search is current file result area.
                   (save-excursion
                     (search-backward-regexp lsp-bridge-ref-regexp-split-line)
                     (search-backward-regexp lsp-bridge-ref-regexp-file nil t))
                 (save-excursion (search-backward-regexp lsp-bridge-ref-regexp-file nil t)))
               )
           nil)))
    (if prev-match-pos
        (progn
          (goto-char prev-match-pos)
          (forward-line)
          (lsp-bridge-ref-open-file))
      (message "[LSP-Bridge] Reach to first file."))))

(defun lsp-bridge-ref-insert-current-line ()
  (interactive)
  (let ((current-line (save-excursion
                        (beginning-of-line)
                        (search-forward-regexp lsp-bridge-ref-regexp-position nil t)
                        (setq start (point))
                        (end-of-line)
                        (setq end (point))
                        (buffer-substring start end)
                        )))
    (lsp-bridge-ref-quit)
    (insert current-line)))

(defun lsp-bridge-ref-open-file (&optional stay)
  (interactive)
  (let* ((match-file (lsp-bridge-ref-get-match-file))
         (match-line (lsp-bridge-ref-get-match-line))
         (match-column (lsp-bridge-ref-get-match-column))
         (match-buffer (lsp-bridge-ref-get-match-buffer match-file))
         in-org-link-content-p
         color-buffer-window)
    (save-excursion
      (let ((inhibit-message t))
        ;; Try fill variables when in org file.
        (lsp-bridge-ref-move-to-column match-column)
        (setq in-org-link-content-p
              (and (lsp-bridge-ref-is-org-file match-file)
                   (lsp-bridge-ref-in-org-link-content-p)))
        ;; Open file in other window.
        ;; Note, don't use `find-file-other-window', it will failed if path is tramp path that start with /sudo:root
        (other-window 1)
        (find-file match-file)
        ;; Add to temp list if file's buffer is not exist.
        (unless match-buffer
          (add-to-list 'lsp-bridge-ref-temp-visit-buffers (current-buffer)))
        ;; Jump to match point.
        ;; We use `ignore-errors' to make sure cursor will back to lsp-bridge-ref buffer
        ;; even target line is not exists in search file (such as delete by user).
        (ignore-errors
          (cond ((lsp-bridge-ref-is-org-file match-file)
                 ;; Jump to match position.
                 (lsp-bridge-ref-move-to-point match-line match-column)
                 ;; Expand org block if current file is *.org file.
                 (org-reveal)
                 ;; Jump to link beginning if keyword in content area.
                 (when in-org-link-content-p
                   (search-backward-regexp "\\[\\[" (line-beginning-position) t)))
                (t
                 (lsp-bridge-ref-move-to-point match-line match-column)))))
      ;; Flash match line.
      (lsp-bridge-ref-flash-line))
    (unless stay
      ;; Keep cursor in search buffer's window.
      (setq color-buffer-window (get-buffer-window lsp-bridge-ref-buffer))
      (if color-buffer-window
          (select-window color-buffer-window)
        ;; Split window and select if color-buffer is not exist in windows.
        (delete-other-windows)
        (split-window)
        (other-window 1)
        (switch-to-buffer lsp-bridge-ref-buffer)))
    ;; Ajust column position.
    (lsp-bridge-ref-move-to-column match-column)
    ))

(defun lsp-bridge-ref-open-file-and-stay ()
  (interactive)
  (lsp-bridge-ref-open-file t))

(defun lsp-bridge-ref-flash-line ()
  (let ((pulse-iterations 1)
        (pulse-delay lsp-bridge-ref-flash-line-delay))
    ;; Flash match line.
    (pulse-momentary-highlight-one-line (point) 'lsp-bridge-ref-font-lock-flash)
    ;; View the function name when navigate in match line.
    (when lsp-bridge-ref-show-function-name-p
      (require 'which-func)
      (let ((function-name (which-function)))
        (when function-name
          (message "[LSP-Bridge] Located in function: %s"
                   (propertize
                    function-name
                    'face 'lsp-bridge-ref-font-lock-function-location
                    )))))))

(defun lsp-bridge-ref-in-org-link-content-p ()
  (and (looking-back "\\[\\[.*" (line-beginning-position))
       (looking-at ".*\\]\\[")
       (looking-at ".*\\]\\]")))

(defun lsp-bridge-ref-is-org-file (file)
  (string-equal (lsp-bridge-ref-file-extension file) "org"))

(defun lsp-bridge-ref-move-to-point (line column)
  ;; Jump to match position.
  (goto-line line)
  (beginning-of-line)

  (lsp-bridge-ref-jump-to-column column))

(defun lsp-bridge-ref-move-to-column (column)
  (beginning-of-line)
  (search-forward-regexp lsp-bridge-ref-regexp-position)

  ;; Forward to column if current line is not empty line (delete by `lsp-bridge-ref-delete-line').
  (unless (looking-at "[[:space:]]*$")
    (lsp-bridge-ref-jump-to-column column)))

(defun lsp-bridge-ref-jump-to-column (column)
  "This function use for jump to correct column positions in multi-byte strings.
Such as, mixed string of Chinese and English.

Function `move-to-column' can't handle mixed string of Chinese and English correctly."
  (let ((scan-column 0)
        (first-char-point (point)))

    (while (> column scan-column)
      (forward-char 1)
      (setq scan-column (string-bytes (buffer-substring first-char-point (point)))))))

(defun lsp-bridge-ref-switch-to-edit-mode ()
  (interactive)
  ;; Clone content to temp buffer.
  (lsp-bridge-ref-clone-to-temp-buffer)
  ;; Update header-line.
  (setf (lsp-bridge-ref-search-mode lsp-bridge-ref-cur-search) "Edit")

  ;; Set `query-replace-skip-read-only' to avoid read-only error when do `query-replace'.
  (set (make-local-variable 'query-replace-skip-read-only) t)
  (lsp-bridge-ref-update-header-line)
  ;; Turn off readonly mode.
  (read-only-mode -1)
  ;; Load edit keymap.
  (use-local-map lsp-bridge-ref-mode-edit-map)
  ;; Set edit area.
  (let (start end)
    ;; Make all buffer with readonly text property.
    (let ((inhibit-read-only t))
      (save-excursion
        (put-text-property 1 2 'front-sticky '(read-only))
        (put-text-property (point-min) (point-max) 'read-only t)
        ))
    ;; Make all code with edit property.
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (setq start (search-forward-regexp lsp-bridge-ref-regexp-position nil t))
          (setq start (point))
          (end-of-line)
          (setq end (point))
          (put-text-property (1- start) end 'read-only nil)))
      ))
  ;; Add change monitor.
  (add-hook 'after-change-functions 'lsp-bridge-ref-after-change-function nil t)
  ;; Message to user.
  (message "[LSP-Bridge] Switch to edit mode: press C-c C-c to apply change, press C-c C-q cancel edit"))

(defun lsp-bridge-ref-quit ()
  (interactive)
  ;; Kill temp buffer open by lsp-bridge-ref.
  (when lsp-bridge-ref-kill-temp-buffer-p
    (dolist (temp-buffer lsp-bridge-ref-temp-visit-buffers)
      (kill-buffer temp-buffer)))
  (setq lsp-bridge-ref-temp-visit-buffers nil)
  ;; Kill search buffer.
  (kill-buffer lsp-bridge-ref-buffer)
  ;; Restore window configuration before search.
  (when lsp-bridge-ref-window-configuration-before-search
    (set-window-configuration lsp-bridge-ref-window-configuration-before-search)
    (goto-char lsp-bridge-ref-buffer-point-before-search)
    (setq lsp-bridge-ref-window-configuration-before-search nil)
    (setq lsp-bridge-ref-buffer-point-before-search nil)))

(defun lsp-bridge-ref-beginning-of-line ()
  (interactive)
  (let* ((row-column-position (lsp-bridge-ref-get-row-column-position)))
    (if row-column-position
        (goto-char row-column-position)
      (move-beginning-of-line 1))))

(defun lsp-bridge-ref-delete-line ()
  (interactive)
  (let* ((row-column-position (lsp-bridge-ref-get-row-column-position)))
    (when row-column-position
      (setq start row-column-position)
      (end-of-line)
      (setq end (point))
      (kill-region start end)
      )))

(defun lsp-bridge-ref-delete-all-lines ()
  (interactive)
  (save-excursion
    (with-current-buffer lsp-bridge-ref-buffer
      (goto-char (point-min))
      (while (search-forward-regexp lsp-bridge-ref-regexp-position nil t)
        (lsp-bridge-ref-delete-line)))))

(defun lsp-bridge-ref-recover-line ()
  (interactive)
  (lsp-bridge-ref-delete-line)
  (insert (lsp-bridge-ref-get-line-content lsp-bridge-ref-temp-buffer (line-number-at-pos))))

(defun lsp-bridge-ref-recover-buffer ()
  (interactive)
  (save-excursion
    (with-current-buffer lsp-bridge-ref-buffer
      (let ((inhibit-read-only t))
        ;; Recover buffer content from temp buffer.
        (lsp-bridge-ref-mode) ; switch to `lsp-bridge-ref-mode' first, otherwise `erase-buffer' will cause "save-excursion: end of buffer" error.
        (read-only-mode -1)
        (erase-buffer)
        (insert (with-current-buffer lsp-bridge-ref-temp-buffer
                  (buffer-substring (point-min) (point-max))))
        ;; Switch to edit mode.
        (lsp-bridge-ref-switch-to-edit-mode)
        ))))

(defun lsp-bridge-ref-apply-changed ()
  (interactive)
  (if (equal (length lsp-bridge-ref-changed-lines) 0)
      (message "Nothing need change.")
    ;; Save window configuration before do apply.
    (setq lsp-bridge-ref-window-configuration-before-apply (current-window-configuration))
    ;; Apply changed.
    (let ((inhibit-message t)) ; don't flush to echo area when apply changed, optimise for lsp-bridge-ref
      (save-excursion
        (dolist (line lsp-bridge-ref-changed-lines)
          (let (match-file match-line changed-line-content)
            (setq changed-line-content (lsp-bridge-ref-get-line-content lsp-bridge-ref-buffer line))
            (with-current-buffer lsp-bridge-ref-buffer
              ;; Get match file and line.
              (goto-line line)
              (setq match-file (lsp-bridge-ref-get-match-file))
              (setq match-line (lsp-bridge-ref-get-match-line)))
            ;; Open file in other window.
            (find-file match-file)
            ;; Remove from temp list if file's buffer is exist.
            (setq lsp-bridge-ref-temp-visit-buffers (remove (current-buffer) lsp-bridge-ref-temp-visit-buffers))
            ;; Kill target line.
            (goto-line match-line)
            (kill-line)
            ;; Insert change line.
            (if (string-equal changed-line-content "")
                ;; Kill empty line if line mark as deleted.
                (kill-line)
              ;; Otherwise insert new line into file.
              (insert changed-line-content))
            ))))
    ;; Restore window configuration before apply changed.
    (when lsp-bridge-ref-window-configuration-before-apply
      (set-window-configuration lsp-bridge-ref-window-configuration-before-apply)
      (setq lsp-bridge-ref-window-configuration-before-apply nil))
    ;; Message to user.
    (message (format "[LSP-Bridge] Apply %s lines" (length lsp-bridge-ref-changed-lines))))
  (lsp-bridge-ref-switch-to-view-mode))

(provide 'lsp-bridge-ref)

;;; lsp-bridge-ref.el ends here
