;; -*- lexical-binding: t -*-
;;; acm.el --- Asynchronous Completion Menu

;; Filename: acm.el
;; Description: Asynchronous Completion Menu
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-05-31 16:29:33
;; Version: 0.1
;; Last-Updated: 2022-11-09 19:57:55 +0800
;;           By: Gong Qijian
;; URL: https://www.github.org/manateelazycat/acm
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
;; Asynchronous Completion Menu
;;

;;; Installation:
;;
;; Put acm.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'acm)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET acm RET
;;

;;; Change log:
;;
;; 2022/05/31
;;      * First released.
;;

;;; Acknowledgements:
;;
;;      * Daniel Mendler: reference some code from corfu, thanks
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'xml)
(require 'svg)
(require 'color)
(require 'subr-x)
(require 'cl-seq)
(require 'cl-lib)
(require 'cl-extra)
(require 'cl-macs)

(require 'acm-icon)
(require 'acm-frame)
(require 'acm-backend-yas)
(require 'acm-backend-elisp)
(require 'acm-backend-lsp)
(require 'acm-backend-path)
(require 'acm-backend-search-file-words)
(require 'acm-backend-search-sdcv-words)
(require 'acm-backend-tempel)
(require 'acm-backend-telega)
(require 'acm-backend-tabnine)
(require 'acm-backend-citre)
(require 'acm-backend-ctags)
(require 'acm-backend-codeium)
(require 'acm-backend-copilot)
(require 'acm-backend-org-roam)
(require 'acm-backend-jupyter)
(require 'acm-quick-access)

;;; Code:

(defgroup acm nil
  "Asynchronous Completion Menu."
  :prefix "acm-"
  :group 'lsp-bridge)

(defcustom acm-menu-length 10
  "Maximal number of candidates to show."
  :type 'integer
  :group 'acm)

(defcustom acm-continue-commands
  ;; nil is undefined command
  '(nil ignore universal-argument universal-argument-more digit-argument
        self-insert-command org-self-insert-command
        ;; Avoid flashing completion menu when backward delete char
        grammatical-edit-backward-delete backward-delete-char-untabify
        python-indent-dedent-line-backspace delete-backward-char hungry-delete-backward
        "\\`acm-" "\\`scroll-other-window" "\\`special-lispy-" "\\`special-" "\\`lispy-")
  "Continue ACM completion after executing these commands."
  :type '(repeat (choice regexp symbol))
  :group 'acm)

(defcustom acm-enable-doc t
  "Popup documentation automatically when this option is turn on."
  :type 'boolean
  :group 'acm)

(defcustom acm-enable-doc-markdown-render 'async
  "Popup documentation automatically when this option is turn on."
  :type '(choice (const :tag "Asynchronous" async)
                 (const :tag "Enabled" t)
                 (const :tag "Disabled" nil))
  :group 'acm)

(defcustom acm-enable-icon t
  "Show icon in completion menu."
  :type 'boolean
  :group 'acm)

(defcustom acm-enable-quick-access nil
  "Show quick-access in completion menu."
  :type 'boolean
  :group 'acm)

(defcustom acm-candidate-match-function 'regexp-quote
  "acm candidate match function."
  :type '(choice
          (const nil)
          (const regexp-quote)
          (const orderless-flex)
          (const orderless-literal)
          (const orderless-prefixes)
          (const orderless-regexp)
          (const orderless-initialism))
  :group 'acm)

(defcustom acm-doc-frame-max-lines 20
  "Max line lines of doc frame."
  :type 'integer
  :group 'acm)

(defcustom acm-frame-background-dark-color "#191a1b"
  "The frame background color for dark theme."
  :type 'string
  :group 'acm)

(defcustom acm-frame-background-light-color "#f0f0f0"
  "The frame background color for dark theme."
  :type 'string
  :group 'acm)

(defcustom acm-completion-backend-merge-order '("mode-first-part-candidates"
                                                "template-first-part-candidates"
                                                "tabnine-candidates"
                                                "copilot-candidates"
                                                "codeium-candidates"
                                                "template-second-part-candidates"
                                                "mode-second-part-candidates")
  "The merge order for completion backend."
  :type 'list
  :group 'acm)

(defcustom acm-completion-mode-candidates-merge-order '("elisp-candidates"
                                                        "lsp-candidates"
                                                        "jupyter-candidates"
                                                        "ctags-candidates"
                                                        "citre-candidates"
                                                        "org-roam-candidates"
                                                        "file-words-candidates"
                                                        "telega-candidates")
  "The merge order for mode candidates."
  :type 'list
  :group 'acm)

(defcustom acm-enable-preview nil
  "Enable tab-and-go preview."
  :type 'boolean
  :group 'acm)

(cl-defmacro acm-run-idle-func (timer idle func)
  `(unless ,timer
     (setq ,timer
           (run-with-idle-timer ,idle t #'(lambda () (funcall ,func))))))

(cl-defmacro acm-with-cache-candidates (cache-candidates &rest body)
  `(if (and (boundp ',cache-candidates)
            ,cache-candidates)
       ,cache-candidates
     (setq-local ,cache-candidates ,@body)
     ,cache-candidates))

(defvar acm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap next-line] #'acm-select-next)
    (define-key map [remap previous-line] #'acm-select-prev)
    (define-key map [down] #'acm-select-next)
    (define-key map [up] #'acm-select-prev)
    (define-key map "\M-n" #'acm-select-next)
    (define-key map "\M-p" #'acm-select-prev)
    (define-key map "\M-," #'acm-select-last)
    (define-key map "\M-." #'acm-select-first)
    (define-key map "\C-v" #'acm-select-next-page)
    (define-key map "\M-v" #'acm-select-prev-page)
    (define-key map [tab]  #'acm-complete)
    (define-key map "\C-m" #'acm-complete)
    (define-key map "\t" #'acm-complete)
    (define-key map "\n" #'acm-complete)
    (define-key map "\M-h" #'acm-complete)
    (define-key map "\M-H" #'acm-insert-common)
    (define-key map "\M-u" #'acm-filter)
    (define-key map "\M-d" #'acm-doc-toggle)
    (define-key map "\M-j" #'acm-doc-scroll-up)
    (define-key map "\M-k" #'acm-doc-scroll-down)
    (define-key map "\M-l" #'acm-hide)
    (define-key map "\C-g" #'acm-hide)
    (define-key map "1" #'acm-insert-number-or-complete-candiate)
    (define-key map "2" #'acm-insert-number-or-complete-candiate)
    (define-key map "3" #'acm-insert-number-or-complete-candiate)
    (define-key map "4" #'acm-insert-number-or-complete-candiate)
    (define-key map "5" #'acm-insert-number-or-complete-candiate)
    (define-key map "6" #'acm-insert-number-or-complete-candiate)
    (define-key map "7" #'acm-insert-number-or-complete-candiate)
    (define-key map "8" #'acm-insert-number-or-complete-candiate)
    (define-key map "9" #'acm-insert-number-or-complete-candiate)
    (define-key map "0" #'acm-insert-number-or-complete-candiate)
    map)
  "Keymap used when popup is shown.")

(defvar acm-buffer " *acm-buffer*")
(defvar acm-menu-frame nil)
(defvar acm-menu-frame-popup-point nil)
(defvar acm-menu-frame-popup-position nil)

(defvar acm-menu-number-cache 0)
(defvar acm-menu-max-length-cache 0)

(defvar-local acm-candidates nil)
(defvar-local acm-menu-candidates nil)
(defvar-local acm-menu-index -1)
(defvar-local acm-menu-offset 0)

(defvar-local acm-input-bound-style "ascii")

(defvar-local acm-filter-overlay nil)
(defvar-local acm-filter-string "")

(defvar acm-doc-frame nil)
(defvar acm-doc-frame-hide-p nil)
(defvar acm-doc-buffer " *acm-doc-buffer*")

(defvar acm-preview-overlay nil)

(defface acm-deprecated-face
  '((t :inherit shadow :strike-through t))
  "Face used for deprecated candidates.")

(defface acm-filter-face
  '()
  "Filter face.")

(defsubst acm-indent-pixel (xpos)
  "Return a display property that aligns to XPOS."
  `(space :align-to (,xpos)))

(define-minor-mode acm-mode
  "LSP Bridge mode."
  :keymap acm-mode-map
  :init-value nil
  ;; Set override map, avoid some language mode map conflict with acm-mode map.
  (acm-overriding-key-setup))

(defun acm-hide ()
  (interactive)
  (let* ((completion-menu-is-show-p (acm-frame-visible-p acm-menu-frame))
         (candidate-info (acm-menu-current-candidate))
         (backend (plist-get candidate-info :backend)))
    ;; Turn off `acm-mode'.
    (acm-mode -1)

    ;; Hide menu frame.
    (acm-frame-hide-frame acm-menu-frame)

    ;; Hide doc frame.
    (acm-doc-hide)

    ;; Turn off acm filter.
    (acm-filter-off t)

    ;; Clean `acm-menu-max-length-cache'.
    (setq acm-menu-max-length-cache 0)

    (when acm-preview-overlay
      (if (not (eq this-command 'acm-hide))
          ;; if `acm-hide' is called as command, not insert
          (acm-complete t)
        (delete-overlay acm-preview-overlay)
        (setq acm-preview-overlay nil)))

    ;; Remove hook of `acm--pre-command'.
    (remove-hook 'pre-command-hook #'acm--pre-command 'local)

    ;; If completion menu is show, clean backend cache.
    ;; Don't clean backend cache if menu is hide, to make sure completion menu have candidate when call command `lsp-bridge-popup-complete-menu'.
    (when completion-menu-is-show-p
      (when-let* ((backend-clean (intern-soft (format "acm-backend-%s-clean" backend)))
                  (fp (fboundp backend-clean)))
        (funcall backend-clean)))

    ;; Remove override map when hide acm menu.
    (acm-overriding-key-remove)))

(defun acm-overriding-key-setup ()
  "Some key define in language mode map will conflict with acm-mode map.

So we use `minor-mode-overriding-map-alist' to override key, make sure all keys in acm-mode can response."
  (when (bound-and-true-p haskell-indentation-mode)
    (let ((override-map (make-sparse-keymap)))
      (define-key override-map [?\C-m] 'acm-complete)
      (set (make-local-variable 'minor-mode-overriding-map-alist)
           `((haskell-indentation-mode . ,override-map))))))

(defun acm-overriding-key-remove ()
  "We need remove override acm-mode keys, to make sure language mode key can work after acm menu hide."
  (when (bound-and-true-p haskell-indentation-mode)
    (set (make-local-variable 'minor-mode-overriding-map-alist)
         (assq-delete-all 'haskell-indentation-mode minor-mode-overriding-map-alist))))

(defun acm-match-symbol-p (pattern sym)
  "Return non-nil if SYM is matching an element of the PATTERN list."
  (and (symbolp sym)
       (cl-loop for x in pattern
                thereis (if (symbolp x)
                            (eq sym x)
                          (string-match-p x (symbol-name sym))))))

(defun acm-in-roam-bracket-p ()
  "If in roam bracket. This is a WORKAROUND to change `acm-input-bound-style' locally for completing and expanding nonascii links."
  (and acm-enable-org-roam
       (eq major-mode 'org-mode)
       (featurep 'org-roam)
       (not (org-in-src-block-p))
       (org-in-regexp org-roam-bracket-completion-re 1)
       ;; Prevent completing before roam bracket, e.g. *[[]]
       (<= (match-beginning 2) (point))))

(defun acm-get-input-prefix-bound ()
  (pcase acm-input-bound-style
    ("symbol"
     (bounds-of-thing-at-point 'symbol))
    ("org-roam"
     (when (org-in-regexp org-roam-bracket-completion-re 1)
       (cons (match-beginning 2)
	         (match-end 2))))
    ("string"
     (cons (point)
           (save-excursion
             (if (search-backward-regexp "\\s-" (point-at-bol) t)
                 (progn
                   (forward-char)
                   (point))
               (point-at-bol)))))
    ("ascii"
     (when-let ((bound (bounds-of-thing-at-point 'symbol)))
       (let* ((keyword (buffer-substring-no-properties (car bound) (cdr bound)))
              (offset (or (string-match "[[:nonascii:]]+" (reverse keyword))
                          (length keyword))))
         (cons (- (cdr bound) offset) (cdr bound)))))))

(defun acm-get-input-prefix ()
  "Get user input prefix."
  (let* ((acm-input-bound-style (if (acm-in-roam-bracket-p)
				                    "org-roam"
				                  "ascii"))
	     (bound (acm-get-input-prefix-bound)))
    (if bound
        (buffer-substring-no-properties (car bound) (cdr bound))
      "")))

(defun acm-candidate-fuzzy-search (keyword candidate)
  "Fuzzy search candidate."
  (if acm-candidate-match-function
      (let ((result (funcall acm-candidate-match-function (downcase keyword))))
        (string-match-p (cond ((stringp result) result)
                              (t (rx-to-string result))) ;; If `rx'.
                        (downcase candidate)))
    t))

(defun acm-candidate-sort-by-prefix (keyword candidates)
  "Priority display of the candidates of the prefix matching."
  (when (and candidates
             (consp candidates))
    (if keyword
        (cl-sort candidates (lambda (a b)
                              (let ((a-include-prefix (string-prefix-p keyword (plist-get a :label)))
                                    (b-include-prefix (string-prefix-p keyword (plist-get b :label))))
                                (cond
                                 ;; Sort by prefix first.
                                 ((and b-include-prefix (not a-include-prefix))
                                  nil)
                                 ((and a-include-prefix (not b-include-prefix))
                                  t)
                                 ;; Then sort by candidate length.
                                 (t
                                  (< (length a) (length b)))))))
      ;; Don't sort candidates is keyword is empty string.
      candidates)))

(defvar-local acm-template-candidate-show-p nil)
(defvar-local acm-template-candidate-ticker 0)
(defvar-local acm-template-candidate-show-ticker 0)
(defvar acm-template-candidate-timer nil)

(defun acm-template-candidate-init ()
  "Call this function in `lsp-bridge-try-completion', init template candidate variable.

Only calculate template candidate when type last character."
  (setq-local acm-template-candidate-show-p nil)
  (setq-local acm-template-candidate-ticker (1+ acm-template-candidate-ticker)))

(defun acm-template-candidate-update ()
  "Set `acm-template-candidate-show-p' to t to calculate template candidates."
  (setq-local acm-template-candidate-show-p t)
  (acm-update))

(cl-defmacro acm-cancel-timer (timer)
  `(when ,timer
     (cancel-timer ,timer)
     (setq ,timer nil)))

(defun acm-update-candidates ()
  (let* ((keyword (acm-get-input-prefix))
         (char-before-keyword (save-excursion
                                (backward-char (length keyword))
                                (acm-char-before)))
         (candidates (list))
         (mode-candidates-min-index 2)
         (template-candidates-min-index 2)
         lsp-candidates
         path-candidates
         yas-candidates
         tabnine-candidates
         codeium-candidates
         copilot-candidates
         jupyter-candidates
         tempel-candidates
         mode-candidates
         mode-first-part-candidates
         mode-second-part-candidates
         mode-candidates-split-index
         template-candidates
         template-first-part-candidates
         template-second-part-candidates
         ctags-candidates
         citre-candidates
         org-roam-candidates)
    (when (acm-in-roam-bracket-p)
      (setq org-roam-candidates (acm-backend-org-roam-candidates keyword)))

    (when acm-enable-tabnine
      (setq tabnine-candidates (acm-backend-tabnine-candidates keyword)))

    (when acm-enable-codeium
      (setq codeium-candidates (acm-backend-codeium-candidates keyword)))

    (when acm-enable-copilot
      (setq copilot-candidates (acm-backend-copilot-candidates keyword)))

    (when acm-enable-jupyter
      (setq jupyter-candidates (acm-backend-jupyter-candidates keyword)))

    (if acm-enable-search-sdcv-words
        ;; Completion SDCV if option `acm-enable-search-sdcv-words' is enable.
        (setq candidates (acm-backend-search-sdcv-words-candidates keyword))

      (setq path-candidates (acm-backend-path-candidates keyword))
      (if (> (length path-candidates) 0)
          ;; Only show path candidates if prefix is valid path.
          (setq candidates path-candidates)

        (when acm-enable-citre
          (setq citre-candidates (unless (acm-in-comment-p) (acm-backend-citre-candidates keyword))))
        (when acm-enable-ctags
          (setq ctags-candidates (unless (acm-in-comment-p) (acm-backend-ctags-candidates keyword))))
        ;; Fetch syntax completion candidates.
        (setq lsp-candidates (unless (acm-in-comment-p) (acm-backend-lsp-candidates keyword)))
        (setq mode-candidates
              (apply #'append (mapcar (lambda (mode-candidate-name)
                                        (pcase mode-candidate-name
                                          ("elisp-candidates" (unless (acm-in-comment-p) (acm-backend-elisp-candidates keyword)))
                                          ("lsp-candidates" lsp-candidates)
                                          ("jupyter-candidates" jupyter-candidates)
                                          ("ctags-candidates" ctags-candidates)
                                          ("citre-candidates" citre-candidates)
			                              ("org-roam-candidates" org-roam-candidates)
                                          ("file-words-candidates" (acm-backend-search-file-words-candidates keyword))
                                          ("telega-candidates" (acm-backend-telega-candidates keyword))
                                          ))
                                      acm-completion-mode-candidates-merge-order)))

        (when (and (or
                    ;; Show snippet candidates if lsp-candidates length is zero.
                    (zerop (length lsp-candidates))
                    ;; Don't search snippet if char before keyword is not in `acm-backend-lsp-completion-trigger-characters'.
                    (and (boundp 'acm-backend-lsp-completion-trigger-characters)
                         (not (member char-before-keyword acm-backend-lsp-completion-trigger-characters))))
                   (not (acm-in-comment-p)))

          ;; Only calculate template candidate when type last character.
          (cond
           ;; Show template candidates when flag `acm-template-candidate-show-p' is t.
           (acm-template-candidate-show-p
            (setq yas-candidates (acm-backend-yas-candidates keyword))
            (setq tempel-candidates (acm-backend-tempel-candidates keyword))
            (setq-local acm-template-candidate-show-p nil)
            (setq-local acm-template-candidate-show-ticker acm-template-candidate-ticker))
           ;; Don't hide template candidates if just shown in last time, avoid menu flick by template candiates.
           ((= (- acm-template-candidate-ticker acm-template-candidate-show-ticker) 1)
            (setq yas-candidates (acm-backend-yas-candidates keyword))
            (setq tempel-candidates (acm-backend-tempel-candidates keyword)))
           ;; Try show template candidates after 200ms later.
           ;; Cancel timer if last timer haven't executed when user type new character.
           (t
            (acm-cancel-timer acm-template-candidate-timer)
            (setq acm-template-candidate-timer (run-with-timer 0.2 nil #'acm-template-candidate-update)))))

        ;; Build template candidates.
        ;; And make sure show part of template candidates in first completion menu.
        (setq template-candidates (append yas-candidates tempel-candidates))
        (if (> (length template-candidates) template-candidates-min-index)
            (progn
              (setq template-first-part-candidates (cl-subseq template-candidates 0 template-candidates-min-index))
              (setq template-second-part-candidates (cl-subseq template-candidates template-candidates-min-index)))
          (setq template-first-part-candidates template-candidates)
          (setq template-second-part-candidates nil))

        ;; Make sure show part of TabNine candidates in first completion menu.
        (setq mode-candidates-split-index
              (max (- acm-menu-length (+ (length template-first-part-candidates) (length tabnine-candidates)))
                   mode-candidates-min-index))

        ;; Build mode candidates.
        (if (> (length mode-candidates) mode-candidates-split-index)
            (progn
              (setq mode-first-part-candidates (cl-subseq mode-candidates 0 mode-candidates-split-index))
              (setq mode-second-part-candidates (cl-subseq mode-candidates mode-candidates-split-index)))
          (setq mode-first-part-candidates mode-candidates)
          (setq mode-second-part-candidates nil))

        ;; Build all backend candidates.
        (setq candidates (apply #'append (mapcar (lambda (backend-name)
                                                   (pcase backend-name
                                                     ("mode-first-part-candidates" mode-first-part-candidates)
                                                     ("template-first-part-candidates" template-first-part-candidates)
                                                     ("tabnine-candidates" tabnine-candidates)
                                                     ("codeium-candidates" codeium-candidates)
                                                     ("copilot-candidates" copilot-candidates)
                                                     ("template-second-part-candidates" template-second-part-candidates)
                                                     ("mode-second-part-candidates" mode-second-part-candidates)
                                                     ))
                                                 acm-completion-backend-merge-order))
              )))

    ;; Return candidates.
    (if acm-filter-overlay
        ;; When acm-filter turn on, use `acm-filter-string' filter candidates.
        (cl-remove-if-not (lambda (candidate)
                            (acm-candidate-fuzzy-search acm-filter-string (plist-get candidate :label)))
                          candidates)
      ;; Otherwise return origin candidates.
      candidates)))

(defun acm-menu-index-info (candidate)
  "Pick label and backend information to record and restore menu select index.
The key of candidate will change between two LSP results."
  (format "%s###%s" (plist-get candidate :label) (plist-get candidate :backend)))

(defun acm-update (&optional candidate)
  ;; Init quick mode map.
  (acm-quick-access-init)

  ;; Adjust `gc-cons-threshold' to maximize temporary,
  ;; make sure Emacs not do GC when filter/sort candidates.
  (let* ((gc-cons-threshold most-positive-fixnum)
         (keyword (acm-get-input-prefix))
         (previous-select-candidate-index (+ acm-menu-offset acm-menu-index))
         (previous-select-candidate (acm-menu-index-info (acm-menu-current-candidate)))
         (candidates (or candidate (acm-update-candidates)))
         (menu-candidates (cl-subseq candidates 0 (min (length candidates) acm-menu-length)))
         (current-select-candidate-index (cl-position previous-select-candidate (mapcar 'acm-menu-index-info menu-candidates) :test 'equal))
	     (acm-input-bound-style (if (acm-in-roam-bracket-p)
				                    "org-roam"
				                  "ascii"))
	     (bounds (acm-get-input-prefix-bound)))
    (cond
     ;; Hide completion menu if user type first candidate completely, except when candidate annotation is `emmet' or `snippet'.
     ((and (equal (length candidates) 1)
           (string-equal keyword (plist-get (nth 0 candidates) :label))
           (not (member (plist-get (nth 0 candidates) :annotation) '("Emmet Abbreviation" "Snippet" "Yas-Snippet" "Tempel"))))
      (acm-hide))
     ((> (length candidates) 0)
      (let* ((menu-old-cache (cons acm-menu-max-length-cache acm-menu-number-cache)))
        ;; Enable acm-mode to inject mode keys.
        (acm-mode 1)

        ;; Use `pre-command-hook' to hide completion menu when command match `acm-continue-commands'.
        (add-hook 'pre-command-hook #'acm--pre-command nil 'local)

        ;; Adjust candidates.
        (setq-local acm-menu-offset 0)  ;init offset to 0
        (if (zerop (length acm-menu-candidates))
            ;; Adjust `acm-menu-index' to -1 if no candidates found.
            (setq-local acm-menu-index -1)
          ;; First init `acm-menu-index' to 0.
          (setq-local acm-menu-index 0)

          ;; The following code is specifically to adjust the selection position of candidate when typing fast.
          (when (and current-select-candidate-index
                     (> (length candidates) 1))
            (cond
             ;; Swap the position of the first two candidates
             ;; if previous candidate's position change from 1st to 2nd.
             ((and (= previous-select-candidate-index 0) (= current-select-candidate-index 1))
              (cl-rotatef (nth 0 candidates) (nth 1 candidates))
              (cl-rotatef (nth 0 menu-candidates) (nth 1 menu-candidates)))
             ;; Swap the position of the first two candidates and select 2nd postion
             ;; if previous candidate's position change from 2nd to 1st.
             ((and (= previous-select-candidate-index 1) (= current-select-candidate-index 0))
              (cl-rotatef (nth 0 candidates) (nth 1 candidates))
              (cl-rotatef (nth 0 menu-candidates) (nth 1 menu-candidates))
              (setq-local acm-menu-index 1))
             ;; Select 2nd position if previous candidate's position still is 2nd.
             ((and (= previous-select-candidate-index 1) (= current-select-candidate-index 1))
              (setq-local acm-menu-index 1)))))

        ;; Set candidates and menu candidates.
        (setq-local acm-candidates candidates)
        (setq-local acm-menu-candidates menu-candidates)

        ;; Init colors.
        (acm-frame-init-colors)

        ;; Record menu popup position and buffer.
        (setq acm-menu-frame-popup-point (or (car bounds) (point)))

        ;; `posn-at-point' will failed in CI, add checker make sure CI can pass.
        ;; CI don't need popup completion menu.
        (when (posn-at-point acm-menu-frame-popup-point)
          (setq acm-menu-frame-popup-position
                (acm-frame-get-popup-position acm-menu-frame-popup-point))

          ;; We need delete frame first when user switch to different frame.
          (when (and (frame-live-p acm-menu-frame)
                     (not (eq (frame-parent acm-menu-frame) (selected-frame))))
            (acm-frame-delete-frame acm-menu-frame)
            (acm-frame-delete-frame acm-doc-frame))

          ;; Create menu frame if it not exists.
          (acm-frame-create-frame-if-not-exist acm-menu-frame acm-buffer "acm frame" 0 t)

          ;; Render menu.
          (acm-menu-render menu-old-cache))
        ))
     (t
      (acm-hide)))))

(defun acm-reset-colors (&rest args)
  ;; Reset colors.
  (acm-frame-init-colors t)

  ;; Reset frame colors.
  (when (frame-live-p acm-menu-frame)
    (acm-frame-set-frame-colors acm-menu-frame)
    (when (frame-visible-p acm-menu-frame)
      (acm-menu-render
       (cons acm-menu-max-length-cache acm-menu-number-cache))))
  (when (frame-live-p acm-doc-frame)
    (acm-frame-set-frame-colors acm-doc-frame)))

(if (daemonp)
    ;; The :background of 'default is unavailable until frame is created in
    ;; daemon mode.
    (add-hook 'server-after-make-frame-hook
              (lambda ()
                (advice-add #'load-theme :after #'acm-reset-colors)
                ;; Compensation for missing the first `load-thme' in
                ;; `after-init-hook'.
                (acm-reset-colors)))
  (advice-add #'load-theme :after #'acm-reset-colors))

(defun acm-running-in-wayland-native ()
  (and (eq window-system 'pgtk)
       (fboundp 'pgtk-backend-display-class)
       (string-equal (pgtk-backend-display-class) "GdkWaylandDisplay")))

;; NOTE:
;; Emacs pgtk branch has bug https://debbugs.gnu.org/cgi/bugreport.cgi?bug=58556
;; we need set `pgtk-wait-for-event-timeout' to 0 to fix frame slow issue.
(when (acm-running-in-wayland-native)
  (setq pgtk-wait-for-event-timeout 0))

(defun acm-doc-hide ()
  (acm-doc--hide))

(defun acm-doc--hide()
  (acm-frame-hide-frame acm-doc-frame)

  (acm-cancel-timer acm-markdown-render-timer)

  (setq acm-markdown-render-doc nil))

(defun acm--pre-command ()
  ;; Use `pre-command-hook' to hide completion menu when command match `acm-continue-commands'.
  (unless (acm-match-symbol-p acm-continue-commands this-command)
    (acm-hide)))

(defun acm-complete (&optional not-hide)
  (interactive)
  (let* ((candidate-info (acm-menu-current-candidate))
         (bound-start acm-menu-frame-popup-point)
         (backend (plist-get candidate-info :backend))
         (candidate-expand (intern-soft (format "acm-backend-%s-candidate-expand" backend))))

    (if (fboundp candidate-expand)
        (funcall candidate-expand candidate-info bound-start)
      (delete-region bound-start (point))
      (insert (plist-get candidate-info :label))))

  (when (overlayp acm-preview-overlay)
    (delete-overlay acm-preview-overlay))
  (setq acm-preview-overlay nil)

  ;; Hide menu and doc frame after complete candidate.
  (unless not-hide
    (acm-hide)))

(defun acm-preview-create-overlay (beg end display)
  (let ((ov (make-overlay beg end nil)))
    (overlay-put ov 'priority 1000)
    (overlay-put ov 'window (selected-window))
    (when (stringp display)
      (overlay-put ov 'display display))
    ov))

(defun acm-preview-current ()
  "Show current candidate as overlay given BEG and END."
  (let* ((candidate-info (acm-menu-current-candidate))
         (beg acm-menu-frame-popup-point)
         (cand (plist-get candidate-info :label))
         (end (+ beg (length cand)))
         (backend (plist-get candidate-info :backend))
         (candidate-expand (intern-soft (format "acm-backend-%s-candidate-expand" backend))))
    (when acm-preview-overlay (delete-overlay acm-preview-overlay))
    (if (and (fboundp candidate-expand)
             ;; check if candidate-expand support preview.
             (let ((doc (documentation candidate-expand t)))
               (if doc
                   (string-match " PREVIEW" doc)
                 (member 'preview (help-function-arglist candidate-expand)))))
        (save-excursion
          (setq acm-preview-overlay (funcall candidate-expand candidate-info beg t)))
      (setq acm-preview-overlay (acm-preview-create-overlay beg (point) cand)))
    ;; adjust pos of menu frame.
    (when-let ((popup-pos (acm-frame-get-popup-position
                           acm-menu-frame-popup-point
                           (1- (length (split-string (overlay-get acm-preview-overlay 'display) "\n")))))
               ((not (eq (cdr popup-pos) (cdr acm-menu-frame-popup-position)))))
      (setcdr acm-menu-frame-popup-position (cdr popup-pos))
      (acm-menu-adjust-pos))))

(defun acm-complete-or-expand-yas-snippet ()
  "Do complete or expand yasnippet, you need binding this funtion to `<tab>' in `yas-keymap'."
  (interactive)
  (if (acm-frame-visible-p acm-menu-frame)
      (acm-complete)
    (yas-next-field-or-maybe-expand)))

(defun acm-insert-common ()
  "Insert common prefix of menu."
  (interactive)
  (when (acm-frame-visible-p acm-menu-frame)
    (let* ((common-string "")
           (items (mapcar (lambda (v) (plist-get v :label)) acm-menu-candidates))
           (item-min-length (cl-reduce #'min (mapcar #'string-width items)))
           (input-prefix (acm-get-input-prefix)))
      (dolist (index (number-sequence 0 (1- item-min-length)))
        (let* ((char-list (mapcar (lambda (i) (substring i index (1+ index))) items))
               (first-char (cl-first char-list)))
          (when (cl-every (lambda (x) (string-equal x first-char)) char-list)
            (setq common-string (concat common-string first-char)))))

      (if (and (> (length common-string) 0)
               (> (length common-string) (length input-prefix)))
          (insert (substring common-string (length (acm-get-input-prefix))))
        (message "No common string found")))))

(defvar acm-string-width-function (if (fboundp 'string-pixel-width)
                                      'string-pixel-width
                                    'string-width))

(defun acm-menu-max-length ()
  "Get max length of menu candidates, use for adjust menu size dynamically."
  (cl-reduce #'max
             (mapcar (lambda (v)
                       (funcall acm-string-width-function
                                (format "%s %s" (plist-get v :displayLabel) (plist-get v :annotation))))
                     acm-menu-candidates)))

(defun acm-menu-render-items (items menu-index)
  (let* ((item-index 0)
         (annotation-not-exits (cl-every (lambda (item) (string-empty-p (plist-get item :annotation))) items)))
    (dolist (v items)
      (let* ((icon (cdr (assoc (downcase (plist-get v :icon)) acm-icon-alist)))
             (icon-default (cdr (assoc t acm-icon-alist)))
             (display-icon (or icon icon-default))
             (candidate (plist-get v :displayLabel))
             (annotation (plist-get v :annotation))
             (annotation-text (format "%s " (capitalize (if annotation annotation ""))))
             (annotation-face (if (equal item-index menu-index) 'acm-frame-select-face 'font-lock-doc-face))
             (item-length (funcall acm-string-width-function annotation-text))
             (icon-text (acm-icon-build (nth 0 display-icon) (nth 1 display-icon) (nth 2 display-icon)))
             (quick-access-key (nth item-index acm-quick-access-keys))
             candidate-line)

        ;; Render deprecated candidate.
        (when (plist-get v :deprecated)
          (add-face-text-property 0 (length candidate) 'acm-deprecated-face 'append candidate))

        ;; Build candidate line.
        (setq candidate-line
              (concat
               ;; Icon.
               (if acm-enable-icon
                   icon-text   ; render icon if `acm-enable-icon' is t
                 " ")   ; add left padding if `acm-enable-icon' is nil
               ;; Index.
               (when acm-enable-quick-access
                 (if quick-access-key (concat quick-access-key ". ") "   "))
               ;; Candidate.
               candidate
               ;; Fill in the blank according to the maximum width, make sure marks align right of menu.
               (unless annotation-not-exits
                 (propertize " "
                             'display
                             (acm-indent-pixel
                              (if (equal acm-string-width-function 'string-pixel-width)
                                  (- (+ acm-menu-max-length-cache (* 20 (string-pixel-width " "))) item-length)
                                (ceiling (* (frame-char-width) (- (+ acm-menu-max-length-cache 20) item-length)))))))
               ;; Render annotation color.
               (propertize annotation-text 'face annotation-face)
               ;; Return char.
               "\n"))

        ;; Render current candidate.
        (when (equal item-index menu-index)
          (add-face-text-property 0 (length candidate-line) 'acm-frame-select-face 'append candidate-line)

          ;; Hide doc frame if some backend not support fetch candidate documentation.
          (when (and
                 (not (fboundp (intern-soft (format "acm-backend-%s-candidate-doc" (plist-get v :backend)))))
                 (acm-frame-visible-p acm-doc-frame))
            (acm-doc-hide)))

        ;; Insert candidate line.
        (insert candidate-line)

        ;; Delete the last extra return line.
        (when (equal item-index (1- (length items)))
          (delete-backward-char 1))

        ;; Update item index.
        (setq item-index (1+ item-index))))))

(defun acm-menu-adjust-pos ()
  "Adjust menu frame position."
  (let* ((emacs-width (frame-pixel-width))
         (emacs-height (frame-pixel-height))
         (acm-frame-width (frame-pixel-width acm-menu-frame))
         (acm-frame-height (frame-pixel-height acm-menu-frame))
         (cursor-x (car acm-menu-frame-popup-position))
         (cursor-y (cdr acm-menu-frame-popup-position))
         (offset-x (* (frame-char-width) acm-icon-width))
         (offset-y (line-pixel-height))
         (acm-frame-x (if (> (+ cursor-x acm-frame-width) emacs-width)
                          (max (- cursor-x acm-frame-width) offset-x)
                        (max (- cursor-x offset-x) 0)))
         (acm-frame-y (cond
                       ;; Always popup up when cursor in minibuffer.
                       ((minibufferp)
                        (- (nth 1 (window-pixel-edges (minibuffer-window))) acm-frame-height))
                       ;; Popup up when acm menu bottom below at Emacs bottom edge.
                       ((> (+ cursor-y acm-frame-height) emacs-height)
                        (- cursor-y acm-frame-height))
                       ;; Otherwise, popup down.
                       (t
                        (+ cursor-y offset-y)))))
    (acm-frame-set-frame-position acm-menu-frame acm-frame-x acm-frame-y)))

(defun acm-doc-try-show (&optional update-completion-item)
  (when acm-enable-doc
    (let* ((candidate (acm-menu-current-candidate))
           (backend (plist-get candidate :backend))
           (candidate-doc-func (intern-soft (format "acm-backend-%s-candidate-doc" backend)))
           (candidate-doc
            (when (fboundp candidate-doc-func)
              (funcall candidate-doc-func candidate))))
      (if (or (consp candidate-doc) ; If the type fo snippet is set to command, then the "doc" will be a list.
              (and (stringp candidate-doc) (not (string-empty-p candidate-doc))))
          (let ((doc (if (stringp candidate-doc)
                         candidate-doc
                       (format "%S" candidate-doc))))
            ;; Create doc frame if it not exist.
            (acm-frame-create-frame-if-not-exist acm-doc-frame acm-doc-buffer "acm doc frame" 1 t)
            (setq acm-doc-frame-hide-p nil)

            ;; Insert documentation and turn on wrap line.
            (with-current-buffer (get-buffer-create acm-doc-buffer)
              (read-only-mode -1)
              (erase-buffer)
              (insert doc)
              (visual-line-mode 1))

            ;; Only render markdown styling when idle 200ms, because markdown render is expensive.
            (when (member backend '("lsp" "codeium" "copilot"))
              (acm-cancel-timer acm-markdown-render-timer)
              (cl-case acm-enable-doc-markdown-render
                (async (setq acm-markdown-render-timer
                             (run-with-idle-timer 0.2 nil
                                                  (lambda ()
                                                    (acm-doc-markdown-render-content doc)))))
                ((t) (acm-doc-markdown-render-content doc))))

            ;; Adjust doc frame position and size.
            (acm-doc-frame-adjust))

        (pcase backend
          ;; If backend is LSP, doc frame hide when `update-completion-item' is t.
          ("lsp" (when update-completion-item
                   (acm-doc-hide)))
          ;; Hide doc frame immediately if backend is not LSP.
          (_ (acm-doc-hide)))))))

(defun acm-doc-frame-adjust ()
  (let* ((emacs-width (frame-pixel-width))
         (emacs-height (frame-pixel-height))
         (acm-frame-width (frame-pixel-width acm-menu-frame))
         (acm-frame-height (frame-pixel-height acm-menu-frame))
         (acm-frame-pos (frame-position acm-menu-frame))
         (acm-frame-x (car acm-frame-pos))
         (acm-frame-y (cdr acm-frame-pos))

         (acm-frame-left-distance acm-frame-x)
         (acm-frame-right-distance (- emacs-width acm-frame-x acm-frame-width))
         (acm-frame-top-distance acm-frame-y)
         (acm-frame-bottom-distance (- emacs-height acm-frame-y acm-frame-height))

         (acm-doc-frame-max-width (max acm-frame-left-distance acm-frame-right-distance))
         (acm-doc-frame-max-height (max acm-frame-top-distance acm-frame-bottom-distance)))

    ;; Make sure doc frame size not out of Emacs area.
    (acm-frame-set-frame-max-size acm-doc-frame
                                  (ceiling (/ acm-doc-frame-max-width (frame-char-width)))
                                  (min (ceiling (/ acm-doc-frame-max-height (window-default-line-height)))
                                       acm-doc-frame-max-lines))

    ;; Adjust doc frame with it's size.
    (let* ((acm-doc-frame-width (frame-pixel-width acm-doc-frame))
           (acm-doc-frame-x (if (> (+ acm-frame-x acm-frame-width acm-doc-frame-max-width) emacs-width)
                                (- acm-frame-x acm-doc-frame-width)
                              (+ acm-frame-x acm-frame-width)))
           (acm-doc-frame-y acm-frame-y))
      (acm-frame-set-frame-position acm-doc-frame acm-doc-frame-x acm-doc-frame-y))))

(defun acm-menu-current-candidate ()
  "Get current candidate with menu index and offset."
  (nth (+ acm-menu-offset acm-menu-index) acm-candidates))

(defun acm-menu-render (menu-old-cache)
  (let* ((items acm-menu-candidates)
         (menu-old-max-length (car menu-old-cache))
         (menu-old-number (cdr menu-old-cache))
         (menu-new-max-length (acm-menu-max-length))
         (menu-new-number (length items))
         (menu-index acm-menu-index))
    ;; Record newest cache.
    (setq acm-menu-max-length-cache menu-new-max-length)
    (setq acm-menu-number-cache menu-new-number)

    ;; Insert menu candidates.
    (with-current-buffer (get-buffer-create acm-buffer)
      (erase-buffer)
      (acm-menu-render-items items menu-index))

    ;; Not adjust menu frame size if not necessary,
    ;; such as select candidate just change index,
    ;; or menu width not change when switch to next page.
    (when (or (not (equal menu-old-max-length menu-new-max-length))
              (not (equal menu-old-number menu-new-number)))
      (acm-frame-set-frame-max-size acm-menu-frame)

      ;; Adjust doc frame with menu frame position.
      (when (acm-frame-visible-p acm-doc-frame)
        (acm-doc-frame-adjust)))

    ;; Adjust menu frame position.
    ;; We need adjust acm menu position after set it size, avoid coordinate shifts due to resizing.
    (acm-menu-adjust-pos)

    ;; Fetch `documentation' and `additionalTextEdits' information.
    (acm-doc-try-show)
    ))

(cl-defmacro acm-silent (&rest body)
  "Silence BODY."
  (declare (indent 0))
  `(cl-letf ((inhibit-message t)
             (message-log-max nil)
             ((symbol-function #'minibuffer-message) #'ignore))
     (ignore-errors ,@body)))

(defun acm-menu-update-candidates ()
  (let ((menu-length (length acm-menu-candidates)))
    ;; Only change menu candidates when filter candidate length bigger than menu length.
    (when (> (length acm-candidates) menu-length)
      (setq-local acm-menu-candidates
                  (cl-subseq acm-candidates acm-menu-offset (+ acm-menu-offset menu-length))))))

(cl-defmacro acm-menu-update (&rest body)
  `(let* ((menu-old-index acm-menu-index)
          (menu-old-offset acm-menu-offset)
          (menu-old-cache (cons acm-menu-max-length-cache acm-menu-number-cache)))
     ,@body

     (cond ((< acm-menu-index 0)
            (setq-local acm-menu-index 0))
           ((>= acm-menu-index (length acm-menu-candidates))
            (setq-local acm-menu-index (1- (length acm-menu-candidates)))))

     (cond ((< acm-menu-offset 0)
            (setq-local acm-menu-offset 0))
           ((>= acm-menu-offset (- (length acm-candidates) (length acm-menu-candidates)))
            (setq-local acm-menu-offset (- (length acm-candidates) (length acm-menu-candidates)))))

     ;; Only update menu candidates when menu index or offset changed.
     (when (or (not (equal menu-old-index acm-menu-index))
               (not (equal menu-old-offset acm-menu-offset)))
       (acm-menu-update-candidates)
       (acm-menu-render menu-old-cache))
     (when acm-enable-preview
       (acm-preview-current))
     ))

(defun acm-char-before ()
  (let ((prev-char (char-before)))
    (if prev-char (char-to-string prev-char) "")))

(defvar-local acm-is-elisp-mode-in-org nil)
(defun acm-is-elisp-mode-p ()
  (or (derived-mode-p 'emacs-lisp-mode)
      (derived-mode-p 'inferior-emacs-lisp-mode)
      (derived-mode-p 'lisp-interaction-mode)
      (and (eq major-mode 'org-mode)
           acm-is-elisp-mode-in-org)
      (and (minibufferp)
           (where-is-internal #'completion-at-point (list (current-local-map))))))

(defun acm-select-first ()
  "Select first candidate."
  (interactive)
  (acm-menu-update
   (setq-local acm-menu-offset 0)
   (setq-local acm-menu-index 0)))

(defun acm-select-last ()
  "Select last candidate."
  (interactive)
  (acm-menu-update
   (let ((menu-length (length acm-menu-candidates)))
     (setq-local acm-menu-offset (- (length acm-candidates) menu-length))
     (setq-local acm-menu-index (- menu-length 1))
     )))

(defun acm-select-next ()
  "Select next candidate."
  (interactive)
  (acm-menu-update
   (cond ((< acm-menu-index (1- (length acm-menu-candidates)))
          (setq-local acm-menu-index (1+ acm-menu-index)))
         ((< (+ acm-menu-offset acm-menu-index) (1- (length acm-candidates)))
          (setq-local acm-menu-offset (1+ acm-menu-offset))))))

(defun acm-select-prev ()
  "Select previous candidate."
  (interactive)
  (acm-menu-update
   (cond ((> acm-menu-index 0)
          (setq-local acm-menu-index (1- acm-menu-index)))
         ((> acm-menu-offset 0)
          (setq-local acm-menu-offset (1- acm-menu-offset))))))

(defun acm-select-next-page ()
  "Select next page candidate."
  (interactive)
  (acm-menu-update
   (cond ((< acm-menu-index (1- (length acm-menu-candidates)))
          (setq-local acm-menu-index (+ acm-menu-index acm-menu-length)))
         ((< (+ acm-menu-offset acm-menu-index) (1- (length acm-candidates)))
          (setq-local acm-menu-offset (+ acm-menu-offset acm-menu-length))))))

(defun acm-select-prev-page ()
  "Select previous page candidate."
  (interactive)
  (acm-menu-update
   (cond ((> acm-menu-index 0)
          (setq-local acm-menu-index (- acm-menu-index acm-menu-length)))
         ((> acm-menu-offset 0)
          (setq-local acm-menu-offset (- acm-menu-offset acm-menu-length))))))

(defun acm-doc-scroll-up ()
  (interactive)
  (with-current-buffer acm-doc-buffer
    (when (framep acm-menu-frame)
      (with-selected-frame acm-doc-frame
        (scroll-up-command)))))

(defun acm-doc-scroll-down ()
  (interactive)
  (with-current-buffer acm-doc-buffer
    (when (framep acm-menu-frame)
      (with-selected-frame acm-doc-frame
        (scroll-down-command)))))

(defun acm-doc-toggle ()
  "Toggle documentation preview for selected candidate."
  (interactive)
  (if (acm-frame-visible-p acm-doc-frame)
      (acm-doc-hide)
    (let ((acm-enable-doc t))
      (acm-doc-try-show))))

(defvar acm-markdown-render-timer nil)
(defvar acm-markdown-render-doc nil)

(defvar acm-markdown-render-prettify-symbols-alist
  (nconc
   (cl-loop for i from 0 to 255
            collect (cons (format "&#x%02X;" i) i))
   '(("\\!" . ?!) ("\\#" . ?#) ("\\*" . ?*) ("\\+" . ?+) ("\\:" . ?:)
     ("\\<" . ?<) ("\\>" . ?>) ("\\[" . ?\[) ("\\]" . ?\]) ("\\^" . ?^)
     ("\\_" . ?_) ("\\`" . ?`) ("\\|" . ?|) ("\\~" . ?~) ("\\\\" . ?\\)
     ("&lt;" . ?<) ("&gt;" . ?>) ("&amp;" . ?&))))

(defun acm-frame-background-color ()
  (pcase (format "%s" (frame-parameter nil 'background-mode))
    ("dark" acm-frame-background-dark-color)
    ("light" acm-frame-background-light-color)))

(defun acm-markdown-render-content (&optional enable-decorations)
  (when (fboundp 'gfm-view-mode)
    (let ((inhibit-message t))
      ;; Enable `gfm-view-mode' first, otherwise `gfm-view-mode' will change attribute of face `markdown-code-face'.
      (gfm-view-mode)

      ;; Then remapping background and height of `markdown-code-face' to same as acm doc buffer.
      (face-remap-add-relative 'markdown-code-face :background (face-attribute 'default :background acm-menu-frame))
      ))

  (read-only-mode 0)

  ;; Enable prettify-symbols.
  (setq prettify-symbols-alist acm-markdown-render-prettify-symbols-alist)
  (setq prettify-symbols-compose-predicate (lambda (_start _end _match) t))
  (prettify-symbols-mode 1)

  ;; Syntax Highlight.
  (font-lock-ensure)

  (unless enable-decorations
    ;; Disable line number.
    (display-line-numbers-mode -1)
    ;; Disable mode line.
    (setq-local mode-line-format nil)))

(defun acm-doc-markdown-render-content (doc)
  (when (and (acm-frame-visible-p acm-doc-frame)
             (not (string-equal doc acm-markdown-render-doc)))
    (with-current-buffer (get-buffer-create acm-doc-buffer)
      (read-only-mode -1)
      (acm-markdown-render-content))

    (setq acm-markdown-render-doc doc)

    ;; We need `acm-doc-frame-adjust' again if `acm-enable-doc-markdown-render' is `async'.
    ;; otherwise, `gfm-view-mode' will remove ``` line.
    (cl-case acm-enable-doc-markdown-render
      (async (acm-doc-frame-adjust))
      )))

(defun acm-in-comment-p (&optional state)
  (if (and (featurep 'treesit) (treesit-available-p) (treesit-parser-list))
      ;; Avoid use `acm-current-parse-state' when treesit is enable.
      ;; `beginning-of-defun' is very expensive function will slow down completion menu.
      ;; We use `treesit-node-type' directly if treesit is enable.
      (or (eq (get-text-property (point) 'face) 'font-lock-comment-face)
          (string-equal (treesit-node-type (treesit-node-at (point))) "comment"))
    (ignore-errors
      (unless (or (bobp) (eobp))
        (save-excursion
          (or
           (nth 4 (or state (acm-current-parse-state)))
           (eq (get-text-property (point) 'face) 'font-lock-comment-face))
          )))))

(defun acm-in-string-p (&optional state)
  (if (and (featurep 'treesit) (treesit-available-p) (treesit-parser-list))
      ;; Avoid use `acm-current-parse-state' when treesit is enable.
      ;; `beginning-of-defun' is very expensive function will slow down completion menu.
      ;; We use `treesit-node-type' directly if treesit is enable.
      (or (eq (get-text-property (point) 'face) 'font-lock-string-face)
          (string-equal (treesit-node-type (treesit-node-at (point))) "string"))
    (ignore-errors
      (unless (or (bobp) (eobp))
        (save-excursion
          (and
           (nth 3 (or state (acm-current-parse-state)))
           (not (equal (point) (line-end-position))))
          )))))

(defun acm-current-parse-state ()
  (let ((point (point)))
    (beginning-of-defun)
    (when (equal point (point))
      (beginning-of-line))
    (parse-partial-sexp (point) point)))

(defun acm-filter ()
  (interactive)
  (if acm-filter-overlay
      (acm-filter-off)
    (acm-filter-on)))

(defun acm-filter-on ()
  (setq-local acm-filter-overlay (make-overlay (point) (point)))
  (message "Turn on acm filter."))

(defun acm-filter-off (&optional quiet)
  (when acm-filter-overlay
    (delete-overlay acm-filter-overlay))
  (setq-local acm-filter-overlay nil)
  (setq-local acm-filter-string "")
  (unless quiet
    (message "Turn off acm filter.")))

(defun acm-filter-insert-char ()
  "Insert filter character."
  (interactive)
  (when acm-filter-overlay
    ;; Append current character in `acm-filter-string'.
    (setq-local acm-filter-string (concat acm-filter-string (format "%s" (this-command-keys))))

    (acm-filter-update)))

(defun acm-filter-delete-char ()
  "Delete filter character."
  (interactive)
  (when acm-filter-overlay
    (if (length= acm-filter-string 0)
        (acm-filter-off)
      ;; Delete last char of `acm-filter-string'
      (setq-local acm-filter-string (substring acm-filter-string 0 -1))

      (acm-filter-update)
      )))

(defun acm-filter-update ()
  ;; Update filter face.
  (when (facep 'hl-line)
    (set-face-background 'acm-filter-face (face-attribute 'hl-line :background)))
  (set-face-foreground 'acm-filter-face (face-attribute 'font-lock-function-name-face :foreground))

  ;; Change overlay string.
  (overlay-put acm-filter-overlay
               'after-string
               (propertize acm-filter-string
                           'face
                           'acm-filter-face))

  ;; Filter candiates.
  (acm-update))

;; Emacs 28: Do not show Acm commands with M-X
(dolist (sym '(acm-hide acm-complete acm-select-first acm-select-last acm-select-next
                        acm-select-prev acm-insert-common acm-doc-scroll-up acm-doc-scroll-down
                        acm-complete-quick-access acm-doc-toggle))
  (put sym 'completion-predicate #'ignore))

(provide 'acm)

;;; acm.el ends here
