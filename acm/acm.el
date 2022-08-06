;; -*- lexical-binding: t; -*-
;;; acm.el --- Asynchronous Completion Menu

;; Filename: acm.el
;; Description: Asynchronous Completion Menu
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-05-31 16:29:33
;; Version: 0.1
;; Last-Updated: 2022-05-31 16:29:33
;;           By: Andy Stewart
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
(require 'acm-backend-yas)
(require 'acm-backend-elisp)
(require 'acm-backend-lsp)
(require 'acm-backend-path)
(require 'acm-backend-search-words)
(require 'acm-backend-tempel)
(require 'acm-backend-telega)
(require 'acm-quick-access)

;;; Code:

(defcustom acm-menu-length 10
  "Maximal number of candidates to show."
  :type 'integer)

(defcustom acm-continue-commands
  ;; nil is undefined command
  '(nil ignore universal-argument universal-argument-more digit-argument self-insert-command org-self-insert-command
        "\\`acm-" "\\`scroll-other-window")
  "Continue ACM completion after executing these commands."
  :type '(repeat (choice regexp symbol)))

(defcustom acm-enable-doc t
  "Popup documentation automatically when this option is turn on."
  :type 'boolean)

(defcustom acm-enable-icon t
  "Show icon in completion menu."
  :type 'boolean)

(defcustom acm-enable-quick-access nil
  "Show quick-access in completion menu."
  :type 'boolean)

(defcustom acm-snippet-insert-index 8
  "Insert index of snippet candidate of menu."
  :type 'integer)

(defcustom acm-candidate-match-function 'regexp-quote
  "acm candidate match function."
  :type '(choice (const regexp-quote)
                 (const orderless-literal)
                 (const orderless-prefixes)
                 (const orderless-flex)
                 (const orderless-regexp)
                 (const orderless-initialism)))

(defcustom acm-doc-frame-max-lines 20
  "Max line lines of doc frame."
  :type 'integer)

(cl-defmacro acm-run-idle-func (timer idle func)
  `(unless ,timer
     (setq ,timer
           (run-with-idle-timer ,idle t #'(lambda () (funcall ,func))))))

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
    (define-key map "\C-m" #'acm-complete)
    (define-key map "\t" #'acm-complete)
    (define-key map "\n" #'acm-complete)
    (define-key map "\M-h" #'acm-complete)
    (define-key map "\M-H" #'acm-insert-common)
    (define-key map "\M-d" #'acm-doc-toggle)
    (define-key map "\M-j" #'acm-doc-scroll-up)
    (define-key map "\M-k" #'acm-doc-scroll-down)
    (define-key map "\M-l" #'acm-hide)
    (define-key map "\C-g" #'acm-hide)
    (acm-keymap--bind-quick-access map)
    map)
  "Keymap used when popup is shown.")

(defvar acm-buffer " *acm-buffer*")
(defvar acm-frame nil)
(defvar acm-frame-popup-point nil)
(defvar acm-frame-popup-position nil)

(defvar acm-menu-number-cache 0)
(defvar acm-menu-max-length-cache 0)

(defvar-local acm-candidates nil)
(defvar-local acm-menu-candidates nil)
(defvar-local acm-menu-index -1)
(defvar-local acm-menu-offset 0)

(defvar-local acm-enable-english-helper nil)

(defvar acm-doc-frame nil)
(defvar acm-doc-buffer " *acm-doc-buffer*")
(defvar acm--mouse-ignore-map
  (let ((map (make-sparse-keymap)))
    (dotimes (i 7)
      (dolist (k '(mouse down-mouse drag-mouse double-mouse triple-mouse))
        (define-key map (vector (intern (format "%s-%s" k (1+ i)))) #'ignore)))
    map)
  "Ignore all mouse clicks.")

(defface acm-default-face
  '()
  "Default face, foreground and background colors used for the popup.")

(defface acm-buffer-size-face
  '()
  "Face for content area.")

(defface acm-select-face
  '()
  "Face used to highlight the currently selected candidate.")

(defface acm-border-face
  '((((class color) (min-colors 88) (background dark)) :background "#323232")
    (((class color) (min-colors 88) (background light)) :background "#d7d7d7")
    (t :background "gray"))
  "The background color used for the thin border.")

(defface acm-deprecated-face
  '((t :inherit shadow :strike-through t))
  "Face used for deprecated candidates.")

(defconst acm-fit-frame-to-buffer
  (if (functionp 'fit-frame-to-buffer-1)
      'fit-frame-to-buffer-1
    'fit-frame-to-buffer)
  "Function used to fit frame to buffer.")

(defsubst acm-indent-pixel (xpos)
  "Return a display property that aligns to XPOS."
  `(space :align-to (,xpos)))

(define-minor-mode acm-mode
  "LSP Bridge mode."
  :keymap acm-mode-map
  :init-value nil)

(defvar x-gtk-resize-child-frames) ;; not present on non-gtk builds
(defun acm-make-frame (frame-name)
  (let* ((after-make-frame-functions nil)
         (parent (selected-frame))
         (x-gtk-resize-child-frames
          (let ((case-fold-search t))
            (and
             ;; Fix resizing frame on gtk3/gnome.
             (string-match-p "gtk3" system-configuration-features)
             (string-match-p "gnome\\|cinnamon"
                             (or (getenv "XDG_CURRENT_DESKTOP")
                                 (getenv "DESKTOP_SESSION") ""))
             'resize-mode)))
         frame)
    (setq frame (make-frame
                 `((name . ,frame-name)
                   (parent-frame . ,parent)
                   (no-accept-focus . t)
                   (no-focus-on-map . t)
                   (minibuffer . nil)
                   (min-width . t)
                   (min-height . t)
                   (width . 0)
                   (height . 0)
                   (border-width . 0)
                   (internal-border-width . 1)
                   (child-frame-border-width . 1)
                   (left-fringe . 0)
                   (right-fringe . 0)
                   (vertical-scroll-bars . nil)
                   (horizontal-scroll-bars . nil)
                   (menu-bar-lines . 0)
                   (tool-bar-lines . 0)
                   (tab-bar-lines . 0)
                   (no-other-frame . t)
                   (no-other-window . t)
                   (no-delete-other-windows . t)
                   (unsplittable . t)
                   (undecorated . t)
                   (cursor-type . nil)
                   (visibility . nil)
                   (no-special-glyphs . t)
                   (desktop-dont-save . t)
                   )))

    (acm-set-frame-colors frame)

    ;; Reset to the input focus to the parent frame.
    (redirect-frame-focus frame parent)
    frame))

(cl-defmacro acm-create-frame-if-not-exist (frame frame-buffer frame-name)
  `(unless (frame-live-p ,frame)
     (setq ,frame (acm-make-frame ,frame-name))

     (with-current-buffer (get-buffer-create ,frame-buffer)
       ;; Install mouse ignore map
       (use-local-map acm--mouse-ignore-map)

       ;; Set buffer arguments.
       (dolist (var '((mode-line-format . nil)
                      (header-line-format . nil)
                      (tab-line-format . nil)
                      (tab-bar-format . nil)
                      (frame-title-format . "")
                      (truncate-lines . t)
                      (cursor-in-non-selected-windows . nil)
                      (cursor-type . nil)
                      (show-trailing-whitespace . nil)
                      (display-line-numbers . nil)
                      (left-fringe-width . nil)
                      (right-fringe-width . nil)
                      (left-margin-width . 0)
                      (right-margin-width . 0)
                      (fringes-outside-margins . 0)))
         (set (make-local-variable (car var)) (cdr var)))
       (buffer-face-set 'acm-buffer-size-face))

     ;; Set frame window and buffer.
     (let ((win (frame-root-window ,frame)))
       (set-window-buffer win ,frame-buffer)
       ;; Mark window as dedicated to prevent frame reuse.
       (set-window-dedicated-p win t))))

(defun acm-set-frame-position (frame x y)
  ;; Make sure frame visible before set position.
  (unless (frame-visible-p frame)
    ;; Force redisplay, otherwise the popup sometimes does not display content.
    (redisplay 'force)
    (make-frame-visible frame))

  (set-frame-position frame x y))

(defun acm-set-frame-size (frame &optional max-width max-height)
  ;; Set the smallest window size value to ensure that frame adjusts to the accurate size of its content.
  (let* ((window-min-height 0)
         (window-min-width 0))
    (funcall acm-fit-frame-to-buffer frame max-height nil max-width nil)))

(defun acm-match-symbol-p (pattern sym)
  "Return non-nil if SYM is matching an element of the PATTERN list."
  (and (symbolp sym)
       (cl-loop for x in pattern
                thereis (if (symbolp x)
                            (eq sym x)
                          (string-match-p x (symbol-name sym))))))

(defun acm-get-input-prefix ()
  "Get user input prefix."
  (let ((bound (bounds-of-thing-at-point 'symbol)))
    (if bound
        (buffer-substring-no-properties (car bound) (cdr bound))
      "")))

(defun acm-fetch-candidate-doc ()
  (when (acm-frame-visible-p acm-frame)
    ;; Don't fetch candidate documentation if last command is scroll operation.
    (unless (string-prefix-p "acm-doc-scroll-" (prin1-to-string last-command))
      (let* ((candidate (acm-menu-current-candidate))
             (backend (plist-get candidate :backend)))
        (pcase backend
          ("lsp" (acm-backend-lsp-candidate-fetch-doc candidate))
          ("elisp" (acm-backend-elisp-candidate-fetch-doc candidate))
          ("yas" (acm-backend-yas-candidate-fetch-doc candidate))
          ("tempel" (acm-backend-tempel-candidate-fetch-doc candidate))
          ;; Hide doc frame for backend that not support fetch candidate documentation.
          (_ (acm-doc-hide)))))))

(defun acm-color-blend (c1 c2 alpha)
  "Blend two colors C1 and C2 with ALPHA.
C1 and C2 are hexidecimal strings.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (apply (lambda (r g b)
           (format "#%02x%02x%02x"
                   (ash r -8)
                   (ash g -8)
                   (ash b -8)))
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          (color-values c1) (color-values c2))))

(defun acm-get-theme-mode ()
  "Get theme mode, dark or light."
  (prin1-to-string (frame-parameter nil 'background-mode)))

(defun acm-candidate-fuzzy-search (keyword candidate)
  "Fuzzy search candidate."
  (string-match-p (funcall acm-candidate-match-function (downcase keyword))
                  (downcase candidate)))

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

(defun acm-update-candidates ()
  (let* ((keyword (acm-get-input-prefix))
         (char-before-keyword (save-excursion
                                (backward-char (length keyword))
                                (acm-char-before)))
         (candidates (list))
         path-candidates
         yas-candidates
         tempel-candidates
         mode-candidates)

    (if acm-enable-english-helper
        ;; Completion english if option `acm-enable-english-helper' is enable.
        (progn
          (require 'acm-backend-english-data)
          (require 'acm-backend-english)

          (setq candidates (acm-backend-english-candidates keyword)))

      (setq path-candidates (acm-backend-path-candidates keyword))
      (if (> (length path-candidates) 0)
          ;; Only show path candidates if prefix is valid path.
          (setq candidates path-candidates)

        ;; Fetch syntax completion candidates.
        (setq mode-candidates (append
                               (acm-backend-elisp-candidates keyword)
                               (acm-backend-lsp-candidates keyword)
                               (acm-backend-search-words-candidates keyword)
			       (acm-backend-telega-candidates keyword)))

        ;; Don't search snippet if char before keyword is not in `acm-backend-lsp-completion-trigger-characters'.
        (when (and (boundp 'acm-backend-lsp-completion-trigger-characters))
          (unless (member char-before-keyword acm-backend-lsp-completion-trigger-characters)
            (setq yas-candidates (acm-backend-yas-candidates keyword))
            (setq tempel-candidates (acm-backend-tempel-candidates keyword))))

        ;; Insert snippet candidates in first page of menu.
        (setq candidates
              (if (> (length mode-candidates) acm-snippet-insert-index)
                  (append (cl-subseq mode-candidates 0 acm-snippet-insert-index)
                          yas-candidates
                          tempel-candidates
                          (cl-subseq mode-candidates acm-snippet-insert-index))
                (append mode-candidates yas-candidates tempel-candidates)
                ))))

    candidates))

(defun acm-update ()
  ;; Adjust `gc-cons-threshold' to maximize temporary,
  ;; make sure Emacs not do GC when filter/sort candidates.
  (let* ((gc-cons-threshold most-positive-fixnum)
         (keyword (acm-get-input-prefix))
         (candidates (acm-update-candidates))
         (bounds (bounds-of-thing-at-point 'symbol)))

    (cond
     ;; Hide completion menu if user type first candidate completely.
     ((and (equal (length candidates) 1)
           (string-equal keyword (plist-get (nth 0 candidates) :label))
           ;; Volar always send back single emmet candidate, we need filter this condition.
           (not (string-equal "Emmet Abbreviation" (plist-get (nth 0 candidates) :annotation))))
      (acm-hide))
     ((> (length candidates) 0)
      (let* ((menu-old-cache (cons acm-menu-max-length-cache acm-menu-number-cache)))
        ;; Enable acm-mode to inject mode keys.
        (acm-mode 1)

        ;; Use `pre-command-hook' to hide completion menu when command match `acm-continue-commands'.
        (add-hook 'pre-command-hook #'acm--pre-command nil 'local)

        ;; Init candidates, menu index and offset.
        (setq-local acm-candidates candidates)
        (setq-local acm-menu-candidates
                    (cl-subseq acm-candidates
                               0 (min (length acm-candidates)
                                      acm-menu-length)))
        (setq-local acm-menu-index (if (zerop (length acm-menu-candidates)) -1 0))
        (setq-local acm-menu-offset 0)

        ;; Init colors.
        (acm-init-colors)

        ;; Record menu popup position and buffer.
        (setq acm-frame-popup-point (or (car bounds) (point)))

        ;; `posn-at-point' will failed in CI, add checker make sure CI can pass.
        ;; CI don't need popup completion menu.
        (when (posn-at-point acm-frame-popup-point)
          (setq acm-frame-popup-position (acm-frame-get-popup-position))

          ;; We need delete frame first when user switch to different frame.
          (when (and (frame-live-p acm-frame)
                     (not (eq (frame-parent acm-frame) (selected-frame))))
            (acm-delete-frames))

          ;; Create menu frame if it not exists.
          (acm-create-frame-if-not-exist acm-frame acm-buffer "acm frame")

          ;; Render menu.
          (acm-menu-render menu-old-cache))
        ))
     (t
      (acm-hide)))))

(defun acm-delete-frames ()
  (when (frame-live-p acm-frame)
    (delete-frame acm-frame)
    (setq acm-frame nil))

  (when (frame-live-p acm-doc-frame)
    (delete-frame acm-doc-frame)
    (setq acm-doc-frame nil)))

(defun acm-init-colors (&optional force)
  (let* ((is-dark-mode (string-equal (acm-get-theme-mode) "dark"))
         (blend-background (if is-dark-mode "#000000" "#AAAAAA")))
    ;; Make sure font size of frame same as Emacs.
    (set-face-attribute 'acm-buffer-size-face nil :height (face-attribute 'default :height))

    ;; Make sure menu follow the theme of Emacs.
    (when (or force (equal (face-attribute 'acm-default-face :background) 'unspecified))
      (set-face-background 'acm-default-face (acm-color-blend (face-attribute 'default :background) blend-background (if is-dark-mode 0.8 0.9))))
    (when (or force (equal (face-attribute 'acm-select-face :background) 'unspecified))
      (set-face-background 'acm-select-face (acm-color-blend (face-attribute 'default :background) blend-background 0.6)))
    (when (or force (equal (face-attribute 'acm-select-face :foreground) 'unspecified))
      (set-face-foreground 'acm-select-face (face-attribute 'font-lock-function-name-face :foreground)))))

(defun acm-set-frame-colors (frame)
  ;; Set frame border color.
  (let* ((face (if (facep 'child-frame-border) 'child-frame-border 'internal-border))
         (new (face-attribute 'acm-border-face :background nil 'default)))
    (unless (equal (face-attribute face :background frame 'default) new)
      (set-face-background face new frame)))

  ;; Set frame background color.
  (let ((new (face-attribute 'acm-default-face :background nil 'default)))
    (unless (equal (frame-parameter frame 'background-color) new)
      (set-frame-parameter frame 'background-color new))))

(defun acm-reset-colors (&rest args)
  ;; Reset colors.
  (acm-init-colors t)

  ;; Reset frame colors.
  (when (acm-frame-visible-p acm-frame)
    (acm-set-frame-colors acm-frame)
    (acm-menu-render (cons acm-menu-max-length-cache acm-menu-number-cache)))
  (when (acm-frame-visible-p acm-doc-frame)
    (acm-set-frame-colors acm-doc-frame)))

(advice-add #'load-theme :after #'acm-reset-colors)

(defun acm-frame-get-popup-position ()
  (let* ((edges (window-pixel-edges))
         (window-left (+ (nth 0 edges)
                         ;; We need adjust left margin for buffer centering module.
                         (/ (- (window-pixel-width)
                               (window-body-width nil t))
                            2)))
         (window-top (nth 1 edges))
         (pos (posn-x-y (posn-at-point acm-frame-popup-point)))
         (x (car pos))
         (y (cdr pos))
         (offset-y
          ;; We need move down to skip tab-line and header-line.
          (if (version< emacs-version "27.0")
              (window-header-line-height)
            (+ (window-tab-line-height)
               (window-header-line-height)))))
    (cons (+ x window-left)
          (+ y window-top offset-y))))

(defun acm-hide ()
  (interactive)
  ;; Turn off `acm-mode'.
  (acm-mode -1)

  ;; Hide menu frame.
  (when (frame-live-p acm-frame)
    (make-frame-invisible acm-frame))

  ;; Hide doc frame.
  (acm-doc-hide)

  ;; Clean `acm-menu-max-length-cache'.
  (setq acm-menu-max-length-cache 0)

  ;; Remove hook of `acm--pre-command'.
  (remove-hook 'pre-command-hook #'acm--pre-command 'local))

(defun acm-cancel-timer (timer)
  `(when ,timer
     (cancel-timer ,timer)
     (setq ,timer nil)))

(defun acm-doc-hide ()
  (when (frame-live-p acm-doc-frame)
    (make-frame-invisible acm-doc-frame)))

(defun acm--pre-command ()
  ;; Use `pre-command-hook' to hide completion menu when command match `acm-continue-commands'.
  (unless (acm-match-symbol-p acm-continue-commands this-command)
    (acm-hide)))

(defun acm-complete ()
  (interactive)

  (let ((candidate-info (acm-menu-current-candidate))
        (bound-start acm-frame-popup-point))
    (let ((backend (plist-get candidate-info :backend)))
      (pcase backend
        ("lsp" (acm-backend-lsp-candidate-expand candidate-info bound-start))
        ("yas" (acm-backend-yas-candidate-expand candidate-info bound-start))
        ("path" (acm-backend-path-candidate-expand candidate-info bound-start))
        ("search-words" (acm-backend-search-words-candidate-expand candidate-info bound-start))
        ("tempel" (acm-backend-tempel-candidate-expand candidate-info bound-start))
        ("english" (acm-backend-english-candidate-expand candidate-info bound-start))
        (_
         (delete-region bound-start (point))
         (insert (plist-get candidate-info :label)))
        )))

  ;; Hide menu and doc frame after complete candidate.
  (acm-hide))

(defun acm-insert-common ()
  "Insert common prefix of menu."
  (interactive)
  (when (acm-frame-visible-p acm-frame)
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
                                (format "%s %s" (plist-get v :display-label) (plist-get v :annotation))))
                     acm-menu-candidates)))

(defun acm-menu-render-items (items menu-index)
  (let ((item-index 0))
    (dolist (v items)
      (let* ((icon (cdr (assoc (downcase (plist-get v :icon)) acm-icon-alist)))
             (candidate (plist-get v :display-label))
             (annotation (plist-get v :annotation))
             (annotation-text (if annotation annotation ""))
             (item-length (funcall acm-string-width-function annotation-text))
             (icon-text (if icon (acm-icon-build (nth 0 icon) (nth 1 icon) (nth 2 icon)) ""))
             (quick-access-key (nth item-index acm-quick-access-keys))
             candidate-line)

        ;; Render deprecated candidate.
        (when (plist-get v :deprecated)
          (add-face-text-property 0 (length candidate) 'acm-deprecated-face 'append candidate))

        ;; Build candidate line.
        (setq candidate-line
              (concat
               icon-text
               (when acm-enable-quick-access
                 (if quick-access-key (concat quick-access-key ". ") "   "))
               candidate
               ;; Fill in the blank according to the maximum width, make sure marks align right of menu.
               (propertize " "
                           'display
                           (acm-indent-pixel
                            (if (equal acm-string-width-function 'string-pixel-width)
                                (- (+ acm-menu-max-length-cache (* 20 (string-pixel-width " "))) item-length)
                              (ceiling (* (window-font-width) (- (+ acm-menu-max-length-cache 20) item-length))))))
               (propertize (format "%s \n" (capitalize annotation-text))
                           'face
                           (if (equal item-index menu-index) 'acm-select-face 'font-lock-doc-face))))

        ;; Render current candidate.
        (when (equal item-index menu-index)
          (add-face-text-property 0 (length candidate-line) 'acm-select-face 'append candidate-line)

          ;; Hide doc frame if some backend not support fetch candidate documentation.
          (when (and
                 (not (member (plist-get v :backend) '("lsp" "elisp" "yas")))
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
         (acm-frame-width (frame-pixel-width acm-frame))
         (acm-frame-height (frame-pixel-height acm-frame))
         (cursor-x (car acm-frame-popup-position))
         (cursor-y (cdr acm-frame-popup-position))
         (offset-x (* (window-font-width) acm-icon-width))
         (offset-y (line-pixel-height))
         (acm-frame-x (if (> (+ cursor-x acm-frame-width) emacs-width)
                          (- cursor-x acm-frame-width)
                        (max (- cursor-x offset-x) 0)))
         (acm-frame-y (if (> (+ cursor-y acm-frame-height) emacs-height)
                          (- cursor-y acm-frame-height)
                        (+ cursor-y offset-y))))
    (acm-set-frame-position acm-frame acm-frame-x acm-frame-y)))

(defun acm-doc-show ()
  (when acm-enable-doc
    (let* ((candidate (acm-menu-current-candidate))
           (backend (plist-get candidate :backend))
           (candidate-doc
            (pcase backend
              ("lsp" (acm-backend-lsp-candidate-doc candidate))
              ("elisp" (acm-backend-elisp-candidate-doc candidate))
              ("yas" (acm-backend-yas-candidate-doc candidate))
              ("tempel" (acm-backend-tempel-candidate-doc candidate))
              (_ ""))))
      (when (and candidate-doc
                 (not (string-equal candidate-doc "")))
        ;; Create doc frame if it not exist.
        (acm-create-frame-if-not-exist acm-doc-frame acm-doc-buffer "acm doc frame")

        ;; Insert documentation and turn on wrap line.
        (with-current-buffer (get-buffer-create acm-doc-buffer)
          (erase-buffer)
          (insert candidate-doc)
          (visual-line-mode 1))

        ;; Adjust doc frame position and size.
        (acm-doc-frame-adjust)
        ))))

(defun acm-doc-frame-adjust ()
  (let* ((emacs-width (frame-pixel-width))
         (emacs-height (frame-pixel-height))
         (acm-frame-width (frame-pixel-width acm-frame))
         (acm-frame-height (frame-pixel-height acm-frame))
         (acm-frame-pos (frame-position acm-frame))
         (acm-frame-x (car acm-frame-pos))
         (acm-frame-y (cdr acm-frame-pos))

         (acm-frame-left-distance acm-frame-x)
         (acm-frame-right-distance (- emacs-width acm-frame-x acm-frame-width))
         (acm-frame-top-distance acm-frame-y)
         (acm-frame-bottom-distance (- emacs-height acm-frame-y acm-frame-height))

         (acm-doc-frame-max-width (max acm-frame-left-distance acm-frame-right-distance))
         (acm-doc-frame-max-height (max acm-frame-top-distance acm-frame-bottom-distance)))

    ;; Make sure doc frame size not out of Emacs area.
    (acm-set-frame-size acm-doc-frame
                        (ceiling (/ acm-doc-frame-max-width (frame-char-width)))
                        (min (ceiling (/ acm-doc-frame-max-height (window-default-line-height)))
                             acm-doc-frame-max-lines))

    ;; Adjust doc frame with it's size.
    (let* ((acm-doc-frame-width (frame-pixel-width acm-doc-frame))
           (acm-doc-frame-x (if (> acm-frame-left-distance acm-frame-right-distance)
                                (- acm-frame-x acm-doc-frame-width)
                              (+ acm-frame-x acm-frame-width)))
           (acm-doc-frame-y acm-frame-y))
      (acm-set-frame-position acm-doc-frame acm-doc-frame-x acm-doc-frame-y))))

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
      (acm-set-frame-size acm-frame)

      ;; Adjust doc frame with menu frame position.
      (when (acm-frame-visible-p acm-doc-frame)
        (acm-doc-frame-adjust)))

    ;; Adjust menu frame position.
    (acm-menu-adjust-pos)

    ;; Fetch `documentation' and `additionalTextEdits' information.
    (acm-fetch-candidate-doc)
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

     ;; Only update menu candidates when menu index or offset changed.
     (when (or (not (equal menu-old-index acm-menu-index))
               (not (equal menu-old-offset acm-menu-offset)))
       (acm-menu-update-candidates)
       (acm-menu-render menu-old-cache)
       )))

(defun acm-char-before ()
  (let ((prev-char (char-before)))
    (if prev-char (char-to-string prev-char) "")))

(defun acm-frame-visible-p (frame)
  (and (frame-live-p frame)
       (frame-visible-p frame)))

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

(defun acm-doc-scroll-up ()
  (interactive)
  (with-current-buffer acm-doc-buffer
    (when (framep acm-frame)
      (with-selected-frame acm-doc-frame
        (scroll-up-command)))))

(defun acm-doc-scroll-down ()
  (interactive)
  (with-current-buffer acm-doc-buffer
    (when (framep acm-frame)
      (with-selected-frame acm-doc-frame
        (scroll-down-command)))))

(defun acm-doc-toggle ()
  "Toggle documentation preview for selected candidate."
  (interactive)
  (if (acm-frame-visible-p acm-doc-frame)
      (acm-doc-hide)
    (let ((acm-enable-doc t))
      (acm-doc-show))))

;; Emacs 28: Do not show Acm commands with M-X
(dolist (sym '(acm-hide acm-complete acm-select-first acm-select-last acm-select-next
               acm-select-prev acm-insert-common acm-doc-scroll-up acm-doc-scroll-down
               acm-complete-quick-access acm-doc-toggle))
  (put sym 'completion-predicate #'ignore))

(provide 'acm)

;;; acm.el ends here
