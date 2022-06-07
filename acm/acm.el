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
;;
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

(require 'acm-icon)
(require 'acm-backend-yas)
(require 'acm-backend-elisp)
(require 'acm-backend-lsp)
(require 'acm-backend-path)
(require 'acm-backend-dabbrev)
(require 'acm-backend-tempel)

;;; Code:

(defcustom acm-menu-length 10
  "Maximal number of candidates to show."
  :type 'integer)

(defcustom acm-fetch-candidate-doc-delay 0.5
  "How many seconds to stay in your fingers will popup the candidate documentation.

Default is 0.5 second."
  :type 'float)

(defcustom acm-idle-completion-delay 1
  "How many seconds to stay in your fingers will popup the enhance completion.

Default is 1 second."
  :type 'float)

(defcustom acm-continue-commands
  ;; nil is undefined command
  '(nil ignore universal-argument universal-argument-more digit-argument self-insert-command
        "\\`acm-" "\\`scroll-other-window")
  "Continue ACM completion after executing these commands."
  :type '(repeat (choice regexp symbol)))

(defcustom acm-enable-doc t
  "Popup documentation when this option is turn on."
  :type 'boolean)

(defcustom acm-enable-icon t
  "Show icon in completion menu."
  :type 'boolean)

(defcustom acm-snippet-insert-index 8
  "Insert index of snippet candidate of menu."
  :type 'integer)

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
    (define-key map "\M-k" #'acm-hide)
    (define-key map "\C-g" #'acm-hide)
    map)
  "Keymap used when popup is shown.")

(defvar acm-buffer " *acm-buffer*")
(defvar acm-frame nil)
(defvar acm-frame-popup-buffer nil)
(defvar acm-frame-popup-point nil)
(defvar acm-frame-popup-pos nil)

(defvar acm-menu-number-cache 0)
(defvar acm-menu-max-length-cache 0)

(defvar-local acm-backend-local-items nil)
(defvar-local acm-candidates nil)
(defvar-local acm-menu-candidates nil)
(defvar-local acm-menu-index -1)
(defvar-local acm-menu-offset 0)

(defvar-local acm-enable-english-helper nil)

(defvar acm-doc-frame nil)
(defvar acm-doc-buffer " *acm-doc-buffer*")

(defvar acm-idle-completion-timer nil)
(defvar acm-fetch-doc-timer nil)

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
(defun acm-make-frame (frame-name internal-border)
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
         (border-width (if internal-border internal-border 1))
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
                   (internal-border-width . ,border-width)
                   (child-frame-border-width . ,border-width)
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

    ;; Set frame border, if border-width more than 1 pixel, don't set border.
    (when (equal border-width 1)
      (let* ((face (if (facep 'child-frame-border) 'child-frame-border 'internal-border))
             (new (face-attribute 'acm-border-face :background nil 'default)))
        (unless (equal (face-attribute face :background frame 'default) new)
          (set-face-background face new frame))))

    ;; Set frame background.
    (let ((new (face-attribute 'acm-default-face :background nil 'default)))
      (unless (equal (frame-parameter frame 'background-color) new)
        (set-frame-parameter frame 'background-color new)))

    (redirect-frame-focus frame parent)
    frame))

(defmacro acm-create-frame-if-not-exist (frame frame-buffer frame-name &optional internal-border)
  `(unless (frame-live-p ,frame)
     (setq ,frame (acm-make-frame ,frame-name ,internal-border))

     (with-current-buffer (get-buffer-create ,frame-buffer)
       ;; Install mouse ignore map
       (use-local-map acm--mouse-ignore-map)
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

     (let ((win (frame-root-window ,frame)))
       (set-window-buffer win ,frame-buffer)
       ;; Mark window as dedicated to prevent frame reuse.
       (set-window-dedicated-p win t))))

(defun acm-set-frame-position (frame x y)
  (unless (frame-visible-p frame)
    (make-frame-visible frame))
  (set-frame-position frame x y))

(defun acm-set-frame-size (frame &optional max-width max-height)
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
  (let ((bound (bounds-of-thing-at-point 'symbol)))
    (if bound
        (buffer-substring-no-properties (car bound) (cdr bound))
      "")))

(defun acm-fetch-candidate-doc ()
  (when (and (frame-live-p acm-frame)
             (frame-visible-p acm-frame))
    (let ((candidate (acm-menu-current-candidate)))
      (when acm-enable-doc
        (let ((backend (plist-get candidate :backend)))
          (pcase backend
            ("lsp" (acm-backend-lsp-candidate-fetch-doc candidate))
            ("elisp" (acm-backend-elisp-candidate-fetch-doc candidate))
            ("yas" (acm-backend-yas-candidate-fetch-doc candidate))
            ("tempel" (acm-backend-tempel-candidate-fetch-doc candidate))
            (_
             (acm-doc-hide))))))))

(defun acm-idle-completion ()
  (when (and (frame-live-p acm-frame)
             (frame-visible-p acm-frame))
    (acm-backend-dabbrev-candidates-append)))

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
  (prin1-to-string (frame-parameter nil 'background-mode)))

(defun acm-candidate-fuzzy-search (keyword candiate)
  (string-match-p (regexp-quote (downcase keyword)) (downcase candiate)))

(defun acm-candidate-sort-by-prefix (keyword candiates)
  (when (and candiates
             (consp candiates))
    (unless keyword
      (setq keyword ""))

    (cl-sort candiates (lambda (a b)
                         (let ((a-include-prefix (string-prefix-p keyword (plist-get a :label)))
                               (b-include-prefix (string-prefix-p keyword (plist-get b :label))))
                           (not (and b-include-prefix (not a-include-prefix))))))))

(defun acm-update-candiates ()
  (let* ((keyword (acm-get-input-prefix))
         (candidates (list))
         path-candidates
         yas-candidates
         tempel-candidates
         mode-candidates)

    (if acm-enable-english-helper
        (progn
          (require 'acm-backend-english-data)
          (require 'acm-backend-english)

          (setq candidates (acm-backend-english-candidates keyword)))

      (setq path-candidates (acm-backend-path-candidates keyword))

      (if (> (length path-candidates) 0)
          (setq candidates path-candidates)
        (setq mode-candidates (append
                               (acm-backend-elisp-candidates keyword)
                               (acm-backend-lsp-candidates keyword)))
        (setq yas-candidates (acm-backend-yas-candidates keyword))
        (setq tempel-candidates (acm-backend-tempel-candidates keyword))

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
  (let* ((keyword (acm-get-input-prefix))
         (candidates (acm-update-candiates))
         (bounds (bounds-of-thing-at-point 'symbol)))

    (cond
     ((and (equal (length candidates) 1)
           (string-equal keyword (plist-get (nth 0 candidates) :label)))
      (acm-hide))
     ((> (length candidates) 0)
      (let* ((menu-old-max-length acm-menu-max-length-cache)
             (menu-old-number acm-menu-number-cache)
             (is-dark-mode (string-equal (acm-get-theme-mode) "dark"))
             (blend-background (if is-dark-mode "#000000" "#AAAAAA")))
        (acm-mode 1)

        (add-hook 'pre-command-hook #'acm--pre-command nil 'local)

        (acm-doc-hide)

        (unless acm-idle-completion-timer
          (setq acm-idle-completion-timer
                (run-with-idle-timer acm-idle-completion-delay t #'acm-idle-completion)))

        (unless acm-fetch-doc-timer
          (setq acm-fetch-doc-timer
                (run-with-idle-timer acm-fetch-candidate-doc-delay t #'acm-fetch-candidate-doc)))

        (setq-local acm-candidates candidates)
        (setq-local acm-menu-candidates
                    (cl-subseq acm-candidates
                               0 (min (length acm-candidates)
                                      acm-menu-length)))
        (setq-local acm-menu-index (if (zerop (length acm-menu-candidates)) -1 0))
        (setq-local acm-menu-offset 0)

        (set-face-attribute 'acm-buffer-size-face nil :height (face-attribute 'default :height))

        (when (equal (face-attribute 'acm-default-face :background) 'unspecified)
          (set-face-background 'acm-default-face (acm-color-blend (face-attribute 'default :background) blend-background (if is-dark-mode 0.8 0.9))))
        (when (equal (face-attribute 'acm-select-face :background) 'unspecified)
          (set-face-background 'acm-select-face (acm-color-blend (face-attribute 'default :background) blend-background 0.6)))
        (when (equal (face-attribute 'acm-select-face :foreground) 'unspecified)
          (set-face-foreground 'acm-select-face (face-attribute 'font-lock-function-name-face :foreground)))

        (setq acm-frame-popup-point (or (car bounds) (point)))
        (let* ((edges (window-pixel-edges))
               (pos (posn-x-y (posn-at-point acm-frame-popup-point))))
          (setq acm-frame-popup-pos
                (save-excursion
                  (backward-char (length (acm-get-input-prefix)))
                  (cons (+ (car pos) (nth 0 edges))
                        (+ (cdr pos)
                           (nth 1 edges)
                           (if (version< emacs-version "27.0")
                               (window-header-line-height)
                             (+ (window-tab-line-height)
                                (window-header-line-height))))))))
        (setq acm-frame-popup-buffer (current-buffer))

        (acm-create-frame-if-not-exist acm-frame acm-buffer "acm frame")

        (acm-menu-render menu-old-max-length (acm-menu-max-length) menu-old-number (length acm-menu-candidates))))
     (t
      (acm-hide)))))

(defun acm-hide ()
  (interactive)
  (acm-mode -1)

  (when (frame-live-p acm-frame)
    (make-frame-invisible acm-frame))

  (acm-doc-hide)

  (setq acm-menu-max-length-cache 0)

  (remove-hook 'pre-command-hook #'acm--pre-command 'local)

  (when acm-idle-completion-timer
    (cancel-timer acm-idle-completion-timer)
    (setq acm-idle-completion-timer nil))

  (when acm-fetch-doc-timer
    (cancel-timer acm-fetch-doc-timer)
    (setq acm-fetch-doc-timer nil)))

(defun acm-doc-hide ()
  (when (frame-live-p acm-doc-frame)
    (make-frame-invisible acm-doc-frame)))

(defun acm--pre-command ()
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
        ("tempel" (acm-backend-tempel-candidate-expand candidate-info bound-start))
        (_
         (delete-region bound-start (point))
         (insert (plist-get candidate-info :label)))
        )))

  (acm-hide))

(defun acm-insert-common ()
  (interactive)
  (when (and (frame-live-p acm-frame)
             (frame-visible-p acm-frame))
    (let* ((common-string "")
           (items (mapcar (lambda (v) (plist-get v :label)) acm-menu-candidates))
           (item-min-length (cl-reduce #'min (mapcar #'string-width items)))
           (input-prefix (acm-get-input-prefix)))
      (dolist (index (number-sequence 0 (1- item-min-length)))
        (let* ((char-list (mapcar (lambda (i) (substring i index (1+ index))) items))
               (first-char (first char-list)))
          (when (every (lambda (x) (string-equal x first-char)) char-list)
            (setq common-string (concat common-string first-char)))))

      (if (and (> (length common-string) 0)
               (> (length common-string) (length input-prefix)))
          (insert (substring common-string (length (acm-get-input-prefix))))
        (message "No common string found")))))

(defun acm-toggle-english-helper ()
  (interactive)
  (if acm-enable-english-helper
      (message "Turn off english helper.")
    (message "Turn on english helper."))
  (setq-local acm-enable-english-helper (not acm-enable-english-helper)))

(defun acm-menu-max-length ()
  (cl-reduce #'max
             (mapcar (lambda (v)
                       (string-width (format "%s %s" (plist-get v :display-label) (plist-get v :annotation))))
                     acm-menu-candidates)))

(defun acm-menu-render-items (items menu-index)
  (let ((item-index 0))
    (dolist (v items)
      (let* ((icon (cdr (assoc (downcase (plist-get v :icon)) acm-icon-alist)))
             (candidate (plist-get v :display-label))
             (annotation (plist-get v :annotation))
             (annotation-text (if annotation annotation ""))
             (item-length (string-width annotation-text))
             (icon-text (if icon (acm-icon-build (nth 0 icon) (nth 1 icon) (nth 2 icon)) ""))
             candidate-line)

        (when (plist-get v :deprecated)
          (add-face-text-property 0 (length candidate) 'acm-deprecated-face 'append candidate))

        (setq candidate-line
              (concat
               icon-text
               candidate
               (propertize " "
                           'display
                           (acm-indent-pixel
                            (ceiling (* (window-font-width) (- (+ acm-menu-max-length-cache 20) item-length)))))
               (propertize (format "%s \n" (capitalize annotation-text))
                           'face
                           (if (equal item-index menu-index)
                               'acm-select-face
                             'font-lock-doc-face))
               ))

        (when (equal item-index menu-index)
          (add-face-text-property 0 (length candidate-line) 'acm-select-face 'append candidate-line)

          (when (and
                 (not (member (plist-get v :backend) '("lsp" "elisp" "yas")))
                 (frame-live-p acm-doc-frame)
                 (frame-visible-p acm-doc-frame))
            (acm-doc-hide)))

        (insert candidate-line)

        (when (equal item-index (1- (length items)))
          (delete-backward-char 1))

        (setq item-index (1+ item-index))))))

(defun acm-menu-adjust-pos ()
  (let* ((emacs-width (frame-pixel-width))
         (emacs-height (frame-pixel-height))
         (acm-frame-width (frame-pixel-width acm-frame))
         (acm-frame-height (frame-pixel-height acm-frame))
         (cursor-x (car acm-frame-popup-pos))
         (cursor-y (cdr acm-frame-popup-pos))
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

      (acm-create-frame-if-not-exist acm-doc-frame acm-doc-buffer "acm doc frame" 10)

      (with-current-buffer (get-buffer-create acm-doc-buffer)
        (erase-buffer)
        (insert candidate-doc)
        (visual-line-mode 1))

      (acm-doc-fame-adjust)
      )))

(defun acm-doc-fame-adjust ()
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

    (acm-set-frame-size acm-doc-frame
                        (ceiling (/ acm-doc-frame-max-width (frame-char-width)))
                        (ceiling (/ acm-doc-frame-max-height (window-default-line-height))))

    (let* ((acm-doc-frame-width (frame-pixel-width acm-doc-frame))
           (acm-doc-frame-x (if (> acm-frame-left-distance acm-frame-right-distance)
                                (- acm-frame-x acm-doc-frame-width)
                              (+ acm-frame-x acm-frame-width)))
           (acm-doc-frame-y acm-frame-y))
      (acm-set-frame-position acm-doc-frame acm-doc-frame-x acm-doc-frame-y))))

(defun acm-menu-current-candidate ()
  (nth (+ acm-menu-offset acm-menu-index) acm-candidates))

(defun acm-menu-render (menu-old-max-length menu-new-max-length menu-old-number menu-new-number)
  (let* ((items acm-menu-candidates)
         (menu-index acm-menu-index))
    (setq acm-menu-max-length-cache menu-new-max-length)
    (setq acm-menu-number-cache menu-new-number)

    (with-current-buffer (get-buffer-create acm-buffer)
      (erase-buffer)
      (acm-menu-render-items items menu-index))

    (when (or (not (equal menu-old-max-length menu-new-max-length))
              (not (equal menu-old-number menu-new-number)))
      (acm-set-frame-size acm-frame)

      (when (and (frame-live-p acm-doc-frame)
                 (frame-visible-p acm-doc-frame))
        (acm-doc-fame-adjust)))

    (acm-menu-adjust-pos)))

(defun acm-update-completion-data (backend-name completion-table)
  ;; Update completion table that match backend-name.
  (puthash backend-name completion-table acm-backend-local-items))

(defmacro acm-silent (&rest body)
  "Silence BODY."
  (declare (indent 0))
  `(cl-letf ((inhibit-message t)
             (message-log-max nil)
             ((symbol-function #'minibuffer-message) #'ignore))
     (ignore-errors ,@body)))

(defun acm-menu-update-candidates ()
  (let ((menu-length (length acm-menu-candidates)))
    (when (> (length acm-candidates) menu-length)
      (setq-local acm-menu-candidates
                  (cl-subseq acm-candidates
                             acm-menu-offset
                             (+ acm-menu-offset menu-length))))))

(defmacro acm-menu-update (&rest body)
  `(let* ((menu-old-index acm-menu-index)
          (menu-old-offset acm-menu-offset)
          (menu-old-max-length acm-menu-max-length-cache)
          (menu-old-number acm-menu-number-cache))
     ,@body

     (when (or (not (equal menu-old-index acm-menu-index))
               (not (equal menu-old-offset acm-menu-offset)))
       (acm-menu-update-candidates)
       (acm-menu-render menu-old-max-length (acm-menu-max-length) menu-old-number (length acm-menu-candidates))
       )))

(defun acm-is-elisp-mode ()
  (or (derived-mode-p 'emacs-lisp-mode)
      (derived-mode-p 'inferior-emacs-lisp-mode)
      (derived-mode-p 'lisp-interaction-mode)))

(defun acm-select-first ()
  (interactive)
  (acm-menu-update
   (setq-local acm-menu-offset 0)
   (setq-local acm-menu-index 0)))

(defun acm-select-last ()
  (interactive)
  (acm-menu-update
   (let ((menu-length (length acm-menu-candidates)))
     (setq-local acm-menu-offset (- (length acm-candidates) menu-length))
     (setq-local acm-menu-index (- menu-length 1))
     )))

(defun acm-select-next ()
  (interactive)
  (acm-menu-update
   (cond ((< acm-menu-index (1- (length acm-menu-candidates)))
          (setq-local acm-menu-index (1+ acm-menu-index)))
         ((< (+ acm-menu-offset acm-menu-index) (1- (length acm-candidates)))
          (setq-local acm-menu-offset (1+ acm-menu-offset))))))

(defun acm-select-prev ()
  (interactive)
  (acm-menu-update
   (cond ((> acm-menu-index 0)
          (setq-local acm-menu-index (1- acm-menu-index)))
         ((> acm-menu-offset 0)
          (setq-local acm-menu-offset (1- acm-menu-offset))))))

(provide 'acm)

;;; acm.el ends here
