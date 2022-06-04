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

;;; Code:

(defcustom acm-menu-length 10
  "Maximal number of candidates to show."
  :type 'integer)

(defcustom acm-continue-commands
  ;; nil is undefined command
  '(nil ignore universal-argument universal-argument-more digit-argument self-insert-command
        "\\`acm-" "\\`scroll-other-window")
  "Continue ACM completion after executing these commands."
  :type '(repeat (choice regexp symbol)))

(defcustom acm-dabbrev-min-length 4
  "Minimum length of dabbrev expansions.
This setting ensures that words which are too short
are not offered as completion candidates, such that
auto completion does not pop up too aggressively."
  :type 'integer)

(defcustom acm-elisp-min-length 3
  "Minimum length of elisp symbol.
This setting ensures that words which are too short
are not offered as completion candidates, such that
auto completion does not pop up too aggressively."
  :type 'integer)

(defvar  acm-icon-collections
  '(("bootstrap" . "https://icons.getbootstrap.com/icons/%s.svg")
    ("material" . "https://raw.githubusercontent.com/Templarian/MaterialDesign/master/svg/%s.svg")
    ("octicons" . "https://raw.githubusercontent.com/primer/octicons/master/icons/%s-24.svg")
    ("boxicons" . "https://boxicons.com/static/img/svg/regular/bx-%s.svg")))

(defvar acm-icon-alist
  `(("unknown" . ("material" "file-find-outline" "#74d2e7"))
    ("text" . ("material" "format-text" "#98c807"))
    ("method" . ("material" "cube" "#da1884"))
    ("function" . ("material" "function" "#ff6a00"))
    ("fun" . ("material" "function-variant" "#0abf53"))
    ("constructor" . ("material" "all-inclusive" "#7ac143"))
    ("ctor" . ("material" "cube" "#b84592"))
    ("field" . ("material" "tag" "#ff6c5f"))
    ("variable" . ("material" "variable" "#00b2a9"))
    ("var" . ("material" "application-variable" "#e04646"))
    ("class" . ("material" "video-input-component" "#ef5734"))
    ("interface" . ("material" "share" "#6cbc35"))
    ("i/f" . ("material" "share" "#ee3322"))
    ("module" . ("material" "sim-outline" "#00c4cc"))
    ("mod" . ("material" "view-module" "#ff6908"))
    ("property" . ("material" "wrench" "#bf033b"))
    ("prop" . ("material" "tools" "#0eb24e"))
    ("unit" . ("material" "video-input-hdmi" "#98c807"))
    ("value" . ("material" "format-align-right" "#ff0092"))
    ("enum" . ("material" "database" "#dc5034"))
    ("keyword" . ("material" "filter" "#0085c3"))
    ("k/w" . ("material" "filter-outline" "#ed6856"))
    ("snippet" . ("material" "format-align-center" "#f05d21"))
    ("sn" . ("material" "format-align-center" "#f69653"))
    ("color" . ("material" "palette" "#099d84"))
    ("file" . ("material" "file-outline" "#e30061"))
    ("reference" . ("material" "bookmark-box-multiple" "#954a97"))
    ("ref" . ("material" "bookmark-box-multiple-outline" "#006e96"))
    ("folder" . ("material" "folder" "#f56040"))
    ("dir" . ("material" "folder" "#d25238"))
    ("enum-member" . ("material" "google-circles-extended" "#ff9900"))
    ("enummember" . ("material" "google-circles-extended" "#8a8acb"))
    ("member" . ("material" "guitar-pick" "#e55e5e"))
    ("constant" . ("material" "shape-square-plus" "#d1de3f"))
    ("const" . ("material" "shape-square-rounded-plus" "#f65314"))
    ("struct" . ("material" "vector-square-plus" "#96cbb3"))
    ("event" . ("material" "bell" "#e990ab"))
    ("operator" . ("material" "plus-circle-outline" "#f47b7b"))
    ("op" . ("material" "plus-circle-multiple-outline" "#eb0973"))
    ("type-parameter" . ("material" "arrow-split-vertical" "#39a6dd"))
    ("param" . ("material" "arrow-split-horizontal" "#ff0e83"))
    ("template" . ("material" "file-document-multiple" "#207c88"))
    ("macro" . ("material" "alpha-m-circle" "#ff9900"))
    ("face" . ("material" "palette-swatch" "#98c807"))
    ("custom" . ("material" "apple-keyboard-option" "#ed6856"))
    (t . ("material" "file-find-outline" "#90cef1"))))

(defvar lsp-bridge-buffer-parameters
  '((mode-line-format . nil)
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
    (right-fringe-width . 0)
    (left-margin-width . 0)
    (right-margin-width . 0)
    (fringes-outside-margins . 0)))

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
    (define-key map "\C-g" #'acm-hide)
    map)
  "Keymap used when popup is shown.")

(defvar acm-buffer " *acm-buffer*")
(defvar acm-frame nil)
(defvar acm-frame-popup-buffer nil)
(defvar acm-frame-popup-point nil)
(defvar acm-frame-popup-pos nil)

(defvar acm-insert-preview "")
(defvar acm-insert-preview-overlay nil)

(defvar acm-icon-cache (make-hash-table :test 'equal))
(defvar acm-icon-dir (expand-file-name "icons" (file-name-directory load-file-name)))
(defvar acm-icon-width 4)
(defvar acm-menu-max-length-cache 0)

(defvar acm-backend-global-items nil)
(defvar-local acm-backend-local-items nil)
(defvar-local acm-candidates nil)
(defvar-local acm-menu-candidates nil)
(defvar-local acm-menu-index -1)
(defvar-local acm-menu-offset 0)

(defvar acm-doc-buffer " *acm-doc-buffer*")
(defvar acm-doc-frame nil)

(defvar acm--mouse-ignore-map
  (let ((map (make-sparse-keymap)))
    (dotimes (i 7)
      (dolist (k '(mouse down-mouse drag-mouse double-mouse triple-mouse))
        (define-key map (vector (intern (format "%s-%s" k (1+ i)))) #'ignore)))
    map)
  "Ignore all mouse clicks.")

(defface acm-default-face
  '((t (:height 140)))
  "Face for content area.")

(defface acm-select-face
  '((((class color) (min-colors 88) (background dark))
     :background "#00415e" :foreground "white")
    (((class color) (min-colors 88) (background light))
     :background "#c0efff" :foreground "black")
    (t :background "blue" :foreground "white"))
  "Face used to highlight the currently selected candidate.")

(defface acm-deprecated-face
  '((t :inherit shadow :strike-through t))
  "Face used for deprecated candidates.")

(defsubst acm-indent-pixel (xpos)
  "Return a display property that aligns to XPOS."
  `(space :align-to (,xpos)))

(define-minor-mode acm-mode
  "LSP Bridge mode."
  :keymap acm-mode-map
  :init-value nil)

(defun acm-icon-filepath (collection name)
  (concat (file-name-as-directory acm-icon-dir) (format "%s_%s.svg" collection name)))

(defun acm-icon-fetch-all ()
  (interactive)
  (dolist (icon acm-icon-alist)
    (let* ((collection (nth 0 (cdr icon)))
           (name (nth 1 (cdr icon)))
           (url (format (cdr (assoc collection acm-icon-collections)) name))
           (filename (acm-icon-filepath collection name)))
      (with-temp-buffer
        (url-insert-file-contents url)
        (write-region (point-min) (point-max) filename)))))

(defun acm-icon-parse (collection name)
  (with-temp-buffer
    (insert-file-contents (acm-icon-filepath collection name))
    (xml-parse-region (point-min) (point-max))))

(defun acm-emacs-color-to-svg-color (color-name)
  "Convert Emacs COLOR-NAME to #rrggbb form.
If COLOR-NAME is unknown to Emacs, then return COLOR-NAME as-is."
  (let ((rgb-color (color-name-to-rgb color-name)))
    (if rgb-color
        (apply #'color-rgb-to-hex (append rgb-color '(2)))
      color-name)))

(defun acm-icon (collection name &optional fg-color bg-color zoom)
  (let* ((root (acm-icon-parse collection name))

         ;; Read original viewbox
         (viewbox (cdr (assq 'viewBox (xml-node-attributes (car root)))))
         (viewbox (mapcar 'string-to-number (split-string viewbox)))
         (view-x (nth 0 viewbox))
         (view-y (nth 1 viewbox))
         (view-width (nth 2 viewbox))
         (view-height (nth 3 viewbox))

         ;; Set icon size (in pixels) to 4x1 characters
         (svg-width  (* (window-font-width)  acm-icon-width))
         (svg-height (* (window-font-height) 1))

         ;; Zoom the icon by using integer factor only
         (zoom (max 1 (truncate (or zoom 1))))
         (svg-width  (* svg-width zoom))
         (svg-height (* svg-height zoom))

         (svg-viewbox (format "%f %f %f %f" view-x view-y view-width view-height))
         (fg-color (acm-emacs-color-to-svg-color
                    (or (when (facep fg-color)
                          (face-foreground fg-color nil t))
                        fg-color (face-attribute 'default :foreground))))
         (bg-color (acm-emacs-color-to-svg-color
                    (or (when (facep bg-color)
                          (face-background bg-color nil t))
                        bg-color "transparent")))
         (svg (svg-create svg-width svg-height
                          :viewBox svg-viewbox
                          :stroke-width 0
                          :fill fg-color)))
    (svg-rectangle svg
                   view-x view-y view-width view-height
                   :fill bg-color)

    (dolist (item (xml-get-children (car root) 'path))
      (let* ((attrs (xml-node-attributes item))
             (path (cdr (assoc 'd attrs)))
             (fill (or (cdr (assoc 'fill attrs)) fg-color)))
        (svg-node svg 'path :d path :fill fill)))
    (svg-image svg :ascent 'center :scale 1)))

(defvar x-gtk-resize-child-frames) ;; Not present on non-gtk builds
(defun acm-make-frame (frame-name)
  (let* ((window-min-height 1)
         (window-min-width 1)
         (x-gtk-resize-child-frames
          (let ((case-fold-search t))
            (and
             ;; XXX HACK to fix resizing on gtk3/gnome taken from posframe.el
             ;; More information:
             ;; * https://github.com/minad/corfu/issues/17
             ;; * https://gitlab.gnome.org/GNOME/mutter/-/issues/840
             ;; * https://lists.gnu.org/archive/html/emacs-devel/2020-02/msg00001.html
             (string-match-p "gtk3" system-configuration-features)
             (string-match-p "gnome\\|cinnamon"
                             (or (getenv "XDG_CURRENT_DESKTOP")
                                 (getenv "DESKTOP_SESSION") ""))
             'resize-mode)))
         (after-make-frame-functions))
    (make-frame
     `((name . ,frame-name)
       (parent-frame . (window-frame))
       (no-accept-focus . t)
       (no-focus-on-map . t)
       (minibuffer . nil)
       (min-width . t)
       (min-height . t)
       (width . 0)
       (height . 0)
       (border-width . 0)
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
       (internal-border-width . 1)
       ))))

(defun acm-frame-background-color ()
  (let ((theme-mode (format "%s" (frame-parameter nil 'background-mode))))
    (if (string-equal theme-mode "dark") "#191a1b" "#f0f0f0")))

(defun acm-insert-preview-delete ()
  (when acm-insert-preview-overlay
    (delete-overlay acm-insert-preview-overlay)
    (setq acm-insert-preview-overlay nil)))

(defmacro acm-save-frame (&rest body)
  `(let* ((current-frame ,(selected-frame)))
     (acm-insert-preview-delete)

     ,@body

     (select-frame current-frame)
     (switch-to-buffer acm-frame-popup-buffer)
     ))

(defmacro acm-create-frame (frame frame-buffer frame-name &rest body)
  `(if (and ,frame
            (frame-live-p ,frame))
       (make-frame-visible ,frame)

     (setq ,frame (acm-make-frame ,frame-name))
     (set-frame-parameter ,frame 'background-color (acm-frame-background-color))

     (with-current-buffer (get-buffer-create ,frame-buffer)
       ;; Install mouse ignore map
       (use-local-map acm--mouse-ignore-map)
       (dolist (var lsp-bridge-buffer-parameters)
         (set (make-local-variable (car var)) (cdr var)))
       (buffer-face-set 'acm-default-face))

     (with-selected-frame ,frame
       (switch-to-buffer ,frame-buffer))

     (make-frame-visible ,frame)

     ,@body
     ))

(defun acm-match-symbol-p (pattern sym)
  "Return non-nil if SYM is matching an element of the PATTERN list."
  (and (symbolp sym)
       (cl-loop for x in pattern
                thereis (if (symbolp x)
                            (eq sym x)
                          (string-match-p x (symbol-name sym))))))

(defun acm-get-point-symbol ()
  (let ((bound (bounds-of-thing-at-point 'symbol)))
    (if bound
        (buffer-substring-no-properties (car bound) (cdr bound))
      "")))

(defun acm-update ()
  (let* ((keyword (acm-get-point-symbol))
         (candidates (list))
         (bounds (bounds-of-thing-at-point 'symbol)))

    (when (and (or (derived-mode-p 'emacs-lisp-mode)
                   (derived-mode-p 'inferior-emacs-lisp-mode))
               (>= (length keyword) acm-elisp-min-length))
      (dolist (elisp-symbol (sort (all-completions keyword obarray) 'string<))
        (let ((symbol-type (acm-elisp-symbol-type (intern elisp-symbol))))
          (add-to-list 'candidates (list :key elisp-symbol
                                         :icon symbol-type
                                         :label elisp-symbol
                                         :annotation (capitalize symbol-type)
                                         :backend "elisp") t))))

    (dolist (backend-hash-table (list acm-backend-global-items
                                      acm-backend-local-items))
      (when (and backend-hash-table
                 (hash-table-p backend-hash-table))
        (dolist (backend-name (hash-table-keys backend-hash-table))
          (maphash
           (lambda (k v)
             (when (or (string-equal keyword "")
                       (string-prefix-p keyword (plist-get v :label)))
               (plist-put v :backend "lsp")
               (add-to-list 'candidates v t)))
           (gethash backend-name backend-hash-table)
           ))))

    (when (>= (length keyword) acm-dabbrev-min-length)
      (dolist (dabbrev-word (acm-dabbrev-list keyword))
        (add-to-list 'candidates (list :key dabbrev-word
                                       :icon "text"
                                       :label dabbrev-word
                                       :annotation "Dabbrev"
                                       :backend "dabbrev") t)))

    (cond
     ((and (equal (length candidates) 1)
           (string-equal keyword (plist-get (nth 0 candidates) :label)))
      (acm-hide))
     ((> (length candidates) 0)
      (acm-save-frame
       (let ((menu-old-max-length acm-menu-max-length-cache)
             menu-new-max-length)
         (acm-mode 1)
         (add-hook 'pre-command-hook #'acm--pre-command nil 'local)

         (setq-local acm-candidates candidates)
         (setq-local acm-menu-candidates
                     (cl-subseq acm-candidates
                                0 (min (length acm-candidates)
                                       acm-menu-length)))
         (setq-local acm-menu-index (if (zerop (length acm-menu-candidates)) -1 0))
         (setq-local acm-menu-offset 0)

         (setq acm-frame-popup-point (or (car bounds) (point)))

         (setq acm-frame-popup-pos
               (save-excursion
                 (backward-char (length (acm-get-point-symbol)))
                 (window-absolute-pixel-position)))

         (setq acm-frame-popup-buffer (current-buffer))

         (acm-create-frame acm-frame acm-buffer "acm frame")

         (setq menu-new-max-length (acm-menu-max-length))
         (acm-menu-render (not (equal menu-old-max-length menu-new-max-length))
                          menu-new-max-length
                          ))))
     (t
      (acm-hide))))
  nil)

(defun acm-hide ()
  (interactive)
  (acm-mode -1)

  (when (and acm-frame
             (frame-live-p acm-frame))
    (make-frame-invisible acm-frame))

  (when (and acm-doc-frame
             (frame-live-p acm-doc-frame))
    (make-frame-invisible acm-doc-frame))

  (acm-insert-preview-delete)

  (setq acm-menu-max-length-cache 0)

  (remove-hook 'pre-command-hook #'acm--pre-command 'local))

(defun acm--pre-command ()
  (unless (acm-match-symbol-p acm-continue-commands this-command)
    (acm-hide)))

(defvar acm-complete-function nil)

(defun acm-complete ()
  (interactive)
  (acm-insert-preview-delete)

  (when acm-complete-function
    (funcall acm-complete-function (acm-menu-current-candidate) acm-frame-popup-point))

  (acm-hide))

(defun acm-insert-common ()
  (interactive)
  )

(defun acm-menu-max-length ()
  (cl-reduce #'max
             (mapcar '(lambda (v)
                        (string-width (format "%s %s" (plist-get v :label) (plist-get v :annotation))))
                     acm-menu-candidates)))

(defvar acm-fetch-candidate-doc-function nil)

(defun acm-menu-render-items (items menu-index)
  (let ((item-index 0))
    (dolist (v items)
      (let* ((icon (cdr (assoc (plist-get v :icon) acm-icon-alist)))
             (candidate (plist-get v :label))
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
               (propertize (format "%s \n" (capitalize annotation-text)) 'face 'font-lock-doc-face)
               ))

        (when (equal item-index menu-index)
          (add-face-text-property 0 (length candidate-line) 'acm-select-face 'append candidate-line)

          (when acm-fetch-candidate-doc-function
            (save-excursion
              (switch-to-buffer acm-frame-popup-buffer)
              (funcall acm-fetch-candidate-doc-function v))))

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
                        (- cursor-x offset-x)))
         (acm-frame-y (if (> (+ cursor-y acm-frame-height) emacs-height)
                          (- cursor-y acm-frame-height)
                        (+ cursor-y offset-y))))
    (set-frame-position acm-frame acm-frame-x acm-frame-y)))

(defun acm-doc-show ()
  (let* ((candidate (acm-menu-current-candidate))
         (candidate-doc (plist-get candidate :documentation)))
    (when (and candidate-doc
               (not (string-equal candidate-doc "")))

      (acm-create-frame
       acm-doc-frame
       acm-doc-buffer
       "acm doc frame"
       (set-frame-size acm-doc-frame
                       (ceiling (* (frame-pixel-width acm-frame) 1.618))
                       (ceiling (* (frame-pixel-height acm-frame) 0.618)) t))

      (with-current-buffer (get-buffer-create acm-doc-buffer)
        (visual-line-mode 1)
        (erase-buffer)
        (insert candidate-doc))

      (let* ((emacs-width (frame-pixel-width))
             (acm-doc-frame-width (frame-pixel-width acm-doc-frame))
             (acm-frame-width (frame-pixel-width acm-frame))
             (acm-frame-pos (frame-position acm-frame))
             (acm-frame-x (car acm-frame-pos))
             (acm-frame-y (cdr acm-frame-pos))
             (acm-doc-frame-x (if (> (+ acm-frame-x acm-frame-width acm-doc-frame-width) emacs-width)
                                  (- acm-frame-x acm-doc-frame-width)
                                (+ acm-frame-x acm-frame-width)))
             (acm-doc-frame-y acm-frame-y))

        (set-frame-position acm-doc-frame acm-doc-frame-x acm-doc-frame-y)
        ))))

(defun acm-menu-current-candidate ()
  (nth (+ acm-menu-offset acm-menu-index) acm-candidates))

(defun acm-menu-current-candidate-expand-text ()
  (let* ((candidate (acm-menu-current-candidate))
         (label (plist-get candidate :label))
         (insert-text (plist-get candidate :insertText))
         (text-edit (plist-get candidate :textEdit))
         (new-text (plist-get text-edit :newText)))
    (or new-text insert-text label)))

(defun acm-menu-render (adjust-size &optional menu-max-length)
  (setq acm-insert-preview-overlay (make-overlay (point) (point) nil t t))
  (overlay-put acm-insert-preview-overlay 'intangible t)
  (overlay-put acm-insert-preview-overlay 'window (get-buffer-window))

  (let* ((items acm-menu-candidates)
         (menu-index acm-menu-index)
         (expand-text (acm-menu-current-candidate-expand-text)))

    (setq acm-menu-max-length-cache
          (if menu-max-length menu-max-length (acm-menu-max-length)))

    (with-current-buffer (get-buffer-create acm-buffer)
      (erase-buffer)
      (acm-menu-render-items items menu-index))

    (when adjust-size
      (fit-frame-to-buffer-1 acm-frame nil nil nil nil nil nil nil))

    (acm-menu-adjust-pos)

    (when (string-prefix-p (acm-get-point-symbol) expand-text)
      (overlay-put acm-insert-preview-overlay
                   'after-string
                   (propertize
                    (string-remove-prefix (acm-get-point-symbol) expand-text)
                    'face 'font-lock-doc-face
                    ;; Make cursor show before insert preview overlay.
                    'cursor t)))

    (acm-doc-show)
    ))

(defun acm-icon-build (collection name fg-color)
  (let* ((icon-key (format "%s_%s" collection name))
         (icon-text (gethash icon-key acm-icon-cache)))
    (unless icon-text
      (setq icon-text (propertize
                       (apply #'concat (make-list acm-icon-width "-"))
                       'display (acm-icon collection name fg-color)))
      (puthash icon-key icon-text acm-icon-cache))
    icon-text))

(defun acm-update-completion-data (backend-name completion-table)
  ;; Update completion table that match backend-name.
  (puthash backend-name completion-table acm-backend-local-items))

(defun acm-elisp-symbol-type (symbol)
  (cond ((functionp symbol)
         "function")
        ((macrop symbol)
         "macro")
        ((facep symbol)
         "face")
        ((custom-variable-p symbol)
         "custom")
        (t
         "variable")))

(defmacro acm-silent (&rest body)
  "Silence BODY."
  (declare (indent 0))
  `(cl-letf ((inhibit-message t)
             (message-log-max nil)
             ((symbol-function #'minibuffer-message) #'ignore))
     (ignore-errors ,@body)))

(defun acm-dabbrev-list (word)
  "Find all dabbrev expansions for WORD."
  (require 'dabbrev)
  (acm-silent
    (let ((dabbrev-check-other-buffers t)
          (dabbrev-check-all-buffers t))
      (dabbrev--reset-global-variables))
    (cl-loop with min-len = (+ acm-dabbrev-min-length (length word))
             for w in (dabbrev--find-all-expansions word (dabbrev--ignore-case-p word))
             if (>= (length w) min-len) collect w)))

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
          menu-new-max-length)
     ,@body

     (when (or (not (equal menu-old-index acm-menu-index))
               (not (equal menu-old-offset acm-menu-offset)))
       (acm-save-frame
        (acm-menu-update-candidates)
        (setq menu-new-max-length (acm-menu-max-length))
        (acm-menu-render (not (equal menu-old-max-length menu-new-max-length))
                         menu-new-max-length
                         )))))

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

(defun acm-build-fuzzy-regex (input)
  "Create a fuzzy regexp of PATTERN."
  (mapconcat (lambda (ch)
               (let ((s (char-to-string ch)))
                 (format "[^%s]*%s" s (regexp-quote s))))
             input ""))

(provide 'acm)

;;; acm.el ends here
