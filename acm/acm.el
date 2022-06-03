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
  '(nil ignore universal-argument universal-argument-more digit-argument
        "\\`acm-" "\\`scroll-other-window")
  "Continue ACM completion after executing these commands."
  :type '(repeat (choice regexp symbol)))

(defvar  acm-icon-collections
  '(("bootstrap" . "https://icons.getbootstrap.com/icons/%s.svg")
    ("material" . "https://raw.githubusercontent.com/Templarian/MaterialDesign/master/svg/%s.svg")
    ("octicons" . "https://raw.githubusercontent.com/primer/octicons/master/icons/%s-24.svg")
    ("boxicons" . "https://boxicons.com/static/img/svg/regular/bx-%s.svg")))

(defvar acm-icon-alist
  `((unknown . ("material" "file-find-outline" "#74d2e7"))
    (text . ("material" "format-text" "#fe5000"))
    (method . ("material" "cube" "#da1884"))
    (function . ("material" "function" "#ff6a00"))
    (fun . ("material" "function-variant" "#0abf53"))
    (constructor . ("material" "all-inclusive" "#7ac143"))
    (ctor . ("material" "cube" "#b84592"))
    (field . ("material" "tag" "#ff6c5f"))
    (variable . ("material" "variable" "#00b2a9"))
    (var . ("material" "application-variable" "#e04646"))
    (class . ("material" "video-input-component" "#ef5734"))
    (interface . ("material" "share" "#6cbc35"))
    (i/f . ("material" "share" "#ee3322"))
    (module . ("material" "sim-outline" "#00c4cc"))
    (mod . ("material" "view-module" "#ff6908"))
    (property . ("material" "wrench" "#bf033b"))
    (prop . ("material" "tools" "#0eb24e"))
    (unit . ("material" "video-input-hdmi" "#98c807"))
    (value . ("material" "format-align-right" "#ff0092"))
    (enum . ("material" "database" "#dc5034"))
    (keyword . ("material" "filter" "#0085c3"))
    (k/w . ("material" "filter-outline" "#ed6856"))
    (snippet . ("material" "format-align-center" "#f05d21"))
    (sn . ("material" "format-align-center" "#f69653"))
    (color . ("material" "palette" "#099d84"))
    (file . ("material" "file-outline" "#e30061"))
    (reference . ("material" "bookmark-box-multiple" "#954a97"))
    (ref . ("material" "bookmark-box-multiple-outline" "#006e96"))
    (folder . ("material" "folder" "#f56040"))
    (dir . ("material" "folder" "#d25238"))
    (enum-member . ("material" "google-circles-extended" "#ff9900"))
    (enummember . ("material" "google-circles-extended" "#8a8acb"))
    (member . ("material" "guitar-pick" "#e55e5e"))
    (constant . ("material" "shape-square-plus" "#d1de3f"))
    (const . ("material" "shape-square-rounded-plus" "#f65314"))
    (struct . ("material" "vector-square-plus" "#96cbb3"))
    (event . ("material" "bell" "#e990ab"))
    (operator . ("material" "plus-circle-outline" "#f47b7b"))
    (op . ("material" "plus-circle-multiple-outline" "#eb0973"))
    (type-parameter . ("material" "arrow-split-vertical" "#39a6dd"))
    (param . ("material" "arrow-split-horizontal" "#ff0e83"))
    (template . ("material" "file-document-multiple" "#207c88"))
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
    (define-key map "\C-g" #'acm-hide)
    map)
  "Keymap used when popup is shown.")

(defvar acm-buffer " *acm-buffer*")
(defvar acm-frame nil)
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

(defun acm-make-frame (frame-name)
  (make-frame
   `((name . ,frame-name)
     (parent-frame . (selected-frame))
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
     )))

(defun acm-frame-background-color ()
  (let ((theme-mode (format "%s" (frame-parameter nil 'background-mode))))
    (if (string-equal theme-mode "dark") "#191a1b" "#f0f0f0")))

(defmacro acm-save-frame (&rest body)
  `(let* ((current-frame ,(selected-frame))
          (current-buffer ,(current-buffer)))
     ,@body

     (select-frame current-frame)
     (switch-to-buffer current-buffer)))

(defmacro acm-create-frame (frame frame-buffer frame-name &rest body)
  `(if (and ,frame
            (frame-live-p ,frame))
       (make-frame-visible ,frame)

     (setq ,frame (acm-make-frame ,frame-name))
     (set-frame-parameter ,frame 'background-color (acm-frame-background-color))

     (with-current-buffer (get-buffer-create ,frame-buffer)
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

(defun acm-pre-command ()
  "Insert selected candidate unless command is marked to continue completion."
  (unless (acm-match-symbol-p acm-continue-commands this-command)
    (acm-hide)))

(defun acm-popup ()
  (interactive)
  (acm-save-frame
   (acm-mode 1)

   (setq acm-frame-popup-pos (window-absolute-pixel-position))

   (acm-create-frame acm-frame acm-buffer "acm frame")

   (setq acm-insert-preview-overlay (make-overlay (point) (point) nil t t))
   (overlay-put acm-insert-preview-overlay 'intangible t)
   (overlay-put acm-insert-preview-overlay 'window (get-buffer-window))

   (acm-menu-render t)

   (add-hook 'pre-command-hook #'acm-pre-command nil 'local)
   ))

(defun acm-hide ()
  (interactive)
  (when (and acm-frame
             (frame-live-p acm-frame))
    (acm-mode -1)

    (make-frame-invisible acm-frame)
    (delete-frame acm-frame)
    (kill-buffer acm-buffer)
    (setq acm-frame nil))

  (when (and acm-doc-frame
             (frame-live-p acm-doc-frame))
    (make-frame-invisible acm-doc-frame)
    (delete-frame acm-doc-frame)
    (kill-buffer acm-doc-buffer)
    (setq acm-doc-frame nil))

  (when acm-insert-preview-overlay
    (delete-overlay acm-insert-preview-overlay)
    (setq acm-insert-preview-overlay nil))

  (remove-hook 'pre-command-hook #'acm-pre-command 'local)
  )

(defun acm-menu-max-length ()
  (cl-reduce #'max
             (mapcar '(lambda (v)
                        (string-width (format "%s %s" (plist-get v :candidate) (plist-get v :annotation))))
                     acm-menu-candidates)))

(defun acm-menu-render-items (items menu-index)
  (let ((item-index 0))
    (dolist (v items)
      (let* ((icon (cdr (assq (plist-get v :icon) acm-icon-alist)))
             (candidate (plist-get v :candidate))
             (annotation (plist-get v :annotation))
             (annotation-text (if annotation annotation ""))
             (item-length (string-width annotation-text))
             icon-text
             candidate-line)

        (setq icon-text (acm-icon-build (nth 0 icon) (nth 1 icon) (nth 2 icon)))

        (when (plist-get v :deprecated)
          (add-face-text-property 0 (length candidate) 'acm-deprecated-face 'append candidate))

        (setq candidate-line
              (concat
               icon-text
               candidate
               (propertize " "
                           'display
                           (acm-indent-pixel
                            (ceiling (* (window-font-width) (- (* acm-menu-max-length-cache 1.5) item-length)))))
               (propertize (format "%s \n" annotation-text) 'face 'font-lock-doc-face)
               ))

        (when (equal item-index menu-index)
          (add-face-text-property 0 (length candidate-line) 'acm-select-face 'append candidate-line))

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
         (candidate-doc (plist-get candidate :doc)))
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

(defun acm-menu-render (adjust-size &optional menu-max-length)
  (let* ((items acm-menu-candidates)
         (menu-index acm-menu-index))

    (setq acm-menu-max-length-cache
          (if menu-max-length menu-max-length (acm-menu-max-length)))

    (with-current-buffer (get-buffer-create acm-buffer)
      (erase-buffer)
      (acm-menu-render-items items menu-index))

    (when adjust-size
      (fit-frame-to-buffer-1 acm-frame nil nil nil nil nil nil nil))

    (acm-menu-adjust-pos)

    (overlay-put acm-insert-preview-overlay
                 'after-string
                 (propertize (plist-get (acm-menu-current-candidate) :candidate)
                             'face 'font-lock-doc-face
                             ;; Make cursor show before insert preview overlay.
                             'cursor t))

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

(defun acm-search-items (keyboard)
  (setq-local acm-candidates (list))

  (dolist (backend-hash-table (list acm-backend-global-items
                                    acm-backend-local-items))
    (when (and backend-hash-table
               (hash-table-p backend-hash-table))
      (dolist (backend-name (hash-table-keys backend-hash-table))
        (maphash
         (lambda (k v)
           (when (string-match-p (acm-build-fuzzy-regex keyboard) (plist-get v :candidate))
             (add-to-list 'acm-candidates v t)
             ))
         (gethash backend-name backend-hash-table)
         ))))

  (setq-local acm-menu-candidates
              (cl-subseq acm-candidates
                         0 (min (length acm-candidates)
                                acm-menu-length)))
  (setq-local acm-menu-index (if (zerop (length acm-menu-candidates)) -1 0))
  (setq-local acm-menu-offset 0))

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

(defun acm-test ()
  (interactive)
  (unless acm-backend-local-items
    (setq-local acm-backend-local-items (make-hash-table :test 'equal)))

  (let* ((completion-table (make-hash-table :test 'equal))
         items)
    (setq items '(
                  (:key "1" :icon unknown :candidate "expanduser" :doc "Doc for expanduser.")
                  (:key "2" :icon text :candidate "expanduser" :annotation "Snippet" :doc "Doc for expanduser dlkdjf lksjlfkjlsfd lskjdfljsljf slkjflskdjf lksdjfsdl jsdljfl sfljsdlf jsdlfjl sjlfd jsljflsdj lfjsd ldkflkdjf slkjflskdj jsdlfjsljf sljflsdj ljsdlfjsdlfj lsdjflsdjfl jsldfjlsdj ljfsdlfj aljsldjflsdjfl jslfjlsdkjfl jsdlfjs ljdflsjf ljsdlfj slfjl dsjlfjsldjf lsjfl jsldfjsldj flsdjfl jsldfj sljdf .")
                  (:key "3" :icon method :candidate "isabs" :annotation "Function" :doc "Doc for isabs.")
                  (:key "4" :icon function :candidate "isabs" :annotation "Snippet" :doc "Doc for isabs.")
                  (:key "5" :icon fun :candidate "exists" :annotation "Function" :doc "Doc for exists.")
                  (:key "6" :icon constructor :candidate "exists" :annotation "Snippet" :doc "Doc for exists.")
                  (:key "7" :icon ctor :candidate "append" :annotation "Function" :doc "Doc for append.")
                  (:key "8" :icon field :candidate "append" :annotation "Snippet" :doc "Doc for append.")
                  (:key "9" :icon variable :candidate "remove" :annotation "Function" :doc "Doc for remove.")
                  (:key "10" :icon var :candidate "remove" :annotation "Snippet" :doc "Doc for remove.")
                  (:key "11" :icon class :candidate "startswith" :annotation "Function" :doc "Doc for startswith.")
                  (:key "12" :icon interface :candidate "startswith" :doc "Doc for startswith.")
                  (:key "13" :icon i/f :candidate "endswith" :annotation "Function" :doc "Doc for endswith.")
                  (:key "14" :icon module :candidate "endswith" :annotation "Snippet" :doc "Doc for endswith.")
                  (:key "15" :icon mod :candidate "long-function-name" :annotation "Function" :doc "Doc for long-function-name.")
                  (:key "16" :icon property :candidate "long-function-name" :doc "Doc for long-function-name.")
                  (:key "17" :icon prop :candidate "cool-vars-name" :annotation "Function" :doc "Doc for cool-vars-name.")
                  (:key "18" :icon unit :candidate "cool-vars-name" :annotation "Snippet" :doc "Doc for cool-vars-name.")
                  (:key "19" :icon value :candidate "expanduser" :annotation "Function" :doc "Doc for expanduser." :deprecated t)
                  (:key "20" :icon enum :candidate "expanduser" :annotation "Snippet" :doc "Doc for expanduser.")
                  (:key "21" :icon keyword :candidate "expanduser" :annotation "Function" :doc "Doc for expanduser.")
                  (:key "22" :icon k/w :candidate "expanduser" :annotation "Snippet" :doc "Doc for expanduser.")
                  (:key "23" :icon snippet :candidate "expanduser" :annotation "Function" :doc "Doc for expanduser.")
                  (:key "24" :icon color :candidate "expanduser" :annotation "Snippet" :doc "Doc for expanduser.")
                  (:key "25" :icon file :candidate "expanduser" :annotation "Function" :doc "Doc for expanduser.")
                  ;; (:key "26" :icon reference :candidate "expanduser" :annotation "Snippet" :doc "Doc for expanduser.")
                  ;; (:key "27" :icon ref :candidate "expanduser" :annotation "Function" :doc "Doc for expanduser.")
                  ;; (:key "28" :icon folder :candidate "expanduser" :annotation "Snippet" :doc "Doc for expanduser.")
                  ;; (:key "29" :icon dir :candidate "expanduser" :annotation "Function" :doc "Doc for expanduser.")
                  ;; (:key "30" :icon enum-member :candidate "expanduser" :annotation "Snippet" :doc "Doc for expanduser.")
                  ;; (:key "31" :icon enummember :candidate "expanduser" :annotation "Function" :doc "Doc for expanduser.")
                  ;; (:key "32" :icon member :candidate "expanduser" :annotation "Snippet" :doc "Doc for expanduser.")
                  ;; (:key "33" :icon constant :candidate "expanduser" :annotation "Function" :doc "Doc for expanduser.")
                  ;; (:key "34" :icon const :candidate "expanduser" :annotation "Snippet" :doc "Doc for expanduser.")
                  ;; (:key "35" :icon event :candidate "expanduser" :annotation "Function" :doc "Doc for expanduser.")
                  ;; (:key "36" :icon operator :candidate "expanduser" :annotation "Snippet" :doc "Doc for expanduser.")
                  ;; (:key "37" :icon op :candidate "expanduser" :annotation "Function" :doc "Doc for expanduser.")
                  ;; (:key "38" :icon type-parameter :candidate "expanduser" :annotation "Snippet" :doc "Doc for expanduser.")
                  ;; (:key "39" :icon param :candidate "expanduser" :annotation "Function" :doc "Doc for expanduser.")
                  ;; (:key "40" :icon template :candidate "expanduser" :annotation "Snippet" :doc "Doc for expanduser.")
                  ;; (:key "41" :icon t :candidate "expanduser" :annotation "Function" :doc "Doc for expanduser.")
                  ))

    (dolist (item items)
      (puthash (plist-get item :key) item completion-table))

    (acm-update-completion-data "lsp-bridge" completion-table)

    (acm-search-items "e")

    (setq acm-insert-preview "hello")

    (acm-popup)))

(provide 'acm)

;;; acm.el ends here
