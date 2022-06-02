;; -*- lexical-binding: t; -*-
;;; lsp-bridge-ui.el --- Asynchronous Completion Menu

;; Filename: lsp-bridge-ui.el
;; Description: Asynchronous Completion Menu
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-05-31 16:29:33
;; Version: 0.1
;; Last-Updated: 2022-05-31 16:29:33
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/lsp-bridge-ui
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
;; Put lsp-bridge-ui.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lsp-bridge-ui)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET lsp-bridge-ui RET
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

;;; Code:

(defcustom lsp-bridge-ui-menu-length 10
  "Maximal number of candidates to show."
  :type 'integer)

(defvar  lsp-bridge-ui-icon-collections
  '(("bootstrap" . "https://icons.getbootstrap.com/icons/%s.svg")
    ("material" . "https://raw.githubusercontent.com/Templarian/MaterialDesign/master/svg/%s.svg")
    ("octicons" . "https://raw.githubusercontent.com/primer/octicons/master/icons/%s-24.svg")
    ("boxicons" . "https://boxicons.com/static/img/svg/regular/bx-%s.svg")))

(defvar lsp-bridge-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap next-line] #'lsp-bridge-ui-select-next)
    (define-key map [remap previous-line] #'lsp-bridge-ui-select-prev)
    (define-key map [down] #'lsp-bridge-ui-select-next)
    (define-key map [up] #'lsp-bridge-ui-select-prev)
    (define-key map "\M-n" #'lsp-bridge-ui-select-next)
    (define-key map "\M-p" #'lsp-bridge-ui-select-prev)
    (define-key map "\M-," #'lsp-bridge-ui-select-last)
    (define-key map "\M-." #'lsp-bridge-ui-select-first)
    (define-key map "\C-g" #'lsp-bridge-ui-hide)
    map)
  "Keymap used when popup is shown.")

(define-minor-mode lsp-bridge-ui-mode
  "LSP Bridge mode."
  :keymap lsp-bridge-ui-mode-map
  :init-value nil
  (if lsp-bridge-ui-mode
      (lsp-bridge-ui-enable)
    (lsp-bridge-ui-disable)))

(defun lsp-bridge-ui-enable ())

(defun lsp-bridge-ui-disable ())

(defvar lsp-bridge-ui-icon-dir (expand-file-name "icons" (file-name-directory load-file-name)))

(defvar lsp-bridge-ui-icon-cache (make-hash-table :test 'equal))

(defun lsp-bridge-ui-icon-filepath (collection name)
  (concat (file-name-as-directory lsp-bridge-ui-icon-dir) (format "%s_%s.svg" collection name)))

(defun lsp-bridge-ui-icon-fetch-all ()
  (interactive)
  (dolist (icon lsp-bridge-ui-icon-alist)
    (let* ((collection (nth 0 (cdr icon)))
           (name (nth 1 (cdr icon)))
           (url (format (cdr (assoc collection lsp-bridge-ui-icon-collections)) name))
           (filename (lsp-bridge-ui-icon-filepath collection name)))
      (with-temp-buffer
        (url-insert-file-contents url)
        (write-region (point-min) (point-max) filename)))))

(defun lsp-bridge-ui-icon-parse (collection name)
  (with-temp-buffer
    (insert-file-contents (lsp-bridge-ui-icon-filepath collection name))
    (xml-parse-region (point-min) (point-max))))

(defun lsp-bridge-ui-emacs-color-to-svg-color (color-name)
  "Convert Emacs COLOR-NAME to #rrggbb form.
If COLOR-NAME is unknown to Emacs, then return COLOR-NAME as-is."
  (let ((rgb-color (color-name-to-rgb color-name)))
    (if rgb-color
        (apply #'color-rgb-to-hex (append rgb-color '(2)))
      color-name)))

(defun lsp-bridge-ui-icon (collection name &optional fg-color bg-color zoom)
  (let* ((root (lsp-bridge-ui-icon-parse collection name))

         ;; Read original viewbox
         (viewbox (cdr (assq 'viewBox (xml-node-attributes (car root)))))
         (viewbox (mapcar 'string-to-number (split-string viewbox)))
         (view-x (nth 0 viewbox))
         (view-y (nth 1 viewbox))
         (view-width (nth 2 viewbox))
         (view-height (nth 3 viewbox))

         ;; Set icon size (in pixels) to 4x1 characters
         (svg-width  (* (window-font-width)  lsp-bridge-ui-icon-width))
         (svg-height (* (window-font-height) 1))

         ;; Zoom the icon by using integer factor only
         (zoom (max 1 (truncate (or zoom 1))))
         (svg-width  (* svg-width zoom))
         (svg-height (* svg-height zoom))

         (svg-viewbox (format "%f %f %f %f" view-x view-y view-width view-height))
         (fg-color (lsp-bridge-ui-emacs-color-to-svg-color
                    (or (when (facep fg-color)
                          (face-foreground fg-color nil t))
                        fg-color (face-attribute 'default :foreground))))
         (bg-color (lsp-bridge-ui-emacs-color-to-svg-color
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

(defvar lsp-bridge-ui-backend-global-items nil)
(defvar-local lsp-bridge-ui-backend-local-items nil)
(defvar-local lsp-bridge-ui-filter-candidates nil)
(defvar-local lsp-bridge-ui-menu-candidates nil)
(defvar-local lsp-bridge-ui-filter-index -1)
(defvar-local lsp-bridge-ui-menu-index -1)
(defvar-local lsp-bridge-ui-menu-offset 0)

(defvar lsp-bridge-ui-icon-width 4)

(defface lsp-bridge-ui-default-face
  '((t (:height 140)))
  "Face for content area.")

(defface lsp-bridge-ui-select-face
  '((((class color) (min-colors 88) (background dark))
     :background "#00415e" :foreground "white")
    (((class color) (min-colors 88) (background light))
     :background "#c0efff" :foreground "black")
    (t :background "blue" :foreground "white"))
  "Face used to highlight the currently selected candidate.")

(defface lsp-bridge-ui-deprecated-face
  '((t :inherit shadow :strike-through t))
  "Face used for deprecated candidates.")

(defvar lsp-bridge-ui-frame nil)

(defvar lsp-bridge-ui-buffer " *lsp-bridge-ui-buffer*")

(defsubst lsp-bridge-ui-indent-pixel (xpos)
  "Return a display property that aligns to XPOS."
  `(space :align-to (,xpos)))

(defvar lsp-bridge-ui-icon-alist
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

(defun lsp-bridge-ui-make-frame (frame-name)
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

(defun lsp-bridge-ui-frame-background-color ()
  (let ((theme-mode (format "%s" (frame-parameter nil 'background-mode))))
    (if (string-equal theme-mode "dark") "#191a1b" "#f0f0f0")))

(defmacro lsp-bridge-ui-save-frame (&rest body)
  `(let* ((current-frame ,(selected-frame))
          (current-buffer ,(current-buffer)))
     ,@body

     (select-frame current-frame)
     (switch-to-buffer current-buffer)
     ))

(defun lsp-bridge-ui-popup ()
  (interactive)
  (lsp-bridge-ui-save-frame
   (lsp-bridge-ui-mode 1)

   (if (and lsp-bridge-ui-frame
            (frame-live-p lsp-bridge-ui-frame))
       (make-frame-visible lsp-bridge-ui-frame)

     (setq lsp-bridge-ui-frame (lsp-bridge-ui-make-frame "lsp-bridge-ui frame"))
     (set-frame-parameter lsp-bridge-ui-frame 'background-color (lsp-bridge-ui-frame-background-color))

     (lsp-bridge-ui-menu-render t)

     (make-frame-visible lsp-bridge-ui-frame)
     )))

(defun lsp-bridge-ui-menu-render (adjust-size)
  (let* ((items lsp-bridge-ui-menu-candidates)
         (item-max-length 0)
         (item-index 0)
         (menu-index lsp-bridge-ui-menu-index))
    (with-current-buffer (get-buffer-create lsp-bridge-ui-buffer)
      (dolist (var lsp-bridge-buffer-parameters)
        (set (make-local-variable (car var)) (cdr var)))

      (erase-buffer)

      (buffer-face-set 'lsp-bridge-ui-default-face)

      (dolist (v items)
        (let* ((candidate (plist-get v :candidate))
               (annotation (plist-get v :annotation))
               (item-length (string-width (format "%s %s" candidate annotation))))
          (when (> item-length item-max-length)
            (setq item-max-length item-length))
          ))

      (dolist (v items)
        (let* ((icon (cdr (assq (plist-get v :icon) lsp-bridge-ui-icon-alist)))
               (candidate (plist-get v :candidate))
               (annotation (plist-get v :annotation))
               (annotation-text (if annotation annotation ""))
               (item-length (string-width annotation-text))
               icon-text
               candidate-line)

          (setq icon-text (lsp-bridge-ui-icon-build (nth 0 icon) (nth 1 icon) (nth 2 icon)))

          (when (plist-get v :deprecated)
            (add-face-text-property 0 (length candidate) 'lsp-bridge-ui-deprecated-face 'append candidate))

          (setq candidate-line
                (concat
                 icon-text
                 candidate
                 (propertize " " 'display (lsp-bridge-ui-indent-pixel (ceiling (* (window-font-width) (- (* item-max-length 1.5) item-length)))))
                 (propertize (format "%s \n" annotation-text) 'face 'font-lock-doc-face)
                 ))

          (when (equal item-index menu-index)
            (add-face-text-property 0 (length candidate-line) 'lsp-bridge-ui-select-face 'append candidate-line))

          (insert candidate-line)

          (setq item-index (1+ item-index))))

      (when (> (length items) 0)
        (delete-backward-char 1)))

    (with-selected-frame lsp-bridge-ui-frame
      (switch-to-buffer lsp-bridge-ui-buffer))

    (when adjust-size
      (fit-frame-to-buffer-1 lsp-bridge-ui-frame nil nil nil nil nil nil nil))

    (let* ((emacs-width (frame-pixel-width))
           (emacs-height (frame-pixel-height))
           (completion-frame-width (frame-pixel-width lsp-bridge-ui-frame))
           (completion-frame-height (frame-pixel-height lsp-bridge-ui-frame))
           (cursor-pos (window-absolute-pixel-position))
           (cursor-x (car cursor-pos))
           (cursor-y (cdr cursor-pos))
           (offset-x (* (window-font-width) lsp-bridge-ui-icon-width))
           (offset-y (line-pixel-height))
           (completion-x (if (> (+ cursor-x completion-frame-width) emacs-width)
                             (- cursor-x completion-frame-width)
                           (- cursor-x offset-x)))
           (completion-y (if (> (+ cursor-y completion-frame-height) emacs-height)
                             (- cursor-y completion-frame-height)
                           (+ cursor-y offset-y))))
      (set-frame-position lsp-bridge-ui-frame completion-x completion-y))
    ))

(defun lsp-bridge-ui-icon-build (collection name fg-color)
  (let* ((icon-key (format "%s_%s" collection name))
         (icon-cache (gethash icon-key lsp-bridge-ui-icon-cache))
         icon-text)
    (if icon-cache
        icon-cache
      (setq icon-text (propertize
                       (apply #'concat (make-list lsp-bridge-ui-icon-width "-"))
                       'display (lsp-bridge-ui-icon collection name fg-color)))
      (puthash icon-key icon-text lsp-bridge-ui-icon-cache)
      icon-text
      )))

(defun lsp-bridge-ui-hide ()
  (interactive)
  (when lsp-bridge-ui-frame
    (lsp-bridge-ui-mode -1)

    (make-frame-invisible lsp-bridge-ui-frame)
    (delete-frame lsp-bridge-ui-frame)
    (kill-buffer lsp-bridge-ui-buffer)
    (setq lsp-bridge-ui-frame nil)))

(defun lsp-bridge-ui-update-completion-data (backend-name completion-table)
  ;; Update completion table that match backend-name.
  (puthash backend-name completion-table lsp-bridge-ui-backend-local-items))

(defun lsp-bridge-ui-search-items (keyboard)
  (setq-local lsp-bridge-ui-filter-candidates (list))

  (dolist (backend-hash-table (list lsp-bridge-ui-backend-global-items
                                    lsp-bridge-ui-backend-local-items))
    (when (and backend-hash-table
               (hash-table-p backend-hash-table))
      (dolist (backend-name (hash-table-keys backend-hash-table))
        (maphash
         (lambda (k v)
           (when (string-match-p (lsp-bridge-ui-build-fuzzy-regex keyboard) (plist-get v :candidate))
             (add-to-list 'lsp-bridge-ui-filter-candidates v t)
             ))
         (gethash backend-name backend-hash-table)
         ))))

  (setq-local lsp-bridge-ui-menu-candidates
              (cl-subseq lsp-bridge-ui-filter-candidates
                         0 (min (length lsp-bridge-ui-filter-candidates)
                                lsp-bridge-ui-menu-length)))
  (setq-local lsp-bridge-ui-filter-index
              (if (zerop (length lsp-bridge-ui-menu-candidates)) -1 0))
  (setq-local lsp-bridge-ui-menu-index lsp-bridge-ui-filter-index)
  (setq-local lsp-bridge-ui-menu-offset 0))

(defun lsp-bridge-ui-menu-update-candidates ()
  (let ((menu-length (length lsp-bridge-ui-menu-candidates)))
    (when (> (length lsp-bridge-ui-filter-candidates) menu-length)
      (setq-local lsp-bridge-ui-menu-candidates
                  (cl-subseq lsp-bridge-ui-filter-candidates
                             lsp-bridge-ui-menu-offset
                             (+ lsp-bridge-ui-menu-offset menu-length))))))

(defmacro lsp-bridge-ui-menu-update (&rest body)
  `(let* ((menu-index lsp-bridge-ui-menu-index)
          (menu-offset lsp-bridge-ui-menu-offset))
     ,@body

     (when (or (not (equal menu-index lsp-bridge-ui-menu-index))
               (not (equal menu-offset lsp-bridge-ui-menu-offset)))
       (lsp-bridge-ui-save-frame
        (lsp-bridge-ui-menu-update-candidates)
        (lsp-bridge-ui-menu-render (not (equal menu-offset lsp-bridge-ui-menu-offset)))))))

(defun lsp-bridge-ui-select-first ()
  (interactive)
  (lsp-bridge-ui-menu-update
   (setq-local lsp-bridge-ui-menu-offset 0)
   (setq-local lsp-bridge-ui-menu-index 0)))

(defun lsp-bridge-ui-select-last ()
  (interactive)
  (lsp-bridge-ui-menu-update
   (let ((menu-length (length lsp-bridge-ui-menu-candidates)))
     (setq-local lsp-bridge-ui-menu-offset (- (length lsp-bridge-ui-filter-candidates) menu-length))
     (setq-local lsp-bridge-ui-menu-index (- menu-length 1))
     )))

(defun lsp-bridge-ui-select-next ()
  (interactive)
  (lsp-bridge-ui-menu-update
   (cond ((< lsp-bridge-ui-menu-index (1- (length lsp-bridge-ui-menu-candidates)))
          (setq-local lsp-bridge-ui-menu-index (1+ lsp-bridge-ui-menu-index)))
         ((< (+ lsp-bridge-ui-menu-offset lsp-bridge-ui-menu-index) (1- (length lsp-bridge-ui-filter-candidates)))
          (setq-local lsp-bridge-ui-menu-offset (1+ lsp-bridge-ui-menu-offset))))))

(defun lsp-bridge-ui-select-prev ()
  (interactive)
  (lsp-bridge-ui-menu-update
   (cond ((> lsp-bridge-ui-menu-index 0)
          (setq-local lsp-bridge-ui-menu-index (1- lsp-bridge-ui-menu-index)))
         ((> lsp-bridge-ui-menu-offset 0)
          (setq-local lsp-bridge-ui-menu-offset (1- lsp-bridge-ui-menu-offset))))))

(defun lsp-bridge-ui-build-fuzzy-regex (input)
  "Create a fuzzy regexp of PATTERN."
  (mapconcat (lambda (ch)
               (let ((s (char-to-string ch)))
                 (format "[^%s]*%s" s (regexp-quote s))))
             input ""))

(defun lsp-bridge-ui-test ()
  (interactive)
  (unless lsp-bridge-ui-backend-local-items
    (setq-local lsp-bridge-ui-backend-local-items (make-hash-table :test 'equal)))

  (let* ((completion-table (make-hash-table :test 'equal))
         items)
    (setq items '(
                  (:key "1" :icon unknown :candidate "expanduser" :doc "Doc for expanduser.")
                  (:key "2" :icon text :candidate "expanduser" :annotation "Snippet" :doc "Doc for expanduser.")
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

    (lsp-bridge-ui-update-completion-data "lsp-bridge" completion-table)

    (lsp-bridge-ui-search-items "e")

    (lsp-bridge-ui-popup)))

(provide 'lsp-bridge-ui)

;;; lsp-bridge-ui.el ends here
