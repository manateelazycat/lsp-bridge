;;; acm-icon.el --- Icon for acm

;; Filename: acm-icon.el
;; Description: Icon for acm
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-06-07 21:41:25
;; Version: 0.1
;; Last-Updated: 2022-06-07 21:41:25
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/acm-icon
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
;; Icon for acm
;;

;;; Installation:
;;
;; Put acm-icon.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'acm-icon)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET acm-icon RET
;;

;;; Change log:
;;
;; 2022/06/07
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


;;; Code:

(defvar acm-icon-collections
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
    ("search" . ("material" "feature-search" "#7ac143"))
    ("ctor" . ("material" "cube" "#b84592"))
    ("field" . ("material" "tag" "#ff6c5f"))
    ("variable" . ("material" "variable" "#00b2a9"))
    ("var" . ("material" "application-variable" "#e04646"))
    ("class" . ("material" "alpha-c-circle-outline" "#ef5734"))
    ("interface" . ("material" "share" "#6cbc35"))
    ("i/f" . ("material" "share" "#ee3322"))
    ("module" . ("material" "sim-outline" "#00c4cc"))
    ("mod" . ("material" "view-module" "#ff6908"))
    ("property" . ("material" "wrench" "#bf033b"))
    ("prop" . ("material" "tools" "#0eb24e"))
    ("unit" . ("material" "alpha-u-box-outline" "#98c807"))
    ("value" . ("material" "format-align-right" "#ff0092"))
    ("enum" . ("material" "database" "#dc5034"))
    ("keyword" . ("material" "filter" "#0085c3"))
    ("k/w" . ("material" "filter-outline" "#ed6856"))
    ("snippet" . ("material" "format-align-center" "#f05d21"))
    ("yas-snippet" . ("material" "format-align-center" "#f05d21"))
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
    ("translate" . ("material" "translate" "#98c807"))
    ("emmet abbreviation" . ("material" "expand-all-outline" "#98c807"))
    ("custom" . ("material" "apple-keyboard-option" "#ed6856"))
    ("special form" . ("material" "function-variant" "#0abf53"))
    ("feature" . ("material" "sim-outline" "#00c4cc"))
    ("at" . ("material" "at" "#0085c3"))
    ("tabnine" . ("material" "alpha-t-box-outline" "#954a97"))
    ("port" . ("material" "blinds-open" "#39a6dd"))
    ("net" . ("material" "chart-timeline-variant" "#ed6856"))
    ("register" . ("material" "cellphone-key" "#ff6908"))
    ("block" . ("material" "contain" "#96cbb3"))
    ("prototype" . ("material" "currency-sign" "#0085c3"))
    ("instance" . ("material" "city-variant-outline" "#da1884"))
    ("task" . ("material" "text-recognition" "#e22272"))
    ("typedef" . ("material" "label-outline" "#bf11b6"))
    (t . ("material" "file-find-outline" "#90cef1"))))

(defvar acm-icon-cache (make-hash-table :test 'equal))
(defvar acm-icon-dir (expand-file-name "icons" (file-name-directory load-file-name)))
(defvar acm-icon-width 4)

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

(defun acm-icon-convert-to-svg-color (color-name)
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
         (fg-color (acm-icon-convert-to-svg-color
                    (or (when (facep fg-color)
                          (face-foreground fg-color nil t))
                        fg-color (face-attribute 'default :foreground))))
         (bg-color (acm-icon-convert-to-svg-color
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

(defun acm-icon-build (collection name fg-color)
  (if (and acm-enable-icon
           (image-type-available-p 'svg))
      (let* ((icon-key (format "%s_%s" collection name))
             (icon-text (gethash icon-key acm-icon-cache)))
        (unless icon-text
          (setq icon-text (propertize
                           (apply #'concat (make-list acm-icon-width "-"))
                           'display (acm-icon collection name fg-color)))
          (puthash icon-key icon-text acm-icon-cache))
        icon-text)
    ""))

(provide 'acm-icon)

;;; acm-icon.el ends here
