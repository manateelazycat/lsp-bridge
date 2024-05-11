;;; acm-icon.el --- Icon for acm  -*- lexical-binding: t -*-

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

;; Search icon from https://pictogrammers.com/library/mdi/icon/
(defvar acm-icon-alist
  `(("unknown" . ("material" "emoticon-lol-outline" "#74d2e7"))
    ("text" . ("material" "format-text" "#98c807"))
    ("method" . ("material" "cube" "#da1884"))
    ("function" . ("material" "lambda" "#ff6a00"))
    ("fun" . ("material" "lambda" "#0abf53"))
    ("constructor" . ("material" "all-inclusive" "#7ac143"))
    ("search" . ("material" "magnify" "#7ac143"))
    ("ctor" . ("material" "cube" "#b84592"))
    ("field" . ("material" "pound" "#ff6c5f"))
    ("variable" . ("material" "angle-acute" "#00b2a9"))
    ("var" . ("material" "angle-acute" "#e04646"))
    ("class" . ("material" "abugida-devanagari" "#ef5734"))
    ("interface" . ("material" "abjad-hebrew" "#6cbc35"))
    ("i/f" . ("material" "abjad-hebrew" "#ee3322"))
    ("module" . ("material" "alpha-m-box-outline" "#00c4cc"))
    ("mod" . ("material" "alpha-m-box-outline" "#ff6908"))
    ("property" . ("material" "alpha-p-box-outline" "#bf033b"))
    ("prop" . ("material" "alpha-p-box-outline" "#0eb24e"))
    ("unit" . ("material" "alpha-u-box-outline" "#98c807"))
    ("value" . ("material" "alpha-v-box-outline" "#ff0092"))
    ("enum" . ("material" "alpha-e-box-outline" "#dc5034"))
    ("keyword" . ("material" "alpha-k-box-outline" "#0085c3"))
    ("k/w" . ("material" "alpha-k-box-outline" "#ed6856"))
    ("key" . ("material" "alpha-k-box-outline" "#0085c3"))
    ("snippet" . ("material" "dna" "#f05d21"))
    ("yas-snippet" . ("material" "dna" "#f05d21"))
    ("template" . ("material" "dna" "#207c88"))
    ("sn" . ("material" "dna" "#f69653"))
    ("color" . ("material" "format-color-highlight" "#099d84"))
    ("face" . ("material" "format-color-highlight" "#98c807"))
    ("file" . ("material" "paperclip" "#e30061"))
    ("reference" . ("material" "link-variant" "#954a97"))
    ("ref" . ("material" "link-variant" "#006e96"))
    ("folder" . ("material" "tree" "#f56040"))
    ("dir" . ("material" "tree" "#d25238"))
    ("enum-member" . ("material" "google-circles-extended" "#ff9900"))
    ("enummember" . ("material" "google-circles-extended" "#8a8acb"))
    ("member" . ("material" "guitar-pick" "#e55e5e"))
    ("constant" . ("material" "pi" "#d1de3f"))
    ("const" . ("material" "pi" "#f65314"))
    ("struct" . ("material" "abugida-thai" "#96cbb3"))
    ("event" . ("material" "post-lamp" "#e990ab"))
    ("operator" . ("material" "plus-minus-variant" "#f47b7b"))
    ("op" . ("material" "plus-minus-variant" "#eb0973"))
    ("type-parameter" . ("material" "comma" "#39a6dd"))
    ("typeparameter" . ("material" "comma" "#dc5034"))
    ("param" . ("material" "comma" "#ff0e83"))
    ("macro" . ("material" "bat" "#ff9900"))
    ("translate" . ("material" "translate" "#98c807"))
    ("emmet abbreviation" . ("material" "arrow-expand" "#98c807"))
    ("custom" . ("material" "unicorn-variant" "#ed6856"))
    ("special form" . ("material" "sawtooth-wave" "#0abf53"))
    ("feature" . ("material" "feather" "#00c4cc"))
    ("at" . ("material" "at" "#0085c3"))
    ("port" . ("material" "alphabet-tengwar" "#39a6dd"))
    ("net" . ("material" "ethereum" "#ed6856"))
    ("register" . ("material" "map-marker-outline" "#ff6908"))
    ("block" . ("material" "alpha-b-box-outline" "#96cbb3"))
    ("prototype" . ("material" "source-commit-start" "#0085c3"))
    ("instance" . ("material" "gamepad-circle-left" "#da1884"))
    ("task" . ("material" "clock-fast" "#e22272"))
    ("typedef" . ("material" "zodiac-leo" "#bf11b6"))
    ("tailwind" . ("material" "creation" "#39a6dd"))
    ("namespace" . ("material" "alpha-n-box-outline" "#dc5034"))
    ("package" . ("material" "poker-chip" "#dc5034"))
    ("string" . ("material" "alphabet-greek" "#98c807"))
    ("number" . ("material" "abjad-arabic" "#98c807"))
    ("boolean" . ("material" "gate-or" "#98c807"))
    ("array" . ("material" "sigma" "#0085c3"))
    ("object" . ("material" "zodiac-taurus" "#0085c3"))
    ("null" . ("material" "spider" "##98c807"))
    ("tabnine" . ("material" "alpha-t-box-outline" "#954a97"))
    ("codeium" . ("material" "alpha-c-box-outline" "#09b6a2"))
    ("note" . ("material" "note" "#77aa99"))
    ("copilot" . ("octicons" "copilot" "#808080"))
    (t . ("material" "smoking-pipe" "#90cef1"))))

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

(defun acm-icon (collection name fg-color)
  (let* ((root (acm-icon-parse collection name))

         ;; Read original viewbox
         (viewbox (cdr (assq 'viewBox (xml-node-attributes (car root)))))
         (viewbox (mapcar 'string-to-number (split-string viewbox)))
         (view-x (nth 0 viewbox))
         (view-y (nth 1 viewbox))
         (view-width (nth 2 viewbox))
         (view-height (nth 3 viewbox))

         ;; Set icon size (in pixels) to 4x1 characters
         (svg-width  (* (frame-char-width)  acm-icon-width))
         (svg-height (* (frame-char-height) 1))

         (svg-viewbox (format "%f %f %f %f" view-x view-y view-width view-height))
         (fg-color (acm-icon-convert-to-svg-color
                    (or (when (facep fg-color)
                          (face-foreground fg-color nil t))
                        fg-color (face-attribute 'default :foreground))))
         (svg (svg-create svg-width svg-height
                          :viewBox svg-viewbox
                          :stroke-width 0
                          :fill fg-color)))

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
