;;; acm-backend-search-sdcv-words.el --- Path backend for acm  -*- lexical-binding: t -*-

;; Filename: acm-backend-search-sdcv-words.el
;; Description: Path backend for acm
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-06-07 08:58:47
;; Version: 0.1
;; Last-Updated: 2022-06-07 08:58:47
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/acm-backend-path
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
;; Path backend for acm
;;

;;; Installation:
;;
;; Put acm-backend-search-sdcv-words.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'acm-backend-search-sdcv-words)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET acm-backend-search-sdcv-words RET
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

(defgroup acm-backend-search-sdcv-words nil
  "SDCV backend for acm."
  :group 'acm)

(defcustom acm-backend-search-sdcv-words-candidates-max-number 100
  "Maximal number of candidate of menu."
  :type 'integer
  :group 'acm-backend-search-sdcv-words)

(defcustom acm-backend-search-sdcv-words-dictionary "kdic-ec-11w"
  "SDCV dictionary for word completion.
Default is `kdic-ec-11w', you can replace it with StarDict dictionary path

Example, if you have dictionary `/usr/share/stardict/dic/stardict-oxford-gb-formated-2.4.2/oxford-gb-formated.ifo',
you need set this value to `/usr/share/stardict/dic/stardict-oxford-gb-formated-2.4.2/oxford-gb-formated', not include `.ifo' extension."
  :type 'string
  :group 'acm-backend-search-sdcv-words)

(defvar-local acm-enable-search-sdcv-words nil)
(defvar-local acm-backend-search-sdcv-words-items nil)

(defun acm-backend-search-sdcv-words-candidates (keyword)
  acm-backend-search-sdcv-words-items)

(defun acm-backend-search-sdcv-words-candidate-expand (candidate-info bound-start)
  (delete-region (beginning-of-thing 'word) (end-of-thing 'word))
  (insert (plist-get candidate-info :display-label)))

(defun acm-backend-search-sdcv-words-clean ()
  (setq-local acm-backend-search-sdcv-words-items nil))

(provide 'acm-backend-search-sdcv-words)

;;; acm-backend-search-sdcv-words.el ends here
