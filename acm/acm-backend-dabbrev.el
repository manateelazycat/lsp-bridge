;;; acm-backend-dabbrev.el --- Dabbrev backend for acm

;; Filename: acm-backend-dabbrev.el
;; Description: Dabbrev backend for acm
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-06-07 09:30:43
;; Version: 0.1
;; Last-Updated: 2022-06-07 09:30:43
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/acm-backend-dabbrev
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
;; Dabbrev backend for acm
;;

;;; Installation:
;;
;; Put acm-backend-dabbrev.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'acm-backend-dabbrev)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET acm-backend-dabbrev RET
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

(defcustom acm-enable-dabbrev nil
  "Popup dabbrev completions when this option is turn on."
  :type 'boolean)

(defcustom acm-backend-dabbrev-min-length 4
  "Minimum length of dabbrev expansions.
This setting ensures that words which are too short
are not offered as completion candidates, such that
auto completion does not pop up too aggressively."
  :type 'integer)

(defvar acm-backend-dabbrev-completion-tick nil)

(defun acm-backend-dabbrev-candidates-append ()
  (when (and acm-enable-dabbrev
             (not (equal acm-backend-dabbrev-completion-tick (acm-backend-dabbrev-auto-tick))))
    (let* ((keyword (acm-get-input-prefix))
           (candidates (list))
           (dabbrev-words (acm-backend-dabbrev-get-words keyword))
           (menu-old-cache (cons acm-menu-max-length-cache acm-menu-number-cache)))
      (when (>= (length keyword) acm-backend-dabbrev-min-length)
        (dolist (dabbrev-word (cl-subseq dabbrev-words 0 (min (length dabbrev-words) 10)))
          (add-to-list 'candidates (list :key dabbrev-word
                                         :icon "text"
                                         :label dabbrev-word
                                         :display-label dabbrev-word
                                         :annotation "Dabbrev"
                                         :backend "dabbrev") t)))

      (when (> (length candidates) 0)
        (setq-local acm-candidates (append acm-candidates candidates))
        (when (< (length acm-menu-candidates) acm-menu-length)
          (setq-local acm-menu-candidates
                      (append acm-menu-candidates
                              (cl-subseq candidates 0
                                         (min (- acm-menu-length (length acm-menu-candidates))
                                              (length candidates))))))

        (acm-menu-render menu-old-cache))

      (setq acm-backend-dabbrev-completion-tick (acm-backend-dabbrev-auto-tick)))))

(defun acm-backend-dabbrev-get-words (word)
  "Find all dabbrev expansions for WORD."
  (require 'dabbrev)
  (acm-silent
    (let ((dabbrev-check-other-buffers nil)
          (dabbrev-check-all-buffers nil))
      (dabbrev--reset-global-variables))
    (cl-loop with min-len = (+ acm-backend-dabbrev-min-length (length word))
             for w in (dabbrev--find-all-expansions word (dabbrev--ignore-case-p word))
             if (>= (length w) min-len) collect w)))

(defun acm-backend-dabbrev-auto-tick ()
  (list (current-buffer) (buffer-chars-modified-tick) (point)))

(provide 'acm-backend-dabbrev)

;;; acm-backend-dabbrev.el ends here
