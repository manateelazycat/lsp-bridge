;;; lsp-bridge-peek.el --- Show peek windows for lsp-bridge  -*- lexical-binding: t -*-

;; Filename: lsp-bridge-peek.el
;; Description: Show definitions and references in peek window
;; Author: AllTheLife <xjn208930@gmail.com>
;; Maintainer: AllTheLife <xjn208930@gmail.com>
;; Copyright (C) 2023, AllTheLife
;; Created: 2023-7-1 19:28 +0800
;; Version: 0.1
;; Last-Updated: 2023-7-15 20:23:54 +0800
;;           By: AllTheLife
;; URL: https://github.com/manateelazycat/lsp-bridge
;; Keywords:
;; Compatibility: emacs-version >= 28
;; Package-Requires: ((emacs "28"))
;;
;; Features that might be required by this library:
;;
;; Please check README
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
;;
;;

;;; Installation:
;;
;;
;;

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET lsp-bridge-peek RET
;;

;;; Change log:
;;
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require:

(require 'color)

;;; Code:

(defgroup lsp-bridge-peek nil
  "Perfect code reading experience in `lsp-bridge' with peek feature."
  :group 'lsp-bridge)

(defvar lsp-bridge-peek--ov nil
  "Overlay used to display the `lsp-bridge-peek' UI.")

(defvar lsp-bridge-peek--bg nil
  "Background color used for file contents when peeking.")

(defvar lsp-bridge-peek--bg-alt nil
  "Background color for unselected tags when peeking.")

(defvar lsp-bridge-peek--bg-selected nil
  "Background color for selected tags when peeking.")

(defvar lsp-bridge-peek--method-fg nil
  "Foreground color for methods when peeking.")

(defvar lsp-bridge-peek--path-fg nil
  "Foreground color for file paths when peeking.")

(defvar lsp-bridge-peek--pos-fg nil
  "Foreground color for postions when peeking.")

(defvar lsp-bridge-peek--symbol-selected nil
  "Foreground color for symbols are selected when peeking.")

(defvar lsp-bridge-peek--symbol-alt nil
  "Foreground color for alt symbols when peeking.")

(defvar lsp-bridge-peek--ace-seqs nil
  "Ace key sequences for ace jump.")

(defvar lsp-bridge-peek--symbol-bounds nil
  "Symbol bounds for ace jump.
Its car is the bound offset, i.e., the starting point of the
region to perform ace jump on.  Its cdr is a list of the symbol
bounds as returned by `lsp-bridge--search-symbols'.")

(defvar lsp-bridge-peek--content-update nil
  "Non-nil means the content in the peek window is updated.")

(defvar lsp-bridge-peek--temp-buffer-alist nil
  "Files and their temporary buffers that don't exist before peeking.
Its keys are file paths, values are buffers.  The buffers will be
killed after disabling `lsp-bridge-peek--mode'.")

(defvar lsp-bridge-peek-chosen-displaying-list '(nil nil nil)
  "Information saved to display the definition or reference.
The first item in the list is the first definition or reference displayed,
the second item is the definition or reference that will be selected, and
the third item is the last definition or reference displayed.")

(defvar lsp-bridge-peek-file-and-pos-before-jump nil)

(defvar lsp-bridge-peek-symbol-tree nil
  "A tree structure for storing symbols, each element is a list. The first
item of the list is the name of the symbol, the second item stores the paths
to files where definitions and references are, the third item stores the
positions where definitions and references are in the file, the fourth item
is the index of the parent node of the symbol in the list, and the fifth
item is a list, storing the index of the node's child nodes in the list.
The sixth item stores which child node is selected next. The seventh item
stores how many lines the file content was moved.")

(defvar lsp-bridge-peek-symbol-at-point nil
  "A variable that stores the current symbol to be added.")

(defvar lsp-bridge-peek-selected-symbol nil
  "A variable that stores which symbol is currently selected.")

(defvar lsp-bridge-peek-ace-list nil
  "A list that stores the related information used to recovery the status before ace peek.
The first is the buffer which need killing. The second is the position before ace peek.
The third is the buffer before ace peek. The fourth is the buffer where the symbol is.
The fifth is the position where the symbol is.")

(defface lsp-bridge-peek--highlight-symbol-face
  `((t :foreground "white" :background "#623d73"))
  "Face for highlighting the symbol you want to look through."
  :group 'lsp-bridge-peek)

(defface lsp-bridge-peek-border-face
  `((t :height 15 :background ,(face-attribute 'default :foreground) :extend t))
  "Face used for borders of peek windows.
You can customize the appearance of the borders by setting the
height and background properties of the face."
  :group 'lsp-bridge-peek)

(defface lsp-bridge-peek-ace-str-face
  '((((background light))
     :foreground "#dddddd" :background "#666666")
    (t
     :foreground "#222222" :background "#c0c0c0"))
  "Face used for ace strings."
  :group 'lsp-bridge-peek)

(defcustom lsp-bridge-peek-file-content-height 12
  "Number of lines displaying file contents in the peek window."
  :type 'integer
  :group 'lsp-bridge-peek)


(defcustom lsp-bridge-peek-file-content-scroll-margin 1
  "Set how much lsp-bridge-peek-file-content-next-line/-prev-line should scroll up and down."
  :type 'integer
  :group 'lsp-bridge-peek)

(defcustom lsp-bridge-peek-list-height 3
  "Number of definitions/references displayed in the peek window."
  :type 'integer
  :group 'lsp-bridge-peek)

(defcustom lsp-bridge-peek-ace-keys '(?a ?s ?d ?f ?j ?k ?l ?\;)
  "Ace keys used for `lsp-bridge-peek-through'."
  :type '(repeat :tag "Keys" character)
  :group 'lsp-bridge-peek)

(defcustom lsp-bridge-peek-ace-cancel-keys '(?\C-g ?q)
  "Keys used for cancel an ace session."
  :type '(repeat :tag "Keys" character)
  :group 'lsp-bridge-peek)

(defcustom lsp-bridge-peek-keymap
  (let ((map (make-sparse-keymap)))
    ;; Browse file
    (define-key map (kbd "M-n") 'lsp-bridge-peek-file-content-next-line)
    (define-key map (kbd "M-p") 'lsp-bridge-peek-file-content-prev-line)

    (define-key map (kbd "n") 'lsp-bridge-peek-file-content-next-line)
    (define-key map (kbd "p") 'lsp-bridge-peek-file-content-prev-line)

    ;; Browse in the definition/reference list
    (define-key map (kbd "M-N") 'lsp-bridge-peek-list-next-line)
    (define-key map (kbd "M-P") 'lsp-bridge-peek-list-prev-line)

    (define-key map (kbd "N") 'lsp-bridge-peek-list-next-line)
    (define-key map (kbd "P") 'lsp-bridge-peek-list-prev-line)

    ;; Browse in the tree history
    (define-key map (kbd "<right>") 'lsp-bridge-peek-tree-next-node)
    (define-key map (kbd "<left>") 'lsp-bridge-peek-tree-previous-node)
    (define-key map (kbd "<up>") 'lsp-bridge-peek-tree-previous-branch)
    (define-key map (kbd "<down>") 'lsp-bridge-peek-tree-next-branch)

    (define-key map (kbd "l") 'lsp-bridge-peek-tree-next-node)
    (define-key map (kbd "h") 'lsp-bridge-peek-tree-previous-node)
    (define-key map (kbd "k") 'lsp-bridge-peek-tree-previous-branch)
    (define-key map (kbd "j") 'lsp-bridge-peek-tree-next-branch)

    ;; Jump
    (define-key map (kbd "M-l j") 'lsp-bridge-peek-jump)
    (define-key map (kbd "M-l b") 'lsp-bridge-peek-jump-back)

    (define-key map (kbd "8") 'lsp-bridge-peek-jump)
    (define-key map (kbd "7") 'lsp-bridge-peek-jump-back)

    ;; Abort
    (define-key map [remap keyboard-quit] 'lsp-bridge-peek-abort)
    map)
  "Keymap used for `lsp-bridge-peek' sessions."
  :type 'keymap
  :group 'lsp-bridge-peek)

(define-minor-mode lsp-bridge-peek-mode
  "Mode for `lsp-bridge-peek'.
This mode is created merely fo handling the UI (display, keymap,
etc.), and is not for interactive use. Users should use commands
like `lsp-bridge-peek', `lsp-bridge-peek-abort', `lsp-bridge-peek-restore',
which take care of setting up other things."
  :keymap lsp-bridge-peek-keymap
  (cond
   (lsp-bridge-peek-mode
    (when lsp-bridge-peek--ov (delete-overlay lsp-bridge-peek--ov))
    (setq lsp-bridge-peek--ov
	      (let ((ov-pos (line-end-position)))
	        (make-overlay ov-pos ov-pos)))
    (overlay-put lsp-bridge-peek--ov 'window (selected-window))
    (let* ((bg-mode (frame-parameter nil 'background-mode))
	       (bg-unspecified-p (string= (face-background 'default)
                                      "unspecified-bg"))
	       (bg (cond
		        ((and bg-unspecified-p (eq bg-mode 'dark)) "#333333")
		        ((and bg-unspecified-p (eq bg-mode 'light)) "#dddddd")
		        (t (face-background 'default)))))
      (cond
       ((eq bg-mode 'dark)
	    (setq lsp-bridge-peek--bg (lsp-bridge--color-blend "#ffffff" bg 0.03)
	          lsp-bridge-peek--bg-alt (lsp-bridge--color-blend "#ffffff" bg 0.2)
	          lsp-bridge-peek--bg-selected (lsp-bridge--color-blend "#ffffff" bg 0.4)
	          lsp-bridge-peek--method-fg "gray"
	          lsp-bridge-peek--path-fg "#6aaf50"
	          lsp-bridge-peek--pos-fg "#f57e00"
	          lsp-bridge-peek--symbol-selected "orange"
	          lsp-bridge-peek--symbol-alt "#dedede"))
       (t
	    (setq lsp-bridge-peek--bg (lsp-bridge--color-blend "#000000" bg 0.02)
	          lsp-bridge-peek--bg-alt (lsp-bridge--color-blend "#000000" bg 0.12)
	          lsp-bridge-peek--bg-selected (lsp-bridge--color-blend "#000000" bg 0.06)
	          lsp-bridge-peek--method-fg "dark"
	          lsp-bridge-peek--path-fg "#477a33"
	          lsp-bridge-peek--pos-fg "#b06515"
	          lsp-bridge-peek--symbol-selected "#ff6200"
	          lsp-bridge-peek--symbol-alt "#212121"))))
    (setq lsp-bridge-peek--content-update t)
    (setq lsp-bridge-peek-chosen-displaying-list (make-list 3 0))
    (setf (nth 2 lsp-bridge-peek-chosen-displaying-list) (1- lsp-bridge-peek-list-height))
    (add-hook 'post-command-hook #'lsp-bridge-peek--show nil 'local))
   (t
    (when lsp-bridge-peek--ov (delete-overlay lsp-bridge-peek--ov))
    (mapc (lambda (pair)
	        (kill-buffer pair))
	      lsp-bridge-peek--temp-buffer-alist)
    (setq lsp-bridge-peek--temp-buffer-alist nil
	      lsp-bridge-peek--ov nil
	      lsp-bridge-peek--bg nil
	      lsp-bridge-peek--bg-alt nil
	      lsp-bridge-peek--bg-selected nil
	      lsp-bridge-peek--method-fg nil
	      lsp-bridge-peek--path-fg nil
	      lsp-bridge-peek--pos-fg nil
	      lsp-bridge-peek--symbol-selected nil
	      lsp-bridge-peek--symbol-alt nil
	      lsp-bridge-peek-ace-list nil
	      lsp-bridge-peek-symbol-tree nil
	      lsp-bridge-peek-selected-symbol nil
	      lsp-bridge-peek-chosen-displaying-list (make-list 3 nil)
	      lsp-bridge-peek-file-and-pos-before-jump nil
	      lsp-bridge-peek--ace-seqs nil
	      lsp-bridge-peek--symbol-bounds nil)
    (remove-hook 'post-command-hook #'lsp-bridge-peek--show 'local))))

;; Ref: https://www.w3.org/TR/WCAG20/#relativeluminancedef
(defun lsp-bridge--color-srgb-to-rgb (c)
  "Convert an sRGB component C to an RGB one."
  (if (<= c 0.03928)
      (/ c 12.92)
    (expt (/ (+ c 0.055) 1.055) 2.4)))

(defun lsp-bridge--color-rgb-to-srgb (c)
  "Convert an RGB component C to an sRGB one."
  (if (<= c (/ 0.03928 12.92))
      (* c 12.92)
    (- (* 1.055 (expt c (/ 1 2.4))) 0.055)))

(defun lsp-bridge--color-blend (c1 c2 alpha)
  "Blend two colors C1 and C2 with ALPHA.
C1 and C2 are hexadecimal strings.  ALPHA is a number between 0.0
and 1.0 which is the influence of C1 on the result.

The blending is done in the sRGB space, which should make ALPHA
feels more linear to human eyes."
  (pcase-let ((`(,r1 ,g1 ,b1)
               (mapcar #'lsp-bridge--color-rgb-to-srgb
                       (color-name-to-rgb c1)))
              (`(,r2 ,g2 ,b2)
               (mapcar #'lsp-bridge--color-rgb-to-srgb
                       (color-name-to-rgb c2)))
              (blend-and-to-rgb
               (lambda (x y)
                 (lsp-bridge--color-srgb-to-rgb
                  (+ (* alpha x)
                     (* (- 1 alpha) y))))))
    (color-rgb-to-hex
     (funcall blend-and-to-rgb r1 r2)
     (funcall blend-and-to-rgb g1 g2)
     (funcall blend-and-to-rgb b1 b2)
     2)))

(defun lsp-bridge--add-face (str face)
  "Add FACE to STR, and return it.
This is mainly for displaying STR in an overlay.  For example, if
FACE specifies background color, then STR will have that
background color, with all other face attributes preserved.

`default' face is appended to make sure the display in overlay is
not affected by its surroundings."
  (let ((len (length str)))
    (add-face-text-property 0 len face nil str)
    (add-face-text-property 0 len 'default 'append str)
    str))

(defun lsp-bridge-peek-abort ()
  "Abort peeking."
  (interactive)
  (lsp-bridge-peek-mode -1))

(defun lsp-bridge-peek--make-border ()
  "Return the border to be used in peek windows."
  (propertize "\n"
	          'face 'lsp-bridge-peek-border-face))

(defun lsp-bridge-peek--get-content (pos content-move)
  (setq file-content nil)
  (goto-char (acm-backend-lsp-position-to-point pos))
  (let* ((beg (save-excursion
		        (forward-line content-move)
		        (line-beginning-position)))
	     (end (save-excursion
		        (forward-line content-move)
		        (forward-line (1- lsp-bridge-peek-file-content-height))
		        (line-end-position)))
	     (highlight-begin (point))
	     (highlight-end (save-excursion
			              (forward-symbol 1)
			              (point))))
    (font-lock-fontify-region beg end)
    (put-text-property highlight-begin highlight-end 'face 'lsp-bridge-peek--highlight-symbol-face)
    (setq file-content (concat (buffer-substring beg end) "\n"))
    (remove-text-properties highlight-begin highlight-end 'face))
  file-content)

(defun lsp-bridge--attach-ace-str (str sym-bounds bound-offset ace-seqs)
  "Return a copy of STR with ace strings attached.
SYM-BOUNDS specifies the symbols in STR, as returned by
`lsp-bridge-peek--search-symbols'.  BOUND-OFFSET is the starting point of
STR in the buffer.  ACE-SEQS is the ace key sequences, as
returned by `lsp-bridge-peek--ace-key-seqs' or `lsp-bridge-peek--pop-ace-key-seqs'.
The beginnings of each symbol are replaced by ace strings with
`lsp-bridge-peek-ace-string-face' attached."
  (let* ((nsyms (length sym-bounds))
         (new-str (copy-sequence str)))
    (dotimes (n nsyms)
      (when (nth n ace-seqs)
        (let* ((beg (- (car (nth n sym-bounds)) bound-offset))
               (end (- (cdr (nth n sym-bounds)) bound-offset))
               (ace-seq (nth n ace-seqs))
               (ace-str-len (min (length ace-seq) (- end beg))))
          (dotimes (idx ace-str-len)
            (aset new-str (+ beg idx) (nth idx ace-seq)))
          (put-text-property beg (+ beg ace-str-len)
                             'face 'lsp-bridge-peek-ace-str-face new-str))))
    new-str))

(defun lsp-bridge-peek--file-content ()
  "Return a string for displaying file content."
  (let* ((n (nth 1 lsp-bridge-peek-chosen-displaying-list))
	     (selected-symbol (nth lsp-bridge-peek-selected-symbol lsp-bridge-peek-symbol-tree))
	     (path-list (nth 1 selected-symbol))
	     (pos-list (nth 2 selected-symbol))
	     (content-move-list (nth 6 selected-symbol))
	     (file (nth n path-list))
	     (pos (nth n pos-list))
	     (content-move (nth n content-move-list))
	     (buf-name (format "*lsp-bridge-peek-%s*" file))
	     (file-content nil))
    (if (not (member buf-name lsp-bridge-peek--temp-buffer-alist))
	    (let ((buf (generate-new-buffer buf-name)))
	      (with-current-buffer buf
	        (insert-file-contents file)
	        (let ((buffer-file-name file))
	          (delay-mode-hooks
		        (set-auto-mode)))
	        (setq file-content (lsp-bridge-peek--get-content pos content-move))
	        (add-to-list 'lsp-bridge-peek--temp-buffer-alist buf-name)))
      (with-current-buffer buf-name
	    (setq file-content (lsp-bridge-peek--get-content pos content-move))))
    (when (and lsp-bridge-peek--symbol-bounds lsp-bridge-peek--ace-seqs)
      (setq file-content
	        (lsp-bridge--attach-ace-str file-content
					                    (cdr lsp-bridge-peek--symbol-bounds)
					                    (car lsp-bridge-peek--symbol-bounds)
					                    lsp-bridge-peek--ace-seqs)))
    file-content))

(defun lsp-bridge-peek--displaying-list ()
  "Return a string for displayed definition/reference list."
  (let* ((selected-symbol (nth lsp-bridge-peek-selected-symbol lsp-bridge-peek-symbol-tree))
	     (path-list (nth 1 selected-symbol))
	     (pos-list (nth 2 selected-symbol))
	     (total-n (min lsp-bridge-peek-list-height (length path-list)))
	     (first-displayed-id (nth 0 lsp-bridge-peek-chosen-displaying-list))
	     (selected-id (nth 1 lsp-bridge-peek-chosen-displaying-list))
	     (last-displayed-id (nth 2 lsp-bridge-peek-chosen-displaying-list))
	     (displaying-list (make-list total-n nil)))
    (dotimes (n total-n)
      (let* ((filename (nth (+ first-displayed-id n) path-list))
	         (pos (nth (+ first-displayed-id n) pos-list))
	         (line (number-to-string (1+ (plist-get pos :line))))
	         (char (number-to-string (1+ (plist-get pos :character))))
	         (method (if (= (+ first-displayed-id n) 0) "definition" "reference"))
	         (bg-selected (list :background lsp-bridge-peek--bg-selected :extend t))
	         (bg-alt (list :background lsp-bridge-peek--bg-alt :extend t)))
	    (lsp-bridge--add-face method (list :foreground  lsp-bridge-peek--method-fg
					                       :extend t))
	    (lsp-bridge--add-face filename (list :foreground lsp-bridge-peek--path-fg
					                         :extend t))
	    (lsp-bridge--add-face line (list :foreground lsp-bridge-peek--pos-fg
					                     :extend t))
	    (lsp-bridge--add-face char (list :foreground lsp-bridge-peek--pos-fg
					                     :extend t))
	    (if (= (+ first-displayed-id n) selected-id)
	        (setf (nth n displaying-list)
		          (lsp-bridge--add-face
		           (concat "(" method ") " filename " " line ":" char "\n")
		           bg-selected))
	      (setf (nth n displaying-list)
		        (lsp-bridge--add-face
		         (concat "(" method ") " filename " " line ":" char "\n")
		         bg-alt)))))
    (string-join displaying-list)))

(defun lsp-bridge-peek--tree-history ()
  "Return a string for displaying the tree history of symbols were peeked."
  (let* ((selected-symbol (nth lsp-bridge-peek-selected-symbol lsp-bridge-peek-symbol-tree))
	     (total-n (length (nth 1 selected-symbol)))
	     (selected-id (nth 1 lsp-bridge-peek-chosen-displaying-list))
	     (history-string (format "[%s]" (lsp-bridge--add-face
					                     (format "%s" (nth 0 selected-symbol))
					                     (list :foreground lsp-bridge-peek--symbol-selected :extend t)))))
    (while (nth 3 selected-symbol)
      (setq selected-symbol (nth (nth 3 selected-symbol) lsp-bridge-peek-symbol-tree))
      (setq history-string (concat (format "%s %s " (lsp-bridge--add-face
						                             (format "%s" (nth 0 selected-symbol))
						                             (list :foreground lsp-bridge-peek--symbol-alt
							                               :extend t))
					                       (if (> (length (nth 4 selected-symbol)) 1)
					                           "<" "->"))
				                   history-string)))
    (setq selected-symbol (nth lsp-bridge-peek-selected-symbol lsp-bridge-peek-symbol-tree))
    (while (nth 4 selected-symbol)
      (setq child-list (nth 4 selected-symbol))
      (setq selected-symbol (nth
			                 (nth
			                  (nth 5 selected-symbol)
			                  (nth 4 selected-symbol))
			                 lsp-bridge-peek-symbol-tree))
      (setq history-string (concat history-string
				                   (format " %s %s"
					                       (if (> (length child-list) 1)
					                           "<" "->")
					                       (lsp-bridge--add-face
					                        (format "%s" (nth 0 selected-symbol))
					                        (list :foreground lsp-bridge-peek--symbol-alt :extend t))))))
    (setq history-string (concat
			              (format "(%s/%s) " (1+ selected-id) total-n)
			              history-string "\n"))
    (lsp-bridge--add-face history-string (list :background lsp-bridge-peek--bg-alt
					                           :extend t))
    history-string))

(defun lsp-bridge-peek-define--return (filename filehost position)
  (setq filename (concat (cdr (assoc filehost lsp-bridge-tramp-alias-alist)) filename))
  (push filename (nth 1 lsp-bridge-peek-symbol-at-point))
  (push position (nth 2 lsp-bridge-peek-symbol-at-point))
  (push 0 (nth 6 lsp-bridge-peek-symbol-at-point))
  (if (not (= (length lsp-bridge-peek-ace-list) 0))
      (with-current-buffer (nth 3 lsp-bridge-peek-ace-list)
	    (goto-char (nth 4 lsp-bridge-peek-ace-list))
	    (lsp-bridge-call-file-api "peek_find_references" (lsp-bridge--position) position))
    (lsp-bridge-call-file-api "peek_find_references" (lsp-bridge--position) position)))

(defun lsp-bridge-peek-references--return (references-content references-counter)
  (if references-content
      (let ((buf (generate-new-buffer "*lsp-bridge-peek--temp-buffer*")))
	    (with-current-buffer buf
	      (insert references-content)
	      (goto-char (point-min))
	      (dotimes (n references-counter)
	        (let ((beg (point))
		          (end nil)
		          (filename nil)
		          (pos nil))
	          (move-end-of-line 1)
	          (setq end (point))
	          (setq line (buffer-substring beg end))
	          (forward-line 1)
	          (setq beg (point))
	          (move-end-of-line 1)
	          (setq end (point))
	          (setq char (buffer-substring beg end))
	          (forward-line 1)
	          (setq beg (point))
	          (move-end-of-line 1)
	          (setq end (point))
	          (forward-line 1)
	          (setq filename (buffer-substring beg end))
	          (setq pos (list :line (string-to-number line) :character (string-to-number char)))
	          (push filename (nth 1 lsp-bridge-peek-symbol-at-point))
	          (push pos (nth 2 lsp-bridge-peek-symbol-at-point))
		      (setf (nth 6 lsp-bridge-peek-symbol-at-point)
			        (nreverse (nth 6 lsp-bridge-peek-symbol-at-point)))
		      (push (- (/ lsp-bridge-peek-file-content-height 2)) (nth 6 lsp-bridge-peek-symbol-at-point))
		      (setf (nth 6 lsp-bridge-peek-symbol-at-point)
			        (nreverse (nth 6 lsp-bridge-peek-symbol-at-point))))))
	    (setf (nth 1 lsp-bridge-peek-symbol-at-point)
	          (nreverse (nth 1 lsp-bridge-peek-symbol-at-point))
	          (nth 2 lsp-bridge-peek-symbol-at-point)
	          (nreverse (nth 2 lsp-bridge-peek-symbol-at-point)))
	    (kill-buffer buf)))
  (if lsp-bridge-peek-selected-symbol
      (progn
	    (push (length lsp-bridge-peek-symbol-tree)
	          (nth 4 (nth lsp-bridge-peek-selected-symbol lsp-bridge-peek-symbol-tree)))
	    (unless (nth 5 (nth lsp-bridge-peek-selected-symbol lsp-bridge-peek-symbol-tree))
	      (setf (nth 5 (nth lsp-bridge-peek-selected-symbol lsp-bridge-peek-symbol-tree)) 0))))
  (setf (nth 3 lsp-bridge-peek-symbol-at-point) lsp-bridge-peek-selected-symbol)
  (setf (nth 5 lsp-bridge-peek-symbol-at-point) 0)
  (setq lsp-bridge-peek-selected-symbol (length lsp-bridge-peek-symbol-tree))
  (setq lsp-bridge-peek-symbol-tree (nreverse lsp-bridge-peek-symbol-tree))
  (push lsp-bridge-peek-symbol-at-point lsp-bridge-peek-symbol-tree)
  (setq lsp-bridge-peek-symbol-at-point nil)
  (setq lsp-bridge-peek-symbol-tree (nreverse lsp-bridge-peek-symbol-tree))
  (if (not (= (length lsp-bridge-peek-ace-list) 0))
      (progn
	    (if (nth 0 lsp-bridge-peek-ace-list)
	        (kill-buffer (nth 0 lsp-bridge-peek-ace-list)))
	    (switch-to-buffer (nth 2 lsp-bridge-peek-ace-list))
	    (goto-char (nth 1 lsp-bridge-peek-ace-list))))
  (if (not lsp-bridge-peek-mode)
      (progn
	    (lsp-bridge-peek-mode 1)
	    (lsp-bridge-peek--show))
    (lsp-bridge-peek--show 'force)))

(defun lsp-bridge-peek--show (&optional force)
  "Show the peek window or deal with the update of contents in peek windows.
When FORCE if non-nil, the content of the peek window is recalculated."
  (unless (minibufferp)
    (let ((overlay-pos (min (point-max) (1+ (line-end-position)))))
      (move-overlay lsp-bridge-peek--ov overlay-pos overlay-pos))
    (when (or lsp-bridge-peek--content-update force)
      (let* ((initial-newline (if (eq (line-end-position) (point-max)) "\n" ""))
	         (border (lsp-bridge-peek--make-border)))
	    (overlay-put lsp-bridge-peek--ov 'after-string
		             (concat initial-newline
			                 border
			                 (concat
			                  (lsp-bridge-peek--file-content)
			                  (lsp-bridge-peek--displaying-list)
			                  (lsp-bridge-peek--tree-history))
			                 border)))
      (setq lsp-bridge-peek--content-update nil))))

(defun lsp-bridge-peek ()
  "Peek the definition of the symbol at point."
  (interactive)
  (setq lsp-bridge-peek-symbol-at-point (make-list 7 nil))
  (setf (nth 0 lsp-bridge-peek-symbol-at-point) (symbol-at-point))
  (lsp-bridge-call-file-api "peek_find_definition" (lsp-bridge--position)))

(defun lsp-bridge-peek--error-if-not-peeking ()
  "Throw an error if not in a peek session."
  (unless lsp-bridge-peek-mode
    (user-error "Not in a peek session.")))

(defun lsp-bridge-peek-list-move-line (num)
  (lsp-bridge-peek--error-if-not-peeking)
  (cl-symbol-macrolet ((first-displayed-id (nth 0 lsp-bridge-peek-chosen-displaying-list))
		               (selected-id (nth 1 lsp-bridge-peek-chosen-displaying-list))
		               (last-displayed-id (nth 2 lsp-bridge-peek-chosen-displaying-list))
		               (next (> num 0)))
    (unless (or (and
		         (= selected-id (1- (length (nth 1 (nth lsp-bridge-peek-selected-symbol
							                            lsp-bridge-peek-symbol-tree)))))
		         next)
		        (and (= selected-id 0) (not next)))
      (if (or (and (= selected-id last-displayed-id) next)
	          (and (= selected-id first-displayed-id) (not next)))
	      (progn
	        (setf first-displayed-id
		          (+ num first-displayed-id))
	        (setf last-displayed-id
		          (+ num last-displayed-id))))
      (setf selected-id (+ selected-id num))))
  (setq lsp-bridge-peek--content-update t))

(defun lsp-bridge-peek-list-next-line ()
  "Choose the next definition/reference in the list."
  (interactive)
  (lsp-bridge-peek-list-move-line 1))

(defun lsp-bridge-peek-list-prev-line ()
  "Choose the prev definition/reference in the list."
  (interactive)
  (lsp-bridge-peek-list-move-line -1))

(defun lsp-bridge-peek-file-content-move (num)
  (lsp-bridge-peek--error-if-not-peeking)
  (cl-symbol-macrolet ((selected-id (nth 1 lsp-bridge-peek-chosen-displaying-list))
		               (selected-symbol (nth lsp-bridge-peek-selected-symbol lsp-bridge-peek-symbol-tree))
		               (content-move-list (nth 6 selected-symbol))
		               (content-move (nth selected-id content-move-list)))
    (setf content-move (+ content-move num))
    (setq lsp-bridge-peek--content-update t)))

(defun lsp-bridge-peek-file-content-next-line ()
  "Step through the next line of file content."
  (interactive)
  (lsp-bridge-peek-file-content-move lsp-bridge-peek-file-content-scroll-margin))

(defun lsp-bridge-peek-file-content-prev-line ()
  "Step through the next line of file content."
  (interactive)
  (lsp-bridge-peek-file-content-move (* lsp-bridge-peek-file-content-scroll-margin -1)))

(defun lsp-bridge-peek-jump ()
  "Jump to where the definition/reference is."
  (interactive)
  (lsp-bridge-peek--error-if-not-peeking)
  (setq lsp-bridge-peek-file-and-pos-before-jump (list))
  (push (point) lsp-bridge-peek-file-and-pos-before-jump)
  (push (buffer-file-name) lsp-bridge-peek-file-and-pos-before-jump)
  (let* ((selected-id (nth 1 lsp-bridge-peek-chosen-displaying-list))
	     (selected-symbol (nth lsp-bridge-peek-selected-symbol lsp-bridge-peek-symbol-tree))
	     (path-list (nth 1 selected-symbol))
	     (pos-list (nth 2 selected-symbol))
	     (file (nth selected-id path-list))
	     (pos (nth selected-id pos-list)))
    (lsp-bridge-jump-to-file file pos)
    ))

(defun lsp-bridge-peek-jump-back ()
  "Jump to the file and position before jump."
  (interactive)
  (when lsp-bridge-peek-file-and-pos-before-jump
    (lsp-bridge-jump-to-file
     (nth 0 lsp-bridge-peek-file-and-pos-before-jump)
     (nth 1 lsp-bridge-peek-file-and-pos-before-jump)
     )))

(defun lsp-bridge-peek--search-symbols (line)
  "Search for symbols from current position to LINEs after.
The search jumps over comments/strings.

The returned value is a list of cons pairs (START . END), the
start/end position of each symbol.  Point will not be moved."
  (let ((bound (save-excursion
		         (forward-line (1- line))
		         (line-end-position)))
	    (symbol-list))
    (save-excursion
      (cl-loop
       while
       (forward-symbol 1)
       do
       (when (> (point) bound)
	     (cl-return))
       (push (cons (save-excursion
		             (forward-symbol -1)
		             (point))
		           (point))
	         symbol-list)))
    (nreverse symbol-list)))

(defun lsp-bridge-peek--ace-key-seqs (n)
  "Make ace key sequences for N symbols.
N can be the length of the list returned by
`lsp-bridge-peek--search-symbols'.  The keys used are
`lsp-bridge-peek-ace-keys'."
  (unless (and (listp lsp-bridge-peek-ace-keys)
	           (null (cl-remove-if #'integerp lsp-bridge-peek-ace-keys))
	           (eq (cl-remove-duplicates lsp-bridge-peek-ace-keys)
		           lsp-bridge-peek-ace-keys))
    (user-error "Invalid `lsp-bridge-peek-ace-keys'"))
  (let* ((key-num (length lsp-bridge-peek-ace-keys))
	     (key-seq-length (pcase n
			               (0 0)
			               (1 1)
			               ;; Though `log' is a float-point operation, this is
                           ;; accurate for sym-num in a huge range.
			               (_ (ceiling (log n key-num)))))
	     (key-seq (make-list n nil))
	     nth-ace-key)
    (dotimes (nkey key-seq-length)
      (setq nth-ace-key -1)
      (dotimes (nsym n)
	    (when (eq (% nsym (expt key-num nkey)) 0)
	      (setq nth-ace-key (% (1+ nth-ace-key) key-num)))
	    (push (nth nth-ace-key lsp-bridge-peek-ace-keys) (nth nsym key-seq))))
    key-seq))

(defun lsp-bridge-peek--pop-ace-key-seqs (seqs char)
  "Modify ace key sequences SEQS as CHAR is pressed.
This sets elements in SEQS which not begin with CHAR to nil, and
pop the element which begin with CHAR.  When the only non-nil
element in seqs is poped, this returns its index, as the element
is hit by user input.

The modified SEQS is returned.  When CHAR is not the car of any
element in SEQS, this does nothing, and returns the original
list."
  (if (not (memq char (mapcar #'car seqs)))
      seqs
    (let (last-poped-idx)
      (dotimes (n (length seqs))
        (if (eq (car (nth n seqs)) char)
            (progn
              (pop (nth n seqs))
              (setq last-poped-idx n))
          (setf (nth n seqs) nil)))
      (if (null (cl-remove-if #'null seqs))
          last-poped-idx
        seqs))))

(defun lsp-bridge-ace-pick-point-in-peek-window ()
  "Pick a point in the buffer shown in peek window using \"ace\" operation.
The buffer and the point is returned in a cons cell."
  (let* ((selected-id (nth 1 lsp-bridge-peek-chosen-displaying-list))
	     (selected-symbol (nth lsp-bridge-peek-selected-symbol lsp-bridge-peek-symbol-tree))
	     (path-list (nth 1 selected-symbol))
	     (pos-list (nth 2 selected-symbol))
	     (content-move-list (nth 6 selected-symbol))
	     (file (nth selected-id path-list))
	     (buf (format "*lsp-bridge-peek-%s*" file))
	     (pos (nth selected-id pos-list))
	     (content-move (nth selected-id content-move-list)))
    (setq lsp-bridge-peek--symbol-bounds
	      (with-current-buffer buf
	        (save-excursion
	          (goto-char (acm-backend-lsp-position-to-point pos))
		      (forward-line content-move)
	          (beginning-of-line 1)
	          (cons (point)
		            (lsp-bridge-peek--search-symbols
		             lsp-bridge-peek-file-content-height)))))
    (setq lsp-bridge-peek--ace-seqs (lsp-bridge-peek--ace-key-seqs
				                     (length (cdr lsp-bridge-peek--symbol-bounds))))
    (lsp-bridge-peek--show 'force)
    (cl-block nil
      (while (setq key (read-key "Ace char:"))
	    (when (memq key lsp-bridge-peek-ace-cancel-keys)
	      (setq lsp-bridge-peek--symbol-bounds nil)
	      (setq lsp-bridge-peek--ace-seqs nil)
	      (lsp-bridge-peek--show 'force)
	      (cl-return))
	    (pcase (lsp-bridge-peek--pop-ace-key-seqs lsp-bridge-peek--ace-seqs key)
	      ((and (pred integerp) i)
	       (let ((pos (car (nth i (cdr lsp-bridge-peek--symbol-bounds)))))
	         (setq lsp-bridge-peek--symbol-bounds nil)
	         (setq lsp-bridge-peek--ace-seqs nil)
	         (lsp-bridge-peek--show 'force)
	         (cl-return (cons file pos))))
	      (_ (lsp-bridge-peek--show 'force)))))))

(defun lsp-bridge-peek-through ()
  "Peek through a symbol in current peek window."
  (interactive)
  (lsp-bridge-peek--error-if-not-peeking)
  (setq lsp-bridge-peek-ace-list nil)
  (setq lsp-bridge-peek-ace-list (make-list 5 nil))
  (setf (nth 1 lsp-bridge-peek-ace-list)
	    (point))
  (setf (nth 2 lsp-bridge-peek-ace-list)
	    (buffer-name))
  (with-temp-message ""
    (let* ((file-pos (lsp-bridge-ace-pick-point-in-peek-window))
	       (file (car file-pos))
	       (pos (cdr file-pos)))
      (setq temp-buf (find-buffer-visiting file))
      (find-file file)
      (save-excursion
	    (goto-char pos)
	    (setq lsp-bridge-peek-chosen-displaying-list (make-list 3 0))
	    (setf (nth 2 lsp-bridge-peek-chosen-displaying-list) (1- lsp-bridge-peek-list-height))
	    (lsp-bridge-peek)
	    (setq single-character (eq
				                (- (save-excursion
				                     (forward-symbol 1)
				                     (point))
				                   (point))
				                1))
	    (setf (nth 4 lsp-bridge-peek-ace-list)
	          (if single-character
		          (point)
		        (1+ (point)))))
      (if (not temp-buf)
	      (setf (nth 0 lsp-bridge-peek-ace-list)
		        (buffer-name)))
      (setf (nth 3 lsp-bridge-peek-ace-list) (buffer-name))))
  (switch-to-buffer (nth 2 lsp-bridge-peek-ace-list)))

(defun lsp-bridge-peek-tree-previous-node ()
  "Select the previous node in the tree history."
  (interactive)
  (lsp-bridge-peek--error-if-not-peeking)
  (setq lsp-bridge-peek-chosen-displaying-list (make-list 3 0))
  (setf (nth 2 lsp-bridge-peek-chosen-displaying-list) (1- lsp-bridge-peek-list-height))
  (let* ((selected-symbol (nth lsp-bridge-peek-selected-symbol lsp-bridge-peek-symbol-tree))
	     (parent-symbol (nth 3 selected-symbol)))
    (when parent-symbol
	  (setq lsp-bridge-peek-selected-symbol parent-symbol)
	  (setq lsp-bridge-peek--content-update t))))

(defun lsp-bridge-peek-tree-next-node ()
  "Select the next node in the tree history."
  (interactive)
  (lsp-bridge-peek--error-if-not-peeking)
  (setq lsp-bridge-peek-chosen-displaying-list (make-list 3 0))
  (setf (nth 2 lsp-bridge-peek-chosen-displaying-list) (1- lsp-bridge-peek-list-height))
  (let* ((selected-symbol (nth lsp-bridge-peek-selected-symbol lsp-bridge-peek-symbol-tree))
	     (child-list (nth 4 selected-symbol))
	     (selected-child (nth 5 selected-symbol)))
    (when child-list
	  (setq lsp-bridge-peek-selected-symbol (nth selected-child child-list))
	  (setq lsp-bridge-peek--content-update t))))

(defun lsp-bridge-peek-tree-change-branch (num)
  (lsp-bridge-peek--error-if-not-peeking)
  (setq lsp-bridge-peek-chosen-displaying-list (make-list 3 0))
  (setf (nth 2 lsp-bridge-peek-chosen-displaying-list) (1- lsp-bridge-peek-list-height))
  (cl-symbol-macrolet ((selected-symbol
			             (nth lsp-bridge-peek-selected-symbol lsp-bridge-peek-symbol-tree)))
    (if (nth 3 selected-symbol)
	    (cl-symbol-macrolet ((parent-symbol (nth (nth 3 selected-symbol) lsp-bridge-peek-symbol-tree))
			                 (brother-list (nth 4 parent-symbol))
			                 (selected-brother (nth 5 parent-symbol)))
	      (if selected-brother
	          (progn
		        (setq already-changed nil)
		        (unless (or (< (+ num selected-brother) 0)
			                (> (+ num selected-brother) (1- (length brother-list))))
		          (setq selected-brother (+ num selected-brother))
		          (setq already-changed t))
		        (unless already-changed
		          (if (< (+ num selected-brother) 0)
		              (progn
			            (setq selected-brother (1- (length brother-list)))
			            (setq already-changed t))))
		        (unless already-changed
		          (if (> (+ num selected-brother) (1- (length brother-list)))
		              (setq selected-brother 0)))
		        (setq lsp-bridge-peek-selected-symbol (nth selected-brother brother-list))
		        (setq lsp-bridge-peek--content-update t)))))))

(defun lsp-bridge-peek-tree-previous-branch ()
  "Select the previous brach in the tree history."
  (interactive)
  (lsp-bridge-peek-tree-change-branch 1))

(defun lsp-bridge-peek-tree-next-branch ()
  "Select the next brach in the tree history."
  (interactive)
  (lsp-bridge-peek-tree-change-branch -1))


(provide 'lsp-bridge-peek)
;;; lsp-bridge-peek.el ends here.
