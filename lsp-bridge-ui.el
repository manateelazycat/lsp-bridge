;;; lsp-bridge-ui.el --- LSP bridge  -*- lexical-binding: t; -*-

;; Filename: lsp-bridge-ui.el
;; Description: LSP bridge
;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Daniel Mendler, Andy Stewart, all rights reserved.
;; Created: 2022-05-01 14:10:12
;; Version: 0.5
;; Last-Updated: Wed May 11 02:44:16 2022 (-0400)
;;           By: Mingde (Matthew) Zeng
;; URL: https://github.com/manateelazycat/lsp-bridge-ui
;; Keywords:
;; Compatibility: emacs-version >= 27
;; Package-Requires: ((emacs "27"))
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
;; Lsp-Bridge-Ui
;;

;;; Installation:
;;
;; Please check README
;;

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET lsp-bridge-ui RET
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

;;; Code:

(require 'seq)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'all-the-icons)
(require 'lsp-bridge)

(defgroup lsp-bridge-ui nil
  "Completion Overlay Region FUnction."
  :group 'convenience
  :prefix "lsp-bridge-ui-")

(defcustom lsp-bridge-ui-count 10
  "Maximal number of candidates to show."
  :type 'integer)

(defcustom lsp-bridge-ui-scroll-margin 2
  "Number of lines at the top and bottom when scrolling.
The value should lie between 0 and lsp-bridge-ui-count/2."
  :type 'integer)

(defcustom lsp-bridge-ui-min-width 15
  "Popup minimum width in characters."
  :type 'integer)

(defcustom lsp-bridge-ui-max-width 100
  "Popup maximum width in characters."
  :type 'integer)

(defcustom lsp-bridge-ui-cycle nil
  "Enable cycling for `lsp-bridge-ui-next' and `lsp-bridge-ui-previous'."
  :type 'boolean)

(defcustom lsp-bridge-ui-on-exact-match 'insert
  "Configure how a single exact match should be handled."
  :type '(choice (const insert) (const quit) (const nil)))

(defcustom lsp-bridge-ui-continue-commands
  ;; nil is undefined command
  '(nil ignore universal-argument universal-argument-more digit-argument
        "\\`lsp-bridge-ui-" "\\`scroll-other-window")
  "Continue Lsp-Bridge-Ui completion after executing these commands."
  :type '(repeat (choice regexp symbol)))

(defcustom lsp-bridge-ui-preview-current 'insert
  "Preview currently selected candidate.
If the variable has the value `insert', the candidate is automatically
inserted on further input."
  :type '(choice boolean (const insert)))

(defcustom lsp-bridge-ui-preselect-first t
  "Preselect first candidate."
  :type 'boolean)

(defcustom lsp-bridge-ui-separator ?\s
  "Component separator character.
The character used for separating components in the input. The presence
of this separator character will inhibit quitting at completion
boundaries, so that any further characters can be entered. To enter the
first separator character, call `lsp-bridge-ui-insert-separator' (bound to M-SPC
by default). Useful for multi-component completion styles such as
Orderless."
  :type 'character)

(defcustom lsp-bridge-ui-quit-at-boundary 'separator
  "Automatically quit at completion boundary.
nil: Never quit at completion boundary.
t: Always quit at completion boundary.
separator: Quit at boundary if no `lsp-bridge-ui-separator' has been inserted."
  :type '(choice boolean (const separator)))

(defcustom lsp-bridge-ui-quit-no-match 'separator
  "Automatically quit if no matching candidate is found.
nil: Stay alive even if there is no match.
t: Quit if there is no match.
separator: Only stay alive if there is no match and
`lsp-bridge-ui-separator' has been inserted."
  :type '(choice boolean (const separator)))

(defcustom lsp-bridge-ui-excluded-modes nil
  "List of modes excluded by `global-lsp-bridge-ui-mode'."
  :type '(repeat symbol))

(defcustom lsp-bridge-ui-left-margin-width 0.5
  "Width of the left margin in units of the character width."
  :type 'float)

(defcustom lsp-bridge-ui-right-margin-width 0.5
  "Width of the right margin in units of the character width."
  :type 'float)

(defcustom lsp-bridge-ui-bar-width 0.2
  "Width of the bar in units of the character width."
  :type 'float)

(defcustom lsp-bridge-ui-echo-documentation '(1.0 . 0.2)
  "Show documentation string in the echo area after that number of seconds.
Set to nil to disable the echo message or to t for an instant message.
The value can be a pair of two floats to specify initial and subsequent
delay."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Instant" t)
                 (number :tag "Delay in seconds")
                 (cons :tag "Two Delays"
                       (choice :tag "Initial   " number))
                 (choice :tag "Subsequent" number)))

(defcustom lsp-bridge-ui-margin-formatters nil
  "Registry for margin formatter functions.
Each function of the list is called with the completion metadata as
argument until an appropriate formatter is found. The function should
return a formatter function, which takes the candidate string and must
return a string, possibly an icon."
  :type 'hook)

(defcustom lsp-bridge-ui-sort-function #'lsp-bridge-ui-sort-length-alpha
  "Default sorting function, used if no `display-sort-function' is specified."
  :type `(choice
          (const :tag "No sorting" nil)
          (const :tag "By length and alpha" ,#'lsp-bridge-ui-sort-length-alpha)
          (function :tag "Custom function")))

(defcustom lsp-bridge-ui-sort-override-function nil
  "Override sort function which overrides the `display-sort-function'."
  :type '(choice (const nil) function))

(defcustom lsp-bridge-ui-auto-prefix 3
  "Minimum length of prefix for auto completion.
The completion backend can override this with
:company-prefix-length."
  :type 'integer)

(defcustom lsp-bridge-ui-auto-delay 0.2
  "Delay for auto completion."
  :type 'float)

(defcustom lsp-bridge-ui-auto-commands
  '("self-insert-command\\'"
    c-electric-colon c-electric-lt-gt c-electric-slash c-scope-operator)
  "Commands which initiate auto completion."
  :type '(repeat (choice regexp symbol)))

(defcustom lsp-bridge-ui-auto nil
  "Enable auto completion."
  :type 'boolean)

(defgroup lsp-bridge-ui-faces nil
  "Faces used by Lsp-Bridge-Ui."
  :group 'lsp-bridge-ui
  :group 'faces)

(defface lsp-bridge-ui-default
  '((((class color) (min-colors 88) (background dark)) :background "#191a1b")
    (((class color) (min-colors 88) (background light)) :background "#f0f0f0")
    (t :background "gray"))
  "Default face, foreground and background colors used for the popup.")

(defface lsp-bridge-ui-current
  '((((class color) (min-colors 88) (background dark))
     :background "#00415e" :foreground "white")
    (((class color) (min-colors 88) (background light))
     :background "#c0efff" :foreground "black")
    (t :background "blue" :foreground "white"))
  "Face used to highlight the currently selected candidate.")

(defface lsp-bridge-ui-bar
  '((((class color) (min-colors 88) (background dark)) :background "#a8a8a8")
    (((class color) (min-colors 88) (background light)) :background "#505050")
    (t :background "gray"))
  "The background color is used for the scrollbar indicator.")

(defface lsp-bridge-ui-border
  '((((class color) (min-colors 88) (background dark)) :background "#323232")
    (((class color) (min-colors 88) (background light)) :background "#d7d7d7")
    (t :background "gray"))
  "The background color used for the thin border.")

(defface lsp-bridge-ui-echo
  '((t :inherit completions-annotations))
  "Face used for echo area messages.")

(defface lsp-bridge-ui-annotations
  '((t :inherit completions-annotations))
  "Face used for annotations.")

(defface lsp-bridge-ui-deprecated
  '((t :inherit shadow :strike-through t))
  "Face used for deprecated candidates.")

(defvar lsp-bridge-ui-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap beginning-of-buffer] #'lsp-bridge-ui-first)
    (define-key map [remap end-of-buffer] #'lsp-bridge-ui-last)
    (define-key map [remap scroll-down-command] #'lsp-bridge-ui-scroll-down)
    (define-key map [remap scroll-up-command] #'lsp-bridge-ui-scroll-up)
    (define-key map [remap next-line] #'lsp-bridge-ui-next)
    (define-key map [remap previous-line] #'lsp-bridge-ui-previous)
    (define-key map [remap completion-at-point] #'lsp-bridge-ui-complete)
    (define-key map [down] #'lsp-bridge-ui-next)
    (define-key map [up] #'lsp-bridge-ui-previous)
    (define-key map [remap keyboard-escape-quit] #'lsp-bridge-ui-reset)
    ;; XXX [tab] is bound because of org-mode
    ;; The binding should be removed from org-mode-map.
    (define-key map [tab] #'lsp-bridge-ui-complete)
    (define-key map "\M-n" #'lsp-bridge-ui-next)
    (define-key map "\M-p" #'lsp-bridge-ui-previous)
    (define-key map "\C-g" #'lsp-bridge-ui-quit)
    (define-key map "\r" #'lsp-bridge-ui-insert)
    (define-key map "\t" #'lsp-bridge-ui-complete)
    (define-key map "\M- " #'lsp-bridge-ui-insert-separator)
    map)
  "Lsp-Bridge-Ui keymap used when popup is shown.")

(defvar lsp-bridge-ui--auto-timer nil
  "Auto completion timer.")

(defvar-local lsp-bridge-ui--candidates nil
  "List of candidates.")

(defvar-local lsp-bridge-ui--metadata nil
  "Completion metadata.")

(defvar-local lsp-bridge-ui--base ""
  "Base string, which is concatenated with the candidate.")

(defvar-local lsp-bridge-ui--total 0
  "Length of the candidate list `lsp-bridge-ui--candidates'.")

(defvar-local lsp-bridge-ui--highlight #'identity
  "Deferred candidate highlighting function.")

(defvar-local lsp-bridge-ui--index -1
  "Index of current candidate or negative for prompt selection.")

(defvar-local lsp-bridge-ui--preselect -1
  "Index of preselected candidate, negative for prompt selection.")

(defvar-local lsp-bridge-ui--scroll 0
  "Scroll position.")

(defvar-local lsp-bridge-ui--input nil
  "Cons of last prompt contents and point.")

(defvar-local lsp-bridge-ui--preview-ov nil
  "Current candidate overlay.")

(defvar-local lsp-bridge-ui--extra nil
  "Extra completion properties.")

(defvar-local lsp-bridge-ui--change-group nil
  "Undo change group.")

(defvar-local lsp-bridge-ui--echo-timer nil
  "Echo area message timer.")

(defvar-local lsp-bridge-ui--echo-message nil
  "Last echo message.")

(defvar lsp-bridge-ui--frame nil
  "Popup frame.")

(defconst lsp-bridge-ui--state-vars
  '(lsp-bridge-ui--base
    lsp-bridge-ui--candidates
    lsp-bridge-ui--highlight
    lsp-bridge-ui--index
    lsp-bridge-ui--preselect
    lsp-bridge-ui--scroll
    lsp-bridge-ui--input
    lsp-bridge-ui--total
    lsp-bridge-ui--preview-ov
    lsp-bridge-ui--extra
    lsp-bridge-ui--echo-timer
    lsp-bridge-ui--echo-message
    lsp-bridge-ui--change-group
    lsp-bridge-ui--metadata)
  "Buffer-local state variables used by Lsp-Bridge-Ui.")

(defvar lsp-bridge-ui--frame-parameters
  '((no-accept-focus . t)
    (no-focus-on-map . t)
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
    (desktop-dont-save . t))
  "Default child frame parameters.")

(defvar lsp-bridge-ui--buffer-parameters
  '((mode-line-format . nil)
    (header-line-format . nil)
    (tab-line-format . nil)
    (tab-bar-format . nil) ;; Emacs 28 tab-bar-format
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
    (fringes-outside-margins . 0)
    (buffer-read-only . t))
  "Default child frame buffer parameters.")

(defvar lsp-bridge-ui--mouse-ignore-map
  (let ((map (make-sparse-keymap)))
    (dotimes (i 7)
      (dolist (k '(mouse down-mouse drag-mouse double-mouse triple-mouse))
        (define-key map (vector (intern (format "%s-%s" k (1+ i)))) #'ignore)))
    map)
  "Ignore all mouse clicks.")

(defun lsp-bridge-ui--make-buffer (content)
  "Create lsp-bridge-ui buffer with CONTENT."
  (let ((fr face-remapping-alist)
        (ls line-spacing)
        (buffer (get-buffer-create " *lsp-bridge-ui*")))
    (with-current-buffer buffer
;;; XXX HACK install mouse ignore map
      (use-local-map lsp-bridge-ui--mouse-ignore-map)
      (dolist (var lsp-bridge-ui--buffer-parameters)
        (set (make-local-variable (car var)) (cdr var)))
      (setq-local face-remapping-alist (copy-tree fr)
                  line-spacing ls)

      ;; Adjust default font height when running in HiDPI screen.
      (when (> (frame-pixel-width) 3000)
        (custom-set-faces '(lsp-bridge-ui-default ((t (:height 1.3))))))

      (cl-pushnew 'lsp-bridge-ui-default (alist-get 'default face-remapping-alist))
      (let ((inhibit-modification-hooks t)
            (inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (goto-char (point-min))))
    buffer))

;; Function adapted from posframe.el by tumashu
(defvar x-gtk-resize-child-frames) ;; Not present on non-gtk builds
(defun lsp-bridge-ui--make-frame (x y width height content)
  "Show child frame at X/Y with WIDTH/HEIGHT and CONTENT."
  (let* ((window-min-height 1)
         (window-min-width 1)
         (x-gtk-resize-child-frames
          (let ((case-fold-search t))
            (and
             ;; XXX HACK to fix resizing on gtk3/gnome taken from posframe.el
             ;; More information:
             ;; * https://github.com/minad/lsp-bridge-ui/issues/17
             ;; * https://gitlab.gnome.org/GNOME/mutter/-/issues/840
             ;; * https://lists.gnu.org/archive/html/emacs-devel/2020-02/msg00001.html
             (string-match-p "gtk3" system-configuration-features)
             (string-match-p "gnome\\|cinnamon"
                             (or (getenv "XDG_CURRENT_DESKTOP")
                                 (getenv "DESKTOP_SESSION") ""))
             'resize-mode)))
         (after-make-frame-functions)
         (edge (window-inside-pixel-edges))
         (ch (default-line-height))
         (border (alist-get 'child-frame-border-width lsp-bridge-ui--frame-parameters))
         (x (max border (min (+ (car edge) x (- border))
                             (- (frame-pixel-width) width))))
         (yb (+ (cadr edge) (window-tab-line-height) y ch))
         (y (if (> (+ yb (* lsp-bridge-ui-count ch) ch ch) (frame-pixel-height))
                (- yb height ch 1)
              yb))
         (buffer (lsp-bridge-ui--make-buffer content))
         (parent (window-frame)))
    (unless (and (frame-live-p lsp-bridge-ui--frame)
                 (eq (frame-parent lsp-bridge-ui--frame) parent))
      (when lsp-bridge-ui--frame (delete-frame lsp-bridge-ui--frame))
      (setq lsp-bridge-ui--frame (make-frame
                                  `((parent-frame . ,parent)
                                    (minibuffer . ,(minibuffer-window parent))
                                    ;; Set `internal-border-width' for Emacs 27
                                    (internal-border-width . ,border)
                                    ,@lsp-bridge-ui--frame-parameters))))
    ;; XXX HACK Setting the same frame-parameter/face-background is not a nop.
    ;; Check explicitly before applying the setting. Without the check, the
    ;; frame flickers on Mac.
    ;; XXX HACK We have to apply the face background before adjusting the frame
    ;; parameter, otherwise the border is not updated (BUG!).
    (let* ((face (if (facep 'child-frame-border) 'child-frame-border 'internal-border))
           (new (face-attribute 'lsp-bridge-ui-border :background nil 'default)))
      (unless (equal (face-attribute face :background lsp-bridge-ui--frame 'default) new)
        (set-face-background face new lsp-bridge-ui--frame)))
    (let ((new (face-attribute 'lsp-bridge-ui-default :background nil 'default)))
      (unless (equal (frame-parameter lsp-bridge-ui--frame 'background-color) new)
        (set-frame-parameter lsp-bridge-ui--frame 'background-color new)))
    (let ((win (frame-root-window lsp-bridge-ui--frame)))
      (set-window-buffer win buffer)
      ;; Mark window as dedicated to prevent frame reuse (#60)
      (set-window-dedicated-p win t))
    (set-frame-size lsp-bridge-ui--frame width height t)
    (if (frame-visible-p lsp-bridge-ui--frame)
        ;; XXX HACK Avoid flicker when frame is already visible.
        ;; Redisplay, wait for resize and then move the frame.
        (unless (equal (frame-position lsp-bridge-ui--frame) (cons x y))
          (redisplay 'force)
          (sleep-for 0.01)
          (set-frame-position lsp-bridge-ui--frame x y))
      ;; XXX HACK: Force redisplay, otherwise the popup sometimes does not
      ;; display content.
      (set-frame-position lsp-bridge-ui--frame x y)
      (redisplay 'force)
      (make-frame-visible lsp-bridge-ui--frame))
    (redirect-frame-focus lsp-bridge-ui--frame parent)))

(defun lsp-bridge-ui--popup-show (pos off width lines &optional curr lo bar)
  "Show LINES as popup at POS - OFF.
WIDTH is the width of the popup.
The current candidate CURR is highlighted.
A scroll bar is displayed from LO to LO+BAR."
  (let* ((ch (default-line-height))
         (cw (default-font-width))
         (ml (ceiling (* cw lsp-bridge-ui-left-margin-width)))
         (mr (ceiling (* cw lsp-bridge-ui-right-margin-width)))
         (bw (ceiling (min mr (* cw lsp-bridge-ui-bar-width))))
         (marginl (and (> ml 0) (propertize " " 'display `(space :width (,ml)))))
         (marginr (and (> mr 0) (propertize " " 'display `(space :align-to right))))
         (sbar (when (> bw 0)
                 (concat (propertize " " 'display `(space :align-to (- right (,mr))))
                         (propertize " " 'display `(space :width (,(- mr bw))))
                         (propertize " " 'face 'lsp-bridge-ui-bar 'display `(space :width (,bw))))))
         (row 0)
         (pos (posn-x-y (posn-at-point pos)))
         (x (or (car pos) 0))
         (y (or (cdr pos) 0)))
    (lsp-bridge-ui--make-frame
     (- x ml (* cw off)) y
     (+ (* width cw) ml mr) (* (length lines) ch)
     (mapconcat (lambda (line)
                  (let ((str (concat marginl line
                                     (if (and lo (<= lo row (+ lo bar))) sbar marginr))))
                    (when (eq row curr)
                      (add-face-text-property
                       0 (length str) 'lsp-bridge-ui-current 'append str))
                    (setq row (1+ row))
                    str))
                lines "\n"))))

(defun lsp-bridge-ui--popup-hide ()
  "Hide Lsp-Bridge-Ui popup."
  (when (frame-live-p lsp-bridge-ui--frame)
    (make-frame-invisible lsp-bridge-ui--frame)
    (with-current-buffer (window-buffer (frame-root-window lsp-bridge-ui--frame))
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(defun lsp-bridge-ui--popup-support-p ()
  "Return non-nil if child frames are supported."
  (display-graphic-p))

(defun lsp-bridge-ui--move-to-front (elem list)
  "Move ELEM to front of LIST."
  (if-let (found (member elem list))
      (let ((head (list (car found))))
        (nconc head (delq (setcar found nil) list)))
    list))

;; bug#47711: Deferred highlighting for `completion-all-completions'
;; XXX There is one complication: `completion--twq-all' already adds
;; `completions-common-part'.
(defun lsp-bridge-ui--all-completions (&rest args)
  "Compute all completions for ARGS with deferred highlighting."
  (cl-letf* ((orig-pcm (symbol-function #'completion-pcm--hilit-commonality))
             (orig-flex (symbol-function #'completion-flex-all-completions))
             ((symbol-function #'completion-flex-all-completions)
              (lambda (&rest args)
                ;; Unfortunately for flex we have to undo the deferred
                ;; highlighting, since flex uses the completion-score for
                ;; sorting, which is applied during highlighting.
                (cl-letf (((symbol-function #'completion-pcm--hilit-commonality) orig-pcm))
                  (apply orig-flex args))))
             ;; Defer the following highlighting functions
             (hl #'identity)
             ((symbol-function #'completion-hilit-commonality)
              (lambda (cands prefix &optional base)
                (setq hl (lambda (x) (nconc (completion-hilit-commonality x prefix base) nil)))
                (and cands (nconc cands base))))
             ((symbol-function #'completion-pcm--hilit-commonality)
              (lambda (pattern cands)
                (setq hl (lambda (x)
                           ;; `completion-pcm--hilit-commonality' sometimes
                           ;; throws an internal error for example when entering
                           ;; "/sudo:://u".
                           (condition-case nil
                               (completion-pcm--hilit-commonality pattern x)
                             (t x))))
                cands)))
    ;; Only advise orderless after it has been loaded to avoid load order issues
    (if (and (fboundp 'orderless-highlight-matches)
             (fboundp 'orderless-pattern-compiler))
        (cl-letf (((symbol-function 'orderless-highlight-matches)
                   (lambda (pattern cands)
                     (let ((regexps (orderless-pattern-compiler pattern)))
                       (setq hl (lambda (x) (orderless-highlight-matches regexps x))))
                     cands)))
          (cons (apply #'completion-all-completions args) hl))
      (cons (apply #'completion-all-completions args) hl))))

(defun lsp-bridge-ui--sort-predicate (x y)
  "Sorting predicate which compares X and Y."
  (or (< (length x) (length y)) (and (= (length x) (length y)) (string< x y))))

(defun lsp-bridge-ui-sort-length-alpha (list)
  "Sort LIST by length and alphabetically."
  (sort list #'lsp-bridge-ui--sort-predicate))

(defmacro lsp-bridge-ui--partition! (list form)
  "Evaluate FORM for every element and partition LIST."
  (let ((head1 (make-symbol "head1"))
        (head2 (make-symbol "head2"))
        (tail1 (make-symbol "tail1"))
        (tail2 (make-symbol "tail2")))
    `(let* ((,head1 (cons nil nil))
            (,head2 (cons nil nil))
            (,tail1 ,head1)
            (,tail2 ,head2))
       (while ,list
         (if (let ((it (car ,list))) ,form)
             (progn
               (setcdr ,tail1 ,list)
               (pop ,tail1))
           (setcdr ,tail2 ,list)
           (pop ,tail2))
         (pop ,list))
       (setcdr ,tail1 (cdr ,head2))
       (setcdr ,tail2 nil)
       (setq ,list (cdr ,head1)))))

(defun lsp-bridge-ui--move-prefix-candidates-to-front (field candidates)
  "Move CANDIDATES which match prefix of FIELD to the beginning."
  (let* ((word (substring field 0
                          (seq-position field lsp-bridge-ui-separator)))
         (len (length word)))
    (lsp-bridge-ui--partition!
     candidates
     (and (>= (length it) len)
          (eq t (compare-strings word 0 len it 0 len
                                 completion-ignore-case))))))

(defun lsp-bridge-ui--filter-files (files)
  "Filter FILES by `completion-ignored-extensions'."
  (let ((re (concat "\\(?:\\(?:\\`\\|/\\)\\.\\.?/\\|"
                    (regexp-opt completion-ignored-extensions)
                    "\\)\\'")))
    (or (seq-remove (lambda (x) (string-match-p re x)) files) files)))

(defun lsp-bridge-ui--sort-function ()
  "Return the sorting function."
  (or lsp-bridge-ui-sort-override-function
      (lsp-bridge-ui--metadata-get 'display-sort-function)
      lsp-bridge-ui-sort-function))

(defun lsp-bridge-ui--recompute-candidates (str pt table pred)
  "Recompute candidates from STR, PT, TABLE and PRED."
  (pcase-let* ((before (substring str 0 pt))
               (after (substring str pt))
               (lsp-bridge-ui--metadata (completion-metadata before table pred))
               ;; bug#47678: `completion-boundaries` fails for `partial-completion`
               ;; if the cursor is moved between the slashes of "~//".
               ;; See also vertico.el which has the same issue.
               (bounds (or (condition-case nil
                               (completion-boundaries before table pred after)
                             (t (cons 0 (length after))))))
               (field (substring str (car bounds) (+ pt (cdr bounds))))
               (completing-file (eq (lsp-bridge-ui--metadata-get 'category) 'file))
               (`(,all . ,hl) (lsp-bridge-ui--all-completions str table pred pt lsp-bridge-ui--metadata))
               (base (or (when-let (z (last all)) (prog1 (cdr z) (setcdr z nil))) 0))
               (lsp-bridge-ui--base (substring str 0 base)))
    ;; Filter the ignored file extensions. We cannot use modified predicate for
    ;; this filtering, since this breaks the special casing in the
    ;; `completion-file-name-table' for `file-exists-p' and `file-directory-p'.
    (when completing-file (setq all (lsp-bridge-ui--filter-files all)))
    (setq all (delete-consecutive-dups (funcall (or (lsp-bridge-ui--sort-function) #'identity) all)))
    (setq all (lsp-bridge-ui--move-prefix-candidates-to-front field all))
    (when (and completing-file (not (string-suffix-p "/" field)))
      (setq all (lsp-bridge-ui--move-to-front (concat field "/") all)))
    (setq all (lsp-bridge-ui--move-to-front field all))
    (list lsp-bridge-ui--base all (length all) hl lsp-bridge-ui--metadata
          ;; Select the prompt when the input is a valid completion
          ;; and if it is not equal to the first candidate.
          (if (or (not lsp-bridge-ui-preselect-first) (not all)
                  (and (not (equal field (car all)))
                       (not (and completing-file (equal (concat field "/") (car all))))
                       (test-completion str table pred)))
              -1 0))))

(defun lsp-bridge-ui--update-candidates (str pt table pred)
  "Update candidates from STR, PT, TABLE and PRED."
  ;; Redisplay such that the input becomes immediately visible before the
  ;; expensive candidate recomputation is performed (Issue #48). See also
  ;; corresponding vertico#89.
  (redisplay)
  (pcase
      ;; Bind non-essential=t to prevent Tramp from opening new connections,
      ;; without the user explicitly requesting it via M-TAB.
      (let ((non-essential t))
        (while-no-input (lsp-bridge-ui--recompute-candidates str pt table pred)))
    ('nil (keyboard-quit))
    (`(,base ,candidates ,total ,hl ,metadata ,preselect)
     (setq lsp-bridge-ui--input (cons str pt)
           lsp-bridge-ui--candidates candidates
           lsp-bridge-ui--base base
           lsp-bridge-ui--total total
           lsp-bridge-ui--preselect preselect
           lsp-bridge-ui--index preselect
           lsp-bridge-ui--highlight hl
           lsp-bridge-ui--metadata metadata))))

(defun lsp-bridge-ui--match-symbol-p (pattern sym)
  "Return non-nil if SYM is matching an element of the PATTERN list."
  (and (symbolp sym)
       (cl-loop for x in pattern
                thereis (if (symbolp x)
                            (eq sym x)
                          (string-match-p x (symbol-name sym))))))

(defun lsp-bridge-ui-quit ()
  "Quit Lsp-Bridge-Ui completion."
  (interactive)
  (completion-in-region-mode -1))

(defun lsp-bridge-ui-reset ()
  "Reset Lsp-Bridge-Ui completion.
This command can be executed multiple times by hammering the ESC key. If a
candidate is selected, unselect the candidate. Otherwise reset the input. If
there hasn't been any input, then quit."
  (interactive)
  (if (/= lsp-bridge-ui--index lsp-bridge-ui--preselect)
      (progn
        (lsp-bridge-ui--goto -1)
        (setq this-command #'lsp-bridge-ui-first))
    ;; Cancel all changes and start new change group.
    (cancel-change-group lsp-bridge-ui--change-group)
    (activate-change-group (setq lsp-bridge-ui--change-group (prepare-change-group)))
    (when (eq last-command #'lsp-bridge-ui-reset) (lsp-bridge-ui-quit))))

(defun lsp-bridge-ui--affixate (cands)
  "Annotate CANDS with annotation function."
  (setq cands
        (if-let (aff (or (lsp-bridge-ui--metadata-get 'affixation-function)
                         (plist-get lsp-bridge-ui--extra :affixation-function)))
            (funcall aff cands)
          (if-let (ann (or (lsp-bridge-ui--metadata-get 'annotation-function)
                           (plist-get lsp-bridge-ui--extra :annotation-function)))
              (cl-loop for cand in cands collect
                       (let ((suffix (or (funcall ann cand) "")))
                         ;; The default completion UI adds the
                         ;; `completions-annotations' face if no other faces are
                         ;; present. We use a custom `lsp-bridge-ui-annotations' face to
                         ;; allow further styling which fits better for popups.
                         (unless (text-property-not-all 0 (length suffix) 'face nil suffix)
                           (setq suffix (propertize suffix 'face 'lsp-bridge-ui-annotations)))
                         (list cand "" suffix)))
            (cl-loop for cand in cands collect (list cand "" "")))))
  (let* ((dep (plist-get lsp-bridge-ui--extra :company-deprecated))
         (completion-extra-properties lsp-bridge-ui--extra)
         (mf (run-hook-with-args-until-success 'lsp-bridge-ui-margin-formatters lsp-bridge-ui--metadata)))
    (cl-loop for x in cands for (c . _) = x do
             (when mf
               (setf (cadr x) (funcall mf c)))
             (when (and dep (funcall dep c))
               (setcar x (setq c (substring c)))
               (add-face-text-property 0 (length c) 'lsp-bridge-ui-deprecated 'append c)))
    (cons mf cands)))

(defun lsp-bridge-ui--metadata-get (prop)
  "Return PROP from completion metadata."
  ;; Note: Do not use `completion-metadata-get' in order to avoid Marginalia.
  ;; The Marginalia annotators are too heavy for the Lsp-Bridge-Ui popup!
  (cdr (assq prop lsp-bridge-ui--metadata)))

(defun lsp-bridge-ui--format-candidates (cands)
  "Format annotated CANDS."
  (setq cands
        (cl-loop for c in cands collect
                 (cl-loop for s in c collect
                          (replace-regexp-in-string "[ \t]*\n[ \t]*" " " s))))
  (let* ((cw (cl-loop for x in cands maximize (string-width (car x))))
         (pw (cl-loop for x in cands maximize (string-width (cadr x))))
         (sw (cl-loop for x in cands maximize (string-width (caddr x))))
         (width (+ pw cw sw))
         ;; -4 because of margins and some additional safety
         (max-width (min lsp-bridge-ui-max-width (- (frame-width) 4))))
    (when (> width max-width)
      (setq sw (max 0 (- max-width pw cw))
            width (+ pw cw sw)))
    (when (< width lsp-bridge-ui-min-width)
      (setq cw (+ cw (- lsp-bridge-ui-min-width width))
            width lsp-bridge-ui-min-width))
    (setq width (min width max-width))
    (list pw width
          (cl-loop for (cand prefix suffix) in cands collect
                   (truncate-string-to-width
                    (concat prefix
                            (make-string (max 0 (- pw (string-width prefix))) ?\s)
                            cand
                            (when (/= sw 0)
                              (make-string
                               (+ (max 0 (- cw (string-width cand)))
                                  (max 0 (- sw (string-width suffix))))
                               ?\s))
                            suffix)
                    width)))))

(defun lsp-bridge-ui--update-scroll ()
  "Update scroll position."
  (let ((off (max (min lsp-bridge-ui-scroll-margin (/ lsp-bridge-ui-count 2)) 0))
        (corr (if (= lsp-bridge-ui-scroll-margin (/ lsp-bridge-ui-count 2)) (1- (mod lsp-bridge-ui-count 2)) 0)))
    (setq lsp-bridge-ui--scroll (min (max 0 (- lsp-bridge-ui--total lsp-bridge-ui-count))
                                     (max 0 (+ lsp-bridge-ui--index off 1 (- lsp-bridge-ui-count))
                                          (min (- lsp-bridge-ui--index off corr) lsp-bridge-ui--scroll))))))

(defun lsp-bridge-ui--candidates-popup (pos)
  "Show candidates popup at POS."
  (lsp-bridge-ui--update-scroll)
  (pcase-let* ((last (min (+ lsp-bridge-ui--scroll lsp-bridge-ui-count) lsp-bridge-ui--total))
               (bar (ceiling (* lsp-bridge-ui-count lsp-bridge-ui-count) lsp-bridge-ui--total))
               (lo (min (- lsp-bridge-ui-count bar 1) (floor (* lsp-bridge-ui-count lsp-bridge-ui--scroll) lsp-bridge-ui--total)))
               (`(,mf . ,acands) (lsp-bridge-ui--affixate (funcall lsp-bridge-ui--highlight
                                                                   (seq-subseq lsp-bridge-ui--candidates lsp-bridge-ui--scroll last))))
               (`(,pw ,width ,fcands) (lsp-bridge-ui--format-candidates acands))
               ;; Disable the left margin if a margin formatter is active.
               (lsp-bridge-ui-left-margin-width (if mf 0 lsp-bridge-ui-left-margin-width)))
    ;; Nonlinearity at the end and the beginning
    (when (/= lsp-bridge-ui--scroll 0)
      (setq lo (max 1 lo)))
    (when (/= last lsp-bridge-ui--total)
      (setq lo (min (- lsp-bridge-ui-count bar 2) lo)))
    (lsp-bridge-ui--popup-show (+ pos (length lsp-bridge-ui--base)) pw width fcands (- lsp-bridge-ui--index lsp-bridge-ui--scroll)
                               (and (> lsp-bridge-ui--total lsp-bridge-ui-count) lo) bar)))

(defun lsp-bridge-ui--preview-current (beg end)
  "Show current candidate as overlay given BEG and END."
  (when-let (cand (and lsp-bridge-ui-preview-current (>= lsp-bridge-ui--index 0)
                       (/= lsp-bridge-ui--index lsp-bridge-ui--preselect)
                       (nth lsp-bridge-ui--index lsp-bridge-ui--candidates)))
    (setq lsp-bridge-ui--preview-ov (make-overlay beg end nil t t))
    (overlay-put lsp-bridge-ui--preview-ov 'priority 1000)
    (overlay-put lsp-bridge-ui--preview-ov 'window (selected-window))
    (overlay-put lsp-bridge-ui--preview-ov
                 (if (= beg end) 'after-string 'display)
                 (concat lsp-bridge-ui--base cand))))

(defun lsp-bridge-ui--echo-refresh ()
  "Refresh echo message to prevent flicker during redisplay."
  (when lsp-bridge-ui--echo-timer
    (cancel-timer lsp-bridge-ui--echo-timer)
    (setq lsp-bridge-ui--echo-timer nil))
  (lsp-bridge-ui--echo-show lsp-bridge-ui--echo-message))

(defun lsp-bridge-ui--echo-show (&optional msg)
  "Show MSG in echo area."
  (when (or msg lsp-bridge-ui--echo-message)
    (setq msg (or msg "")
          lsp-bridge-ui--echo-message msg)
    (lsp-bridge-ui--message "%s" (if (text-property-not-all 0 (length msg) 'face nil msg)
                                     msg
                                   (propertize msg 'face 'lsp-bridge-ui-echo)))))

(defun lsp-bridge-ui--echo-documentation ()
  "Show documentation string of current candidate in echo area."
  (if-let* ((delay (if (consp lsp-bridge-ui-echo-documentation)
                       (funcall (if lsp-bridge-ui--echo-message #'cdr #'car)
                                lsp-bridge-ui-echo-documentation)
                     lsp-bridge-ui-echo-documentation))
            (fun (plist-get lsp-bridge-ui--extra :company-docsig))
            (cand (and (>= lsp-bridge-ui--index 0)
                       (nth lsp-bridge-ui--index lsp-bridge-ui--candidates))))
      (if (or (eq delay t) (<= delay 0))
          (lsp-bridge-ui--echo-show (funcall fun cand))
        (when lsp-bridge-ui--echo-timer (cancel-timer lsp-bridge-ui--echo-timer))
        (setq lsp-bridge-ui--echo-timer
              (run-at-time delay nil
                           (lambda ()
                             (lsp-bridge-ui--echo-show (funcall fun cand)))))
        (lsp-bridge-ui--echo-show))
    (lsp-bridge-ui--echo-show)))

(defun lsp-bridge-ui--update ()
  "Refresh Lsp-Bridge-Ui UI."
  (pcase-let* ((`(,beg ,end ,table ,pred) completion-in-region--data)
               (pt (- (point) beg))
               (str (buffer-substring-no-properties beg end))
               (initializing (not lsp-bridge-ui--input)))
    (lsp-bridge-ui--echo-refresh)
    (cond
     ;; XXX Guard against errors during candidate generation.
     ;; Turn off completion immediately if there are errors
     ;; For example dabbrev throws error "No dynamic expansion ... found".
     ;; TODO Report this as a bug? Are completion tables supposed to throw errors?
     ((condition-case err
          ;; Only recompute when input changed
          (unless (equal lsp-bridge-ui--input (cons str pt))
            (lsp-bridge-ui--update-candidates str pt table pred)
            nil)
        (error (lsp-bridge-ui-quit)
               (message "Lsp-Bridge-Ui completion error: %s" (error-message-string err)))))
     ;; 1) Initializing, no candidates => Quit. Happens during auto completion.
     ((and initializing (not lsp-bridge-ui--candidates))
      (lsp-bridge-ui-quit))
     ;; 2) Single exactly matching candidate and no further completion is possible.
     ((and (not (equal str ""))
           (equal (car lsp-bridge-ui--candidates) str) (not (cdr lsp-bridge-ui--candidates))
           (not (consp (completion-try-completion str table pred pt lsp-bridge-ui--metadata)))
           (or initializing lsp-bridge-ui-on-exact-match))
      ;; Quit directly when initializing. This happens during auto completion.
      (if (or initializing (eq lsp-bridge-ui-on-exact-match 'quit))
          (lsp-bridge-ui-quit)
        (lsp-bridge-ui--done str 'finished)))
     ;; 3) There exist candidates => Show candidates popup.
     (lsp-bridge-ui--candidates
      (lsp-bridge-ui--candidates-popup beg)
      (lsp-bridge-ui--preview-current beg end)
      (lsp-bridge-ui--echo-documentation)
      (redisplay 'force)) ;; XXX HACK Ensure that popup is redisplayed
     ;; 4) There are no candidates & lsp-bridge-ui-quit-no-match => Confirmation popup.
     ((and (not lsp-bridge-ui--candidates)
           (pcase-exhaustive lsp-bridge-ui-quit-no-match
             ('t nil)
             ('nil t)
             ('separator (seq-contains-p (car lsp-bridge-ui--input) lsp-bridge-ui-separator))))
      (lsp-bridge-ui--popup-show beg 0 8 '(#("No match" 0 8 (face italic))))
      (redisplay 'force)) ;; XXX HACK Ensure that popup is redisplayed
     (t (lsp-bridge-ui-quit)))))

(defun lsp-bridge-ui--pre-command ()
  "Insert selected candidate unless command is marked to continue completion."
  (when lsp-bridge-ui--preview-ov
    (delete-overlay lsp-bridge-ui--preview-ov)
    (setq lsp-bridge-ui--preview-ov nil))
  (when (and (eq lsp-bridge-ui-preview-current 'insert)
             (/= lsp-bridge-ui--index lsp-bridge-ui--preselect)
             ;; See the comment about `overriding-local-map' in `lsp-bridge-ui--post-command'.
             (not (or overriding-terminal-local-map
                      (lsp-bridge-ui--match-symbol-p lsp-bridge-ui-continue-commands this-command))))
    (lsp-bridge-ui--insert 'exact)))

(defun lsp-bridge-ui-insert-separator ()
  "Insert a separator character, inhibiting quit on completion boundary.
See `lsp-bridge-ui-separator' for more details."
  (interactive)
  (insert lsp-bridge-ui-separator))

(defun lsp-bridge-ui--post-command ()
  "Refresh Lsp-Bridge-Ui after last command."
  (or (pcase completion-in-region--data
        (`(,beg ,end . ,_)
         (when (let ((pt (point)))
                 (and (eq (marker-buffer beg) (current-buffer))
                      ;; Check ranges
                      (<= beg pt end)
                      (save-excursion
                        (goto-char beg)
                        (let ((inhibit-field-text-motion t))
                          (<= (line-beginning-position) pt (line-end-position))))
                      (or
                       ;; TODO We keep alive Lsp-Bridge-Ui if a `overriding-terminal-local-map' is
                       ;; installed, for example the `universal-argument-map'. It would be good to
                       ;; think about a better criterion instead. Unfortunately relying on
                       ;; `this-command' alone is not sufficient, since the value of `this-command'
                       ;; gets clobbered in the case of transient keymaps.
                       overriding-terminal-local-map
                       ;; Check if it is an explicitly listed continue command
                       (lsp-bridge-ui--match-symbol-p lsp-bridge-ui-continue-commands this-command)
                       (and
                        ;; Check for empty input
                        (or (not lsp-bridge-ui--input) (< beg end))
                        ;; Check separator or predicate
                        (or (not lsp-bridge-ui-quit-at-boundary)
                            (and (eq lsp-bridge-ui-quit-at-boundary 'separator)
                                 (or (eq this-command #'lsp-bridge-ui-insert-separator)
                                     ;; with separator, any further chars allowed
                                     (seq-contains-p (car lsp-bridge-ui--input) lsp-bridge-ui-separator)))
                            (funcall completion-in-region-mode--predicate))))))
           (lsp-bridge-ui--update)
           t)))
      (lsp-bridge-ui-quit)))

(defun lsp-bridge-ui--goto (index)
  "Go to candidate with INDEX."
  (setq lsp-bridge-ui--index (max lsp-bridge-ui--preselect (min index (1- lsp-bridge-ui--total)))))

(defun lsp-bridge-ui-next (&optional n)
  "Go forward N candidates."
  (interactive "p")
  (let ((index (+ lsp-bridge-ui--index (or n 1))))
    (lsp-bridge-ui--goto
     (cond
      ((not lsp-bridge-ui-cycle) index)
      ((= lsp-bridge-ui--total 0) -1)
      ((< lsp-bridge-ui--preselect 0) (1- (mod (1+ index) (1+ lsp-bridge-ui--total))))
      (t (mod index lsp-bridge-ui--total))))))

(defun lsp-bridge-ui-previous (&optional n)
  "Go backward N candidates."
  (interactive "p")
  (lsp-bridge-ui-next (- (or n 1))))

(defun lsp-bridge-ui-scroll-down (&optional n)
  "Go back by N pages."
  (interactive "p")
  (lsp-bridge-ui--goto (max 0 (- lsp-bridge-ui--index (* (or n 1) lsp-bridge-ui-count)))))

(defun lsp-bridge-ui-scroll-up (&optional n)
  "Go forward by N pages."
  (interactive "p")
  (lsp-bridge-ui-scroll-down (- (or n 1))))

(defun lsp-bridge-ui-first ()
  "Go to first candidate, or to the prompt when the first candidate is selected."
  (interactive)
  (lsp-bridge-ui--goto (if (> lsp-bridge-ui--index 0) 0 -1)))

(defun lsp-bridge-ui-last ()
  "Go to last candidate."
  (interactive)
  (lsp-bridge-ui--goto (1- lsp-bridge-ui--total)))

(defun lsp-bridge-ui-complete ()
  "Try to complete current input.
If a candidate is selected, insert it."
  (interactive)
  (pcase-let ((`(,beg ,end ,table ,pred) completion-in-region--data))
    (if (>= lsp-bridge-ui--index 0)
        ;; Continue completion with selected candidate
        (progn
          (lsp-bridge-ui--insert nil)
          ;; Exit with status 'finished if input is a valid match and no further
          ;; completion is possible. Furthermore treat the completion as
          ;; finished if we are at the end of a boundary, even if other longer
          ;; candidates would still match, since the user invoked `lsp-bridge-ui-complete'
          ;; with an explicitly selected candidate!
          (let ((newstr (buffer-substring-no-properties beg end)))
            (when (and (test-completion newstr table pred)
                       (or
                        (not (consp (completion-try-completion
                                     newstr table pred (length newstr)
                                     (completion-metadata newstr table pred))))
                        (equal (completion-boundaries newstr table pred "") '(0 . 0))))
              (lsp-bridge-ui--done newstr 'finished))))
      ;; Try to complete the current input string
      (let* ((pt (max 0 (- (point) beg)))
             (str (buffer-substring-no-properties beg end))
             (metadata (completion-metadata (substring str 0 pt) table pred)))
        (pcase (completion-try-completion str table pred pt metadata)
          ('t
           (goto-char end)
           (lsp-bridge-ui--done str 'finished))
          (`(,newstr . ,newpt)
           (unless (equal str newstr)
             ;; bug#55205: completion--replace removes properties!
             (completion--replace beg end (concat newstr)))
           (goto-char (+ beg newpt))
           ;; Exit with status 'finished if input is a valid match
           ;; and no further completion is possible.
           (when (and (test-completion newstr table pred)
                      (not (consp (completion-try-completion
                                   newstr table pred newpt
                                   (completion-metadata newstr table pred)))))
             (lsp-bridge-ui--done newstr 'finished))))))))

(defun lsp-bridge-ui--insert (status)
  "Insert current candidate, exit with STATUS if non-nil."
  (pcase-let* ((`(,beg ,end . ,_) completion-in-region--data)
               (str (buffer-substring-no-properties beg end)))
    ;; XXX There is a small bug here, depending on interpretation.
    ;; When completing "~/emacs/master/li|/calc" where "|" is the
    ;; cursor, then the candidate only includes the prefix
    ;; "~/emacs/master/lisp/", but not the suffix "/calc". Default
    ;; completion has the same problem when selecting in the
    ;; *Completions* buffer. See bug#48356.
    (setq str (concat lsp-bridge-ui--base (substring-no-properties
                                           (nth lsp-bridge-ui--index lsp-bridge-ui--candidates))))
    ;; bug#55205: completion--replace removes properties!
    (completion--replace beg end (concat str))
    (lsp-bridge-ui--goto -1) ;; Reset selection, but continue completion.
    (when status (lsp-bridge-ui--done str status)))) ;; Exit with status

(defun lsp-bridge-ui--done (str status)
  "Call the `:exit-function' with STR and STATUS and exit completion."
  (let ((exit (plist-get lsp-bridge-ui--extra :exit-function)))
    ;; For successfull completions, amalgamate undo operations,
    ;; such that completion can be undone in a single step.
    (undo-amalgamate-change-group lsp-bridge-ui--change-group)
    (lsp-bridge-ui-quit)
    ;; XXX Is the :exit-function handling sufficient?
    (when exit (funcall exit str status))))

(defun lsp-bridge-ui-insert ()
  "Insert current candidate.
Quit if no candidate is selected."
  (interactive)
  (if (>= lsp-bridge-ui--index 0)
      (lsp-bridge-ui--insert 'finished)
    (lsp-bridge-ui-quit)))

(defun lsp-bridge-ui--setup ()
  "Setup Lsp-Bridge-Ui completion state."
  (setq lsp-bridge-ui--extra completion-extra-properties)
  (completion-in-region-mode 1)
  (undo-boundary) ;; Necessary to support `lsp-bridge-ui-reset'
  (activate-change-group (setq lsp-bridge-ui--change-group (prepare-change-group)))
  (setcdr (assq #'completion-in-region-mode minor-mode-overriding-map-alist) lsp-bridge-ui-map)
  (add-hook 'pre-command-hook #'lsp-bridge-ui--pre-command nil 'local)
  (add-hook 'post-command-hook #'lsp-bridge-ui--post-command)
  ;; Disable default post-command handling, since we have our own
  ;; checks in `lsp-bridge-ui--post-command'.
  (remove-hook 'post-command-hook #'completion-in-region--postch)
  (let ((sym (make-symbol "lsp-bridge-ui--teardown"))
        (buf (current-buffer)))
    (fset sym (lambda ()
                ;; Ensure that the teardown runs in the correct buffer, if still alive.
                (unless completion-in-region-mode
                  (remove-hook 'completion-in-region-mode-hook sym)
                  (with-current-buffer (if (buffer-live-p buf) buf (current-buffer))
                    (lsp-bridge-ui--teardown)))))
    (add-hook 'completion-in-region-mode-hook sym)))

(defun lsp-bridge-ui--teardown ()
  "Teardown Lsp-Bridge-Ui."
  ;; Redisplay such that the input becomes immediately visible before the popup
  ;; hiding, which is slow (Issue #48). See also corresponding vertico#89.
  (redisplay)
  (lsp-bridge-ui--popup-hide)
  (remove-hook 'pre-command-hook #'lsp-bridge-ui--pre-command 'local)
  (remove-hook 'post-command-hook #'lsp-bridge-ui--post-command)
  (when lsp-bridge-ui--preview-ov (delete-overlay lsp-bridge-ui--preview-ov))
  (when lsp-bridge-ui--echo-timer (cancel-timer lsp-bridge-ui--echo-timer))
  (lsp-bridge-ui--echo-show)
  (accept-change-group lsp-bridge-ui--change-group)
  (mapc #'kill-local-variable lsp-bridge-ui--state-vars))

(defun lsp-bridge-ui--in-region (&rest args)
  "Lsp-Bridge-Ui completion in region function called with ARGS."
  ;; XXX We can get an endless loop when `completion-in-region-function' is set
  ;; globally to `lsp-bridge-ui--in-region'. This should never happen.
  (apply (if (lsp-bridge-ui--popup-support-p) #'lsp-bridge-ui--in-region-1
           (default-value 'completion-in-region-function))
         args))

(defun lsp-bridge-ui--in-region-1 (beg end table &optional pred)
  "Complete in region, see `completion-in-region' for BEG, END, TABLE, PRED."
  (barf-if-buffer-read-only)
  ;; Restart the completion. This can happen for example if C-M-/
  ;; (`dabbrev-completion') is pressed while the Lsp-Bridge-Ui popup is already open.
  (when completion-in-region-mode (lsp-bridge-ui-quit))
  (let* ((pt (max 0 (- (point) beg)))
         (str (buffer-substring-no-properties beg end))
         (before (substring str 0 pt))
         (metadata (completion-metadata before table pred))
         (exit (plist-get completion-extra-properties :exit-function))
         (threshold (completion--cycle-threshold metadata))
         (completion-in-region-mode-predicate
          (or completion-in-region-mode-predicate (lambda () t))))
    (pcase (completion-try-completion str table pred pt metadata)
      ('nil (lsp-bridge-ui--message "No match") nil)
      ('t (goto-char end)
          (lsp-bridge-ui--message "Sole match")
          (when exit (funcall exit str 'finished))
          t)
      (`(,newstr . ,newpt)
       (pcase-let ((`(,base ,candidates ,total . ,_)
                    (lsp-bridge-ui--recompute-candidates str pt table pred)))
         (unless (markerp beg) (setq beg (copy-marker beg)))
         (setq end (copy-marker end t)
               completion-in-region--data (list beg end table pred))
         (unless (equal str newstr)
           ;; bug#55205: completion--replace removes properties!
           (completion--replace beg end (concat newstr)))
         (goto-char (+ beg newpt))
         (if (= total 1)
             ;; If completion is finished and cannot be further completed,
             ;; return 'finished. Otherwise setup the Lsp-Bridge-Ui popup.
             (cond
              ((consp (completion-try-completion
                       newstr table pred newpt
                       (completion-metadata newstr table pred)))
               (lsp-bridge-ui--setup))
              (exit (funcall exit newstr 'finished)))
           (if (or (= total 0) (not threshold)
                   (and (not (eq threshold t)) (< threshold total)))
               (lsp-bridge-ui--setup)
             (lsp-bridge-ui--cycle-candidates total candidates (+ (length base) beg) end)
             ;; Do not show Lsp-Bridge-Ui when "trivially" cycling, i.e.,
             ;; when the completion is finished after the candidate.
             (unless (equal (completion-boundaries (car candidates) table pred "")
                            '(0 . 0))
               (lsp-bridge-ui--setup)))))
       t))))

(defun lsp-bridge-ui--message (&rest msg)
  "Show completion MSG."
  (let (message-log-max) (apply #'message msg)))

(defun lsp-bridge-ui--cycle-candidates (total cands beg end)
  "Cycle between TOTAL number of CANDS.
See `completion-in-region' for the arguments BEG, END, TABLE, PRED."
  (let* ((idx 0)
         (map (make-sparse-keymap))
         (replace (lambda ()
                    (interactive)
                    ;; bug#55205: completion--replace removes properties!
                    (completion--replace beg end (concat (nth idx cands)))
                    (lsp-bridge-ui--message "Cycling %d/%d..." (1+ idx) total)
                    (setq idx (mod (1+ idx) total))
                    (set-transient-map map))))
    (define-key map [remap completion-at-point] replace)
    (define-key map [remap lsp-bridge-ui-complete] replace)
    (define-key map (vector last-command-event) replace)
    (funcall replace)))

(defun lsp-bridge-ui--auto-complete (tick)
  "Initiate auto completion if TICK did not change."
  (setq lsp-bridge-ui--auto-timer nil)
  (when (and (not completion-in-region-mode) (equal tick (lsp-bridge-ui--auto-tick)))
    (pcase (while-no-input ;; Interruptible capf query
             (run-hook-wrapped 'completion-at-point-functions #'lsp-bridge-ui--capf-wrapper))
      (`(,fun ,beg ,end ,table . ,plist)
       (let ((completion-in-region-mode-predicate
              (lambda () (eq beg (car-safe (funcall fun)))))
             (completion-extra-properties plist))
         (setq completion-in-region--data
               (list (if (markerp beg) beg (copy-marker beg))
                     (copy-marker end t)
                     table
                     (plist-get plist :predicate)))
         (lsp-bridge-ui--setup)
         (lsp-bridge-ui--update))))))

(defun lsp-bridge-ui--auto-post-command ()
  "Post command hook which initiates auto completion."
  (when lsp-bridge-ui--auto-timer
    (cancel-timer lsp-bridge-ui--auto-timer)
    (setq lsp-bridge-ui--auto-timer nil))
  (when (and (not completion-in-region-mode)
             (not defining-kbd-macro)
             (lsp-bridge-ui--match-symbol-p lsp-bridge-ui-auto-commands this-command)
             (lsp-bridge-ui--popup-support-p))
    ;; NOTE: Do not use idle timer since this leads to unacceptable slowdowns,
    ;; in particular if flyspell-mode is enabled.
    (setq lsp-bridge-ui--auto-timer
          (run-at-time lsp-bridge-ui-auto-delay nil
                       #'lsp-bridge-ui--auto-complete (lsp-bridge-ui--auto-tick)))))

(defun lsp-bridge-ui--auto-tick ()
  "Return the current tick/status of the buffer.
Auto completion is only performed if the tick did not change."
  (list (current-buffer) (buffer-chars-modified-tick) (point)))

;;;###autoload
(define-minor-mode lsp-bridge-ui-mode
  "Completion Overlay Region FUnction."
  :global nil :group 'lsp-bridge-ui
  (cond
   (lsp-bridge-ui-mode
    ;; FIXME: Install advice which fixes `completion--capf-wrapper', such that
    ;; it respects the completion styles for non-exclusive capfs. See FIXME in
    ;; the `completion--capf-wrapper' function in minibuffer.el, where the
    ;; issue has been mentioned. We never uninstall this advice since the
    ;; advice is active *globally*.
    (advice-add #'completion--capf-wrapper :around #'lsp-bridge-ui--capf-wrapper-advice)
    (advice-add #'eldoc-display-message-no-interference-p :before-while #'lsp-bridge-ui--allow-eldoc)
    (and lsp-bridge-ui-auto (add-hook 'post-command-hook #'lsp-bridge-ui--auto-post-command nil 'local))
    (setq-local completion-in-region-function #'lsp-bridge-ui--in-region))
   (t
    (remove-hook 'post-command-hook #'lsp-bridge-ui--auto-post-command 'local)
    (kill-local-variable 'completion-in-region-function))))

(defun lsp-bridge-ui--capf-wrapper (fun &optional prefix)
  "Wrapper for `completion-at-point' FUN.
The wrapper determines if the capf is applicable at the current position
and performs sanity checking on the returned result. PREFIX is a prefix
length override, set to t for manual completion."
  (pcase (funcall fun)
    ((and res `(,beg ,end ,table . ,plist))
     (and (integer-or-marker-p beg) ;; Valid capf result
          (<= beg (point) end)      ;; Sanity checking
          ;; When auto completing, check the prefix length!
          (let ((len (or prefix
                         (plist-get plist :company-prefix-length)
                         (- (point) beg))))
            (or (eq len t) (>= len lsp-bridge-ui-auto-prefix)))
          ;; For non-exclusive capfs, check for valid completion.
          (or (not (eq 'no (plist-get plist :exclusive)))
              (let* ((str (buffer-substring-no-properties beg end))
                     (pt (- (point) beg))
                     (pred (plist-get plist :predicate))
                     (md (completion-metadata (substring str 0 pt) table pred)))
                ;; We use `completion-try-completion' to check if there are
                ;; completions. The upstream `completion--capf-wrapper' uses
                ;; `try-completion' which is incorrect since it only checks for
                ;; prefix completions.
                (completion-try-completion str table pred pt md)))
          (cons fun res)))))

(defun lsp-bridge-ui--capf-wrapper-advice (orig fun which)
  "Around advice for `completion--capf-wrapper'.
The ORIG function takes the FUN and WHICH arguments."
  (if lsp-bridge-ui-mode (lsp-bridge-ui--capf-wrapper fun t) (funcall orig fun which)))

;;;###autoload
(define-globalized-minor-mode global-lsp-bridge-ui-mode lsp-bridge-ui-mode lsp-bridge-ui--on :group 'lsp-bridge-ui)

(defun lsp-bridge-ui--on ()
  "Turn `lsp-bridge-ui-mode' on."
  (unless (or noninteractive
              (eq (aref (buffer-name) 0) ?\s)
              (memq major-mode lsp-bridge-ui-excluded-modes))
    (lsp-bridge-ui-mode 1)))

(defun lsp-bridge-ui--allow-eldoc ()
  "Return non-nil if Lsp-Bridge-Ui is currently not active."
  (not (and lsp-bridge-ui-mode completion-in-region-mode)))

;; Emacs 28: Do not show Lsp-Bridge-Ui commands with M-X
(dolist (sym '(lsp-bridge-ui-next lsp-bridge-ui-previous lsp-bridge-ui-first lsp-bridge-ui-last lsp-bridge-ui-quit lsp-bridge-ui-reset
                                  lsp-bridge-ui-complete lsp-bridge-ui-insert lsp-bridge-ui-scroll-up lsp-bridge-ui-scroll-down
                                  lsp-bridge-ui-insert-separator))
  (put sym 'completion-predicate #'ignore))


(defvar lsp-bridge-ui-icon--cache nil
  "The cache of styled and padded label (text or icon).
An alist.")

(defun lsp-bridge-ui-icon-reset-cache ()
  "Remove all cached icons from `lsp-bridge-ui-icon-mapping'."
  (interactive)
  (setq lsp-bridge-ui-icon--cache nil))

(defvar lsp-bridge-ui-icon--icons
  `((unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
    (text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
    (method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
    (function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
    (fun . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
    (constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
    (ctor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
    (field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
    (variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
    (var . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
    (class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
    (interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
    (i/f . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
    (module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
    (mod . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
    (property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
    (prop . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
    (unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
    (value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
    (enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
    (keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
    (k/w . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
    (snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
    (sn . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
    (color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
    (file . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
    (reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
    (ref . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
    (folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
    (dir . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
    (enum-member . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
    (enummember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
    (member . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
    (constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
    (const . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
    (struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
    (event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
    (operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
    (op . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
    (type-parameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
    (param . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
    (template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15))
    (t . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))))

(defsubst lsp-bridge-ui-icon--metadata-get (metadata type-name)
  (or
   (plist-get completion-extra-properties (intern (format ":%s" type-name)))
   (cdr (assq (intern type-name) metadata))))

(defun lsp-bridge-ui-icon-formatted (kind)
  "Format icon kind with all-the-icons"
  (or (alist-get kind lsp-bridge-ui-icon--cache)
      (let ((map (assq kind lsp-bridge-ui-icon--icons)))
        (let*  ((icon (if map
                          (cdr map)
                        (cdr (assq t lsp-bridge-ui-icon--icons))))
                (half (/ (default-font-width) 2))
                (pad (propertize " " 'display `(space :width (,half))))
                (disp (concat pad icon pad)))
          (setf (alist-get kind lsp-bridge-ui-icon--cache) disp)
          disp))))

(defun lsp-bridge-ui-icon-margin-formatter (metadata)
  "Return a margin-formatter function which produces kind icons.
METADATA is the completion metadata supplied by the caller (see
info node `(elisp)Programmed Completion').  To use, add this
function to the relevant margin-formatters list."
  (if-let ((kind-func (lsp-bridge-ui-icon--metadata-get metadata "company-kind")))
      (lambda (cand)
        (if-let ((kind (funcall kind-func cand)))
            (lsp-bridge-ui-icon-formatted kind)
          (lsp-bridge-ui-icon-formatted t))))) ;; as a backup

(add-to-list 'lsp-bridge-ui-margin-formatters #'lsp-bridge-ui-icon-margin-formatter)

(provide 'lsp-bridge-ui)

;;; lsp-bridge-ui.el ends here
