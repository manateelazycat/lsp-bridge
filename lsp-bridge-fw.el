;;; lsp-bridge-fw.el --- LSP bridge  -*- lexical-binding: t; -*-

;; Filename: lsp-bridge-fw.el
;; Description: LSP bridge
;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Daniel Mendler, Andy Stewart, all rights reserved.
;; Created: 2022-05-01 14:10:12
;; Version: 0.5
;; Last-Updated: Wed May 11 02:44:16 2022 (-0400)
;;           By: Mingde (Matthew) Zeng
;; URL: https://github.com/manateelazycat/lsp-bridge-fw
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
;; Lsp-Bridge-Fw
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
;;      M-x customize-group RET lsp-bridge-fw RET
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

(defgroup lsp-bridge-fw nil
  "Completion Overlay Region FUnction."
  :group 'convenience
  :prefix "lsp-bridge-fw-")

(defcustom lsp-bridge-fw-count 10
  "Maximal number of candidates to show."
  :type 'integer)

(defcustom lsp-bridge-fw-scroll-margin 2
  "Number of lines at the top and bottom when scrolling.
The value should lie between 0 and lsp-bridge-fw-count/2."
  :type 'integer)

(defcustom lsp-bridge-fw-min-width 15
  "Popup minimum width in characters."
  :type 'integer)

(defcustom lsp-bridge-fw-max-width 100
  "Popup maximum width in characters."
  :type 'integer)

(defcustom lsp-bridge-fw-cycle nil
  "Enable cycling for `lsp-bridge-fw-next' and `lsp-bridge-fw-previous'."
  :type 'boolean)

(defcustom lsp-bridge-fw-on-exact-match 'insert
  "Configure how a single exact match should be handled."
  :type '(choice (const insert) (const quit) (const nil)))

(defcustom lsp-bridge-fw-continue-commands
  ;; nil is undefined command
  '(nil ignore universal-argument universal-argument-more digit-argument
        "\\`lsp-bridge-fw-" "\\`scroll-other-window")
  "Continue Lsp-Bridge-Fw completion after executing these commands."
  :type '(repeat (choice regexp symbol)))

(defcustom lsp-bridge-fw-preview-current 'insert
  "Preview currently selected candidate.
If the variable has the value `insert', the candidate is automatically
inserted on further input."
  :type '(choice boolean (const insert)))

(defcustom lsp-bridge-fw-preselect-first t
  "Preselect first candidate."
  :type 'boolean)

(defcustom lsp-bridge-fw-separator ?\s
  "Component separator character.
The character used for separating components in the input. The presence
of this separator character will inhibit quitting at completion
boundaries, so that any further characters can be entered. To enter the
first separator character, call `lsp-bridge-fw-insert-separator' (bound to M-SPC
by default). Useful for multi-component completion styles such as
Orderless."
  :type 'character)

(defcustom lsp-bridge-fw-quit-at-boundary 'separator
  "Automatically quit at completion boundary.
nil: Never quit at completion boundary.
t: Always quit at completion boundary.
separator: Quit at boundary if no `lsp-bridge-fw-separator' has been inserted."
  :type '(choice boolean (const separator)))

(defcustom lsp-bridge-fw-quit-no-match 'separator
  "Automatically quit if no matching candidate is found.
nil: Stay alive even if there is no match.
t: Quit if there is no match.
separator: Only stay alive if there is no match and
`lsp-bridge-fw-separator' has been inserted."
  :type '(choice boolean (const separator)))

(defcustom lsp-bridge-fw-excluded-modes nil
  "List of modes excluded by `global-lsp-bridge-fw-mode'."
  :type '(repeat symbol))

(defcustom lsp-bridge-fw-left-margin-width 0.5
  "Width of the left margin in units of the character width."
  :type 'float)

(defcustom lsp-bridge-fw-right-margin-width 0.5
  "Width of the right margin in units of the character width."
  :type 'float)

(defcustom lsp-bridge-fw-bar-width 0.2
  "Width of the bar in units of the character width."
  :type 'float)

(defcustom lsp-bridge-fw-echo-documentation '(1.0 . 0.2)
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

(defcustom lsp-bridge-fw-margin-formatters nil
  "Registry for margin formatter functions.
Each function of the list is called with the completion metadata as
argument until an appropriate formatter is found. The function should
return a formatter function, which takes the candidate string and must
return a string, possibly an icon."
  :type 'hook)

(defcustom lsp-bridge-fw-sort-function #'lsp-bridge-fw-sort-length-alpha
  "Default sorting function, used if no `display-sort-function' is specified."
  :type `(choice
          (const :tag "No sorting" nil)
          (const :tag "By length and alpha" ,#'lsp-bridge-fw-sort-length-alpha)
          (function :tag "Custom function")))

(defcustom lsp-bridge-fw-sort-override-function nil
  "Override sort function which overrides the `display-sort-function'."
  :type '(choice (const nil) function))

(defcustom lsp-bridge-fw-auto-prefix 3
  "Minimum length of prefix for auto completion.
The completion backend can override this with
:company-prefix-length."
  :type 'integer)

(defcustom lsp-bridge-fw-auto-delay 0.2
  "Delay for auto completion."
  :type 'float)

(defcustom lsp-bridge-fw-auto-commands
  '("self-insert-command\\'"
    c-electric-colon c-electric-lt-gt c-electric-slash c-scope-operator)
  "Commands which initiate auto completion."
  :type '(repeat (choice regexp symbol)))

(defcustom lsp-bridge-fw-auto nil
  "Enable auto completion."
  :type 'boolean)

(defgroup lsp-bridge-fw-faces nil
  "Faces used by Lsp-Bridge-Fw."
  :group 'lsp-bridge-fw
  :group 'faces)

(defface lsp-bridge-fw-default
  '((((class color) (min-colors 88) (background dark)) :background "#191a1b")
    (((class color) (min-colors 88) (background light)) :background "#f0f0f0")
    (t :background "gray"))
  "Default face, foreground and background colors used for the popup.")

(defface lsp-bridge-fw-current
  '((((class color) (min-colors 88) (background dark))
     :background "#00415e" :foreground "white")
    (((class color) (min-colors 88) (background light))
     :background "#c0efff" :foreground "black")
    (t :background "blue" :foreground "white"))
  "Face used to highlight the currently selected candidate.")

(defface lsp-bridge-fw-bar
  '((((class color) (min-colors 88) (background dark)) :background "#a8a8a8")
    (((class color) (min-colors 88) (background light)) :background "#505050")
    (t :background "gray"))
  "The background color is used for the scrollbar indicator.")

(defface lsp-bridge-fw-border
  '((((class color) (min-colors 88) (background dark)) :background "#323232")
    (((class color) (min-colors 88) (background light)) :background "#d7d7d7")
    (t :background "gray"))
  "The background color used for the thin border.")

(defface lsp-bridge-fw-echo
  '((t :inherit completions-annotations))
  "Face used for echo area messages.")

(defface lsp-bridge-fw-annotations
  '((t :inherit completions-annotations))
  "Face used for annotations.")

(defface lsp-bridge-fw-deprecated
  '((t :inherit shadow :strike-through t))
  "Face used for deprecated candidates.")

(defvar lsp-bridge-fw-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap beginning-of-buffer] #'lsp-bridge-fw-first)
    (define-key map [remap end-of-buffer] #'lsp-bridge-fw-last)
    (define-key map [remap scroll-down-command] #'lsp-bridge-fw-scroll-down)
    (define-key map [remap scroll-up-command] #'lsp-bridge-fw-scroll-up)
    (define-key map [remap next-line] #'lsp-bridge-fw-next)
    (define-key map [remap previous-line] #'lsp-bridge-fw-previous)
    (define-key map [remap completion-at-point] #'lsp-bridge-fw-complete)
    (define-key map [down] #'lsp-bridge-fw-next)
    (define-key map [up] #'lsp-bridge-fw-previous)
    (define-key map [remap keyboard-escape-quit] #'lsp-bridge-fw-reset)
    ;; XXX [tab] is bound because of org-mode
    ;; The binding should be removed from org-mode-map.
    (define-key map [tab] #'lsp-bridge-fw-complete)
    (define-key map "\M-n" #'lsp-bridge-fw-next)
    (define-key map "\M-p" #'lsp-bridge-fw-previous)
    (define-key map "\C-g" #'lsp-bridge-fw-quit)
    (define-key map "\r" #'lsp-bridge-fw-insert)
    (define-key map "\t" #'lsp-bridge-fw-complete)
    (define-key map "\M- " #'lsp-bridge-fw-insert-separator)
    map)
  "Lsp-Bridge-Fw keymap used when popup is shown.")

(defvar lsp-bridge-fw--auto-timer nil
  "Auto completion timer.")

(defvar-local lsp-bridge-fw--candidates nil
  "List of candidates.")

(defvar-local lsp-bridge-fw--metadata nil
  "Completion metadata.")

(defvar-local lsp-bridge-fw--base ""
  "Base string, which is concatenated with the candidate.")

(defvar-local lsp-bridge-fw--total 0
  "Length of the candidate list `lsp-bridge-fw--candidates'.")

(defvar-local lsp-bridge-fw--highlight #'identity
  "Deferred candidate highlighting function.")

(defvar-local lsp-bridge-fw--index -1
  "Index of current candidate or negative for prompt selection.")

(defvar-local lsp-bridge-fw--preselect -1
  "Index of preselected candidate, negative for prompt selection.")

(defvar-local lsp-bridge-fw--scroll 0
  "Scroll position.")

(defvar-local lsp-bridge-fw--input nil
  "Cons of last prompt contents and point.")

(defvar-local lsp-bridge-fw--preview-ov nil
  "Current candidate overlay.")

(defvar-local lsp-bridge-fw--extra nil
  "Extra completion properties.")

(defvar-local lsp-bridge-fw--change-group nil
  "Undo change group.")

(defvar-local lsp-bridge-fw--echo-timer nil
  "Echo area message timer.")

(defvar-local lsp-bridge-fw--echo-message nil
  "Last echo message.")

(defvar lsp-bridge-fw--frame nil
  "Popup frame.")

(defconst lsp-bridge-fw--state-vars
  '(lsp-bridge-fw--base
    lsp-bridge-fw--candidates
    lsp-bridge-fw--highlight
    lsp-bridge-fw--index
    lsp-bridge-fw--preselect
    lsp-bridge-fw--scroll
    lsp-bridge-fw--input
    lsp-bridge-fw--total
    lsp-bridge-fw--preview-ov
    lsp-bridge-fw--extra
    lsp-bridge-fw--echo-timer
    lsp-bridge-fw--echo-message
    lsp-bridge-fw--change-group
    lsp-bridge-fw--metadata)
  "Buffer-local state variables used by Lsp-Bridge-Fw.")

(defvar lsp-bridge-fw--frame-parameters
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

(defvar lsp-bridge-fw--buffer-parameters
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

(defvar lsp-bridge-fw--mouse-ignore-map
  (let ((map (make-sparse-keymap)))
    (dotimes (i 7)
      (dolist (k '(mouse down-mouse drag-mouse double-mouse triple-mouse))
        (define-key map (vector (intern (format "%s-%s" k (1+ i)))) #'ignore)))
    map)
  "Ignore all mouse clicks.")

(defun lsp-bridge-fw--make-buffer (content)
  "Create lsp-bridge-fw buffer with CONTENT."
  (let ((fr face-remapping-alist)
        (ls line-spacing)
        (buffer (get-buffer-create " *lsp-bridge-fw*")))
    (with-current-buffer buffer
;;; XXX HACK install mouse ignore map
      (use-local-map lsp-bridge-fw--mouse-ignore-map)
      (dolist (var lsp-bridge-fw--buffer-parameters)
        (set (make-local-variable (car var)) (cdr var)))
      (setq-local face-remapping-alist (copy-tree fr)
                  line-spacing ls)
      (cl-pushnew 'lsp-bridge-fw-default (alist-get 'default face-remapping-alist))
      (let ((inhibit-modification-hooks t)
            (inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (goto-char (point-min))))
    buffer))

;; Function adapted from posframe.el by tumashu
(defvar x-gtk-resize-child-frames) ;; Not present on non-gtk builds
(defun lsp-bridge-fw--make-frame (x y width height content)
  "Show child frame at X/Y with WIDTH/HEIGHT and CONTENT."
  (let* ((window-min-height 1)
         (window-min-width 1)
         (x-gtk-resize-child-frames
          (let ((case-fold-search t))
            (and
             ;; XXX HACK to fix resizing on gtk3/gnome taken from posframe.el
             ;; More information:
             ;; * https://github.com/minad/lsp-bridge-fw/issues/17
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
         (border (alist-get 'child-frame-border-width lsp-bridge-fw--frame-parameters))
         (x (max border (min (+ (car edge) x (- border))
                             (- (frame-pixel-width) width))))
         (yb (+ (cadr edge) (window-tab-line-height) y ch))
         (y (if (> (+ yb (* lsp-bridge-fw-count ch) ch ch) (frame-pixel-height))
                (- yb height ch 1)
              yb))
         (buffer (lsp-bridge-fw--make-buffer content))
         (parent (window-frame)))
    (unless (and (frame-live-p lsp-bridge-fw--frame)
                 (eq (frame-parent lsp-bridge-fw--frame) parent))
      (when lsp-bridge-fw--frame (delete-frame lsp-bridge-fw--frame))
      (setq lsp-bridge-fw--frame (make-frame
                                  `((parent-frame . ,parent)
                                    (minibuffer . ,(minibuffer-window parent))
                                    ;; Set `internal-border-width' for Emacs 27
                                    (internal-border-width . ,border)
                                    ,@lsp-bridge-fw--frame-parameters))))
    ;; XXX HACK Setting the same frame-parameter/face-background is not a nop.
    ;; Check explicitly before applying the setting. Without the check, the
    ;; frame flickers on Mac.
    ;; XXX HACK We have to apply the face background before adjusting the frame
    ;; parameter, otherwise the border is not updated (BUG!).
    (let* ((face (if (facep 'child-frame-border) 'child-frame-border 'internal-border))
           (new (face-attribute 'lsp-bridge-fw-border :background nil 'default)))
      (unless (equal (face-attribute face :background lsp-bridge-fw--frame 'default) new)
        (set-face-background face new lsp-bridge-fw--frame)))
    (let ((new (face-attribute 'lsp-bridge-fw-default :background nil 'default)))
      (unless (equal (frame-parameter lsp-bridge-fw--frame 'background-color) new)
        (set-frame-parameter lsp-bridge-fw--frame 'background-color new)))
    (let ((win (frame-root-window lsp-bridge-fw--frame)))
      (set-window-buffer win buffer)
      ;; Mark window as dedicated to prevent frame reuse (#60)
      (set-window-dedicated-p win t))
    (set-frame-size lsp-bridge-fw--frame width height t)
    (if (frame-visible-p lsp-bridge-fw--frame)
        ;; XXX HACK Avoid flicker when frame is already visible.
        ;; Redisplay, wait for resize and then move the frame.
        (unless (equal (frame-position lsp-bridge-fw--frame) (cons x y))
          (redisplay 'force)
          (sleep-for 0.01)
          (set-frame-position lsp-bridge-fw--frame x y))
      ;; XXX HACK: Force redisplay, otherwise the popup sometimes does not
      ;; display content.
      (set-frame-position lsp-bridge-fw--frame x y)
      (redisplay 'force)
      (make-frame-visible lsp-bridge-fw--frame))
    (redirect-frame-focus lsp-bridge-fw--frame parent)))

(defun lsp-bridge-fw--popup-show (pos off width lines &optional curr lo bar)
  "Show LINES as popup at POS - OFF.
WIDTH is the width of the popup.
The current candidate CURR is highlighted.
A scroll bar is displayed from LO to LO+BAR."
  (let* ((ch (default-line-height))
         (cw (default-font-width))
         (ml (ceiling (* cw lsp-bridge-fw-left-margin-width)))
         (mr (ceiling (* cw lsp-bridge-fw-right-margin-width)))
         (bw (ceiling (min mr (* cw lsp-bridge-fw-bar-width))))
         (marginl (and (> ml 0) (propertize " " 'display `(space :width (,ml)))))
         (marginr (and (> mr 0) (propertize " " 'display `(space :align-to right))))
         (sbar (when (> bw 0)
                 (concat (propertize " " 'display `(space :align-to (- right (,mr))))
                         (propertize " " 'display `(space :width (,(- mr bw))))
                         (propertize " " 'face 'lsp-bridge-fw-bar 'display `(space :width (,bw))))))
         (row 0)
         (pos (posn-x-y (posn-at-point pos)))
         (x (or (car pos) 0))
         (y (or (cdr pos) 0)))
    (lsp-bridge-fw--make-frame
     (- x ml (* cw off)) y
     (+ (* width cw) ml mr) (* (length lines) ch)
     (mapconcat (lambda (line)
                  (let ((str (concat marginl line
                                     (if (and lo (<= lo row (+ lo bar))) sbar marginr))))
                    (when (eq row curr)
                      (add-face-text-property
                       0 (length str) 'lsp-bridge-fw-current 'append str))
                    (setq row (1+ row))
                    str))
                lines "\n"))))

(defun lsp-bridge-fw--popup-hide ()
  "Hide Lsp-Bridge-Fw popup."
  (when (frame-live-p lsp-bridge-fw--frame)
    (make-frame-invisible lsp-bridge-fw--frame)
    (with-current-buffer (window-buffer (frame-root-window lsp-bridge-fw--frame))
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(defun lsp-bridge-fw--popup-support-p ()
  "Return non-nil if child frames are supported."
  (display-graphic-p))

(defun lsp-bridge-fw--move-to-front (elem list)
  "Move ELEM to front of LIST."
  (if-let (found (member elem list))
      (let ((head (list (car found))))
        (nconc head (delq (setcar found nil) list)))
    list))

;; bug#47711: Deferred highlighting for `completion-all-completions'
;; XXX There is one complication: `completion--twq-all' already adds
;; `completions-common-part'.
(defun lsp-bridge-fw--all-completions (&rest args)
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

(defun lsp-bridge-fw--sort-predicate (x y)
  "Sorting predicate which compares X and Y."
  (or (< (length x) (length y)) (and (= (length x) (length y)) (string< x y))))

(defun lsp-bridge-fw-sort-length-alpha (list)
  "Sort LIST by length and alphabetically."
  (sort list #'lsp-bridge-fw--sort-predicate))

(defmacro lsp-bridge-fw--partition! (list form)
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

(defun lsp-bridge-fw--move-prefix-candidates-to-front (field candidates)
  "Move CANDIDATES which match prefix of FIELD to the beginning."
  (let* ((word (substring field 0
                          (seq-position field lsp-bridge-fw-separator)))
         (len (length word)))
    (lsp-bridge-fw--partition!
     candidates
     (and (>= (length it) len)
          (eq t (compare-strings word 0 len it 0 len
                                 completion-ignore-case))))))

(defun lsp-bridge-fw--filter-files (files)
  "Filter FILES by `completion-ignored-extensions'."
  (let ((re (concat "\\(?:\\(?:\\`\\|/\\)\\.\\.?/\\|"
                    (regexp-opt completion-ignored-extensions)
                    "\\)\\'")))
    (or (seq-remove (lambda (x) (string-match-p re x)) files) files)))

(defun lsp-bridge-fw--sort-function ()
  "Return the sorting function."
  (or lsp-bridge-fw-sort-override-function
      (lsp-bridge-fw--metadata-get 'display-sort-function)
      lsp-bridge-fw-sort-function))

(defun lsp-bridge-fw--recompute-candidates (str pt table pred)
  "Recompute candidates from STR, PT, TABLE and PRED."
  (pcase-let* ((before (substring str 0 pt))
               (after (substring str pt))
               (lsp-bridge-fw--metadata (completion-metadata before table pred))
               ;; bug#47678: `completion-boundaries` fails for `partial-completion`
               ;; if the cursor is moved between the slashes of "~//".
               ;; See also vertico.el which has the same issue.
               (bounds (or (condition-case nil
                               (completion-boundaries before table pred after)
                             (t (cons 0 (length after))))))
               (field (substring str (car bounds) (+ pt (cdr bounds))))
               (completing-file (eq (lsp-bridge-fw--metadata-get 'category) 'file))
               (`(,all . ,hl) (lsp-bridge-fw--all-completions str table pred pt lsp-bridge-fw--metadata))
               (base (or (when-let (z (last all)) (prog1 (cdr z) (setcdr z nil))) 0))
               (lsp-bridge-fw--base (substring str 0 base)))
    ;; Filter the ignored file extensions. We cannot use modified predicate for
    ;; this filtering, since this breaks the special casing in the
    ;; `completion-file-name-table' for `file-exists-p' and `file-directory-p'.
    (when completing-file (setq all (lsp-bridge-fw--filter-files all)))
    (setq all (delete-consecutive-dups (funcall (or (lsp-bridge-fw--sort-function) #'identity) all)))
    (setq all (lsp-bridge-fw--move-prefix-candidates-to-front field all))
    (when (and completing-file (not (string-suffix-p "/" field)))
      (setq all (lsp-bridge-fw--move-to-front (concat field "/") all)))
    (setq all (lsp-bridge-fw--move-to-front field all))
    (list lsp-bridge-fw--base all (length all) hl lsp-bridge-fw--metadata
          ;; Select the prompt when the input is a valid completion
          ;; and if it is not equal to the first candidate.
          (if (or (not lsp-bridge-fw-preselect-first) (not all)
                  (and (not (equal field (car all)))
                       (not (and completing-file (equal (concat field "/") (car all))))
                       (test-completion str table pred)))
              -1 0))))

(defun lsp-bridge-fw--update-candidates (str pt table pred)
  "Update candidates from STR, PT, TABLE and PRED."
  ;; Redisplay such that the input becomes immediately visible before the
  ;; expensive candidate recomputation is performed (Issue #48). See also
  ;; corresponding vertico#89.
  (redisplay)
  (pcase
      ;; Bind non-essential=t to prevent Tramp from opening new connections,
      ;; without the user explicitly requesting it via M-TAB.
      (let ((non-essential t))
        (while-no-input (lsp-bridge-fw--recompute-candidates str pt table pred)))
    ('nil (keyboard-quit))
    (`(,base ,candidates ,total ,hl ,metadata ,preselect)
     (setq lsp-bridge-fw--input (cons str pt)
           lsp-bridge-fw--candidates candidates
           lsp-bridge-fw--base base
           lsp-bridge-fw--total total
           lsp-bridge-fw--preselect preselect
           lsp-bridge-fw--index preselect
           lsp-bridge-fw--highlight hl
           lsp-bridge-fw--metadata metadata))))

(defun lsp-bridge-fw--match-symbol-p (pattern sym)
  "Return non-nil if SYM is matching an element of the PATTERN list."
  (and (symbolp sym)
       (cl-loop for x in pattern
                thereis (if (symbolp x)
                            (eq sym x)
                          (string-match-p x (symbol-name sym))))))

(defun lsp-bridge-fw-quit ()
  "Quit Lsp-Bridge-Fw completion."
  (interactive)
  (completion-in-region-mode -1))

(defun lsp-bridge-fw-reset ()
  "Reset Lsp-Bridge-Fw completion.
This command can be executed multiple times by hammering the ESC key. If a
candidate is selected, unselect the candidate. Otherwise reset the input. If
there hasn't been any input, then quit."
  (interactive)
  (if (/= lsp-bridge-fw--index lsp-bridge-fw--preselect)
      (progn
        (lsp-bridge-fw--goto -1)
        (setq this-command #'lsp-bridge-fw-first))
    ;; Cancel all changes and start new change group.
    (cancel-change-group lsp-bridge-fw--change-group)
    (activate-change-group (setq lsp-bridge-fw--change-group (prepare-change-group)))
    (when (eq last-command #'lsp-bridge-fw-reset) (lsp-bridge-fw-quit))))

(defun lsp-bridge-fw--affixate (cands)
  "Annotate CANDS with annotation function."
  (setq cands
        (if-let (aff (or (lsp-bridge-fw--metadata-get 'affixation-function)
                         (plist-get lsp-bridge-fw--extra :affixation-function)))
            (funcall aff cands)
          (if-let (ann (or (lsp-bridge-fw--metadata-get 'annotation-function)
                           (plist-get lsp-bridge-fw--extra :annotation-function)))
              (cl-loop for cand in cands collect
                       (let ((suffix (or (funcall ann cand) "")))
                         ;; The default completion UI adds the
                         ;; `completions-annotations' face if no other faces are
                         ;; present. We use a custom `lsp-bridge-fw-annotations' face to
                         ;; allow further styling which fits better for popups.
                         (unless (text-property-not-all 0 (length suffix) 'face nil suffix)
                           (setq suffix (propertize suffix 'face 'lsp-bridge-fw-annotations)))
                         (list cand "" suffix)))
            (cl-loop for cand in cands collect (list cand "" "")))))
  (let* ((dep (plist-get lsp-bridge-fw--extra :company-deprecated))
         (completion-extra-properties lsp-bridge-fw--extra)
         (mf (run-hook-with-args-until-success 'lsp-bridge-fw-margin-formatters lsp-bridge-fw--metadata)))
    (cl-loop for x in cands for (c . _) = x do
             (when mf
               (setf (cadr x) (funcall mf c)))
             (when (and dep (funcall dep c))
               (setcar x (setq c (substring c)))
               (add-face-text-property 0 (length c) 'lsp-bridge-fw-deprecated 'append c)))
    (cons mf cands)))

(defun lsp-bridge-fw--metadata-get (prop)
  "Return PROP from completion metadata."
  ;; Note: Do not use `completion-metadata-get' in order to avoid Marginalia.
  ;; The Marginalia annotators are too heavy for the Lsp-Bridge-Fw popup!
  (cdr (assq prop lsp-bridge-fw--metadata)))

(defun lsp-bridge-fw--format-candidates (cands)
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
         (max-width (min lsp-bridge-fw-max-width (- (frame-width) 4))))
    (when (> width max-width)
      (setq sw (max 0 (- max-width pw cw))
            width (+ pw cw sw)))
    (when (< width lsp-bridge-fw-min-width)
      (setq cw (+ cw (- lsp-bridge-fw-min-width width))
            width lsp-bridge-fw-min-width))
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

(defun lsp-bridge-fw--update-scroll ()
  "Update scroll position."
  (let ((off (max (min lsp-bridge-fw-scroll-margin (/ lsp-bridge-fw-count 2)) 0))
        (corr (if (= lsp-bridge-fw-scroll-margin (/ lsp-bridge-fw-count 2)) (1- (mod lsp-bridge-fw-count 2)) 0)))
    (setq lsp-bridge-fw--scroll (min (max 0 (- lsp-bridge-fw--total lsp-bridge-fw-count))
                                     (max 0 (+ lsp-bridge-fw--index off 1 (- lsp-bridge-fw-count))
                                          (min (- lsp-bridge-fw--index off corr) lsp-bridge-fw--scroll))))))

(defun lsp-bridge-fw--candidates-popup (pos)
  "Show candidates popup at POS."
  (lsp-bridge-fw--update-scroll)
  (pcase-let* ((last (min (+ lsp-bridge-fw--scroll lsp-bridge-fw-count) lsp-bridge-fw--total))
               (bar (ceiling (* lsp-bridge-fw-count lsp-bridge-fw-count) lsp-bridge-fw--total))
               (lo (min (- lsp-bridge-fw-count bar 1) (floor (* lsp-bridge-fw-count lsp-bridge-fw--scroll) lsp-bridge-fw--total)))
               (`(,mf . ,acands) (lsp-bridge-fw--affixate (funcall lsp-bridge-fw--highlight
                                                                   (seq-subseq lsp-bridge-fw--candidates lsp-bridge-fw--scroll last))))
               (`(,pw ,width ,fcands) (lsp-bridge-fw--format-candidates acands))
               ;; Disable the left margin if a margin formatter is active.
               (lsp-bridge-fw-left-margin-width (if mf 0 lsp-bridge-fw-left-margin-width)))
    ;; Nonlinearity at the end and the beginning
    (when (/= lsp-bridge-fw--scroll 0)
      (setq lo (max 1 lo)))
    (when (/= last lsp-bridge-fw--total)
      (setq lo (min (- lsp-bridge-fw-count bar 2) lo)))
    (lsp-bridge-fw--popup-show (+ pos (length lsp-bridge-fw--base)) pw width fcands (- lsp-bridge-fw--index lsp-bridge-fw--scroll)
                               (and (> lsp-bridge-fw--total lsp-bridge-fw-count) lo) bar)))

(defun lsp-bridge-fw--preview-current (beg end)
  "Show current candidate as overlay given BEG and END."
  (when-let (cand (and lsp-bridge-fw-preview-current (>= lsp-bridge-fw--index 0)
                       (/= lsp-bridge-fw--index lsp-bridge-fw--preselect)
                       (nth lsp-bridge-fw--index lsp-bridge-fw--candidates)))
    (setq lsp-bridge-fw--preview-ov (make-overlay beg end nil t t))
    (overlay-put lsp-bridge-fw--preview-ov 'priority 1000)
    (overlay-put lsp-bridge-fw--preview-ov 'window (selected-window))
    (overlay-put lsp-bridge-fw--preview-ov
                 (if (= beg end) 'after-string 'display)
                 (concat lsp-bridge-fw--base cand))))

(defun lsp-bridge-fw--echo-refresh ()
  "Refresh echo message to prevent flicker during redisplay."
  (when lsp-bridge-fw--echo-timer
    (cancel-timer lsp-bridge-fw--echo-timer)
    (setq lsp-bridge-fw--echo-timer nil))
  (lsp-bridge-fw--echo-show lsp-bridge-fw--echo-message))

(defun lsp-bridge-fw--echo-show (&optional msg)
  "Show MSG in echo area."
  (when (or msg lsp-bridge-fw--echo-message)
    (setq msg (or msg "")
          lsp-bridge-fw--echo-message msg)
    (lsp-bridge-fw--message "%s" (if (text-property-not-all 0 (length msg) 'face nil msg)
                                     msg
                                   (propertize msg 'face 'lsp-bridge-fw-echo)))))

(defun lsp-bridge-fw--echo-documentation ()
  "Show documentation string of current candidate in echo area."
  (if-let* ((delay (if (consp lsp-bridge-fw-echo-documentation)
                       (funcall (if lsp-bridge-fw--echo-message #'cdr #'car)
                                lsp-bridge-fw-echo-documentation)
                     lsp-bridge-fw-echo-documentation))
            (fun (plist-get lsp-bridge-fw--extra :company-docsig))
            (cand (and (>= lsp-bridge-fw--index 0)
                       (nth lsp-bridge-fw--index lsp-bridge-fw--candidates))))
      (if (or (eq delay t) (<= delay 0))
          (lsp-bridge-fw--echo-show (funcall fun cand))
        (when lsp-bridge-fw--echo-timer (cancel-timer lsp-bridge-fw--echo-timer))
        (setq lsp-bridge-fw--echo-timer
              (run-at-time delay nil
                           (lambda ()
                             (lsp-bridge-fw--echo-show (funcall fun cand)))))
        (lsp-bridge-fw--echo-show))
    (lsp-bridge-fw--echo-show)))

(defun lsp-bridge-fw--update ()
  "Refresh Lsp-Bridge-Fw UI."
  (pcase-let* ((`(,beg ,end ,table ,pred) completion-in-region--data)
               (pt (- (point) beg))
               (str (buffer-substring-no-properties beg end))
               (initializing (not lsp-bridge-fw--input)))
    (lsp-bridge-fw--echo-refresh)
    (cond
     ;; XXX Guard against errors during candidate generation.
     ;; Turn off completion immediately if there are errors
     ;; For example dabbrev throws error "No dynamic expansion ... found".
     ;; TODO Report this as a bug? Are completion tables supposed to throw errors?
     ((condition-case err
          ;; Only recompute when input changed
          (unless (equal lsp-bridge-fw--input (cons str pt))
            (lsp-bridge-fw--update-candidates str pt table pred)
            nil)
        (error (lsp-bridge-fw-quit)
               (message "Lsp-Bridge-Fw completion error: %s" (error-message-string err)))))
     ;; 1) Initializing, no candidates => Quit. Happens during auto completion.
     ((and initializing (not lsp-bridge-fw--candidates))
      (lsp-bridge-fw-quit))
     ;; 2) Single exactly matching candidate and no further completion is possible.
     ((and (not (equal str ""))
           (equal (car lsp-bridge-fw--candidates) str) (not (cdr lsp-bridge-fw--candidates))
           (not (consp (completion-try-completion str table pred pt lsp-bridge-fw--metadata)))
           (or initializing lsp-bridge-fw-on-exact-match))
      ;; Quit directly when initializing. This happens during auto completion.
      (if (or initializing (eq lsp-bridge-fw-on-exact-match 'quit))
          (lsp-bridge-fw-quit)
        (lsp-bridge-fw--done str 'finished)))
     ;; 3) There exist candidates => Show candidates popup.
     (lsp-bridge-fw--candidates
      (lsp-bridge-fw--candidates-popup beg)
      (lsp-bridge-fw--preview-current beg end)
      (lsp-bridge-fw--echo-documentation)
      (redisplay 'force)) ;; XXX HACK Ensure that popup is redisplayed
     ;; 4) There are no candidates & lsp-bridge-fw-quit-no-match => Confirmation popup.
     ((and (not lsp-bridge-fw--candidates)
           (pcase-exhaustive lsp-bridge-fw-quit-no-match
             ('t nil)
             ('nil t)
             ('separator (seq-contains-p (car lsp-bridge-fw--input) lsp-bridge-fw-separator))))
      (lsp-bridge-fw--popup-show beg 0 8 '(#("No match" 0 8 (face italic))))
      (redisplay 'force)) ;; XXX HACK Ensure that popup is redisplayed
     (t (lsp-bridge-fw-quit)))))

(defun lsp-bridge-fw--pre-command ()
  "Insert selected candidate unless command is marked to continue completion."
  (when lsp-bridge-fw--preview-ov
    (delete-overlay lsp-bridge-fw--preview-ov)
    (setq lsp-bridge-fw--preview-ov nil))
  (when (and (eq lsp-bridge-fw-preview-current 'insert)
             (/= lsp-bridge-fw--index lsp-bridge-fw--preselect)
             ;; See the comment about `overriding-local-map' in `lsp-bridge-fw--post-command'.
             (not (or overriding-terminal-local-map
                      (lsp-bridge-fw--match-symbol-p lsp-bridge-fw-continue-commands this-command))))
    (lsp-bridge-fw--insert 'exact)))

(defun lsp-bridge-fw-insert-separator ()
  "Insert a separator character, inhibiting quit on completion boundary.
See `lsp-bridge-fw-separator' for more details."
  (interactive)
  (insert lsp-bridge-fw-separator))

(defun lsp-bridge-fw--post-command ()
  "Refresh Lsp-Bridge-Fw after last command."
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
                       ;; TODO We keep alive Lsp-Bridge-Fw if a `overriding-terminal-local-map' is
                       ;; installed, for example the `universal-argument-map'. It would be good to
                       ;; think about a better criterion instead. Unfortunately relying on
                       ;; `this-command' alone is not sufficient, since the value of `this-command'
                       ;; gets clobbered in the case of transient keymaps.
                       overriding-terminal-local-map
                       ;; Check if it is an explicitly listed continue command
                       (lsp-bridge-fw--match-symbol-p lsp-bridge-fw-continue-commands this-command)
                       (and
                        ;; Check for empty input
                        (or (not lsp-bridge-fw--input) (< beg end))
                        ;; Check separator or predicate
                        (or (not lsp-bridge-fw-quit-at-boundary)
                            (and (eq lsp-bridge-fw-quit-at-boundary 'separator)
                                 (or (eq this-command #'lsp-bridge-fw-insert-separator)
                                     ;; with separator, any further chars allowed
                                     (seq-contains-p (car lsp-bridge-fw--input) lsp-bridge-fw-separator)))
                            (funcall completion-in-region-mode--predicate))))))
           (lsp-bridge-fw--update)
           t)))
      (lsp-bridge-fw-quit)))

(defun lsp-bridge-fw--goto (index)
  "Go to candidate with INDEX."
  (setq lsp-bridge-fw--index (max lsp-bridge-fw--preselect (min index (1- lsp-bridge-fw--total)))))

(defun lsp-bridge-fw-next (&optional n)
  "Go forward N candidates."
  (interactive "p")
  (let ((index (+ lsp-bridge-fw--index (or n 1))))
    (lsp-bridge-fw--goto
     (cond
      ((not lsp-bridge-fw-cycle) index)
      ((= lsp-bridge-fw--total 0) -1)
      ((< lsp-bridge-fw--preselect 0) (1- (mod (1+ index) (1+ lsp-bridge-fw--total))))
      (t (mod index lsp-bridge-fw--total))))))

(defun lsp-bridge-fw-previous (&optional n)
  "Go backward N candidates."
  (interactive "p")
  (lsp-bridge-fw-next (- (or n 1))))

(defun lsp-bridge-fw-scroll-down (&optional n)
  "Go back by N pages."
  (interactive "p")
  (lsp-bridge-fw--goto (max 0 (- lsp-bridge-fw--index (* (or n 1) lsp-bridge-fw-count)))))

(defun lsp-bridge-fw-scroll-up (&optional n)
  "Go forward by N pages."
  (interactive "p")
  (lsp-bridge-fw-scroll-down (- (or n 1))))

(defun lsp-bridge-fw-first ()
  "Go to first candidate, or to the prompt when the first candidate is selected."
  (interactive)
  (lsp-bridge-fw--goto (if (> lsp-bridge-fw--index 0) 0 -1)))

(defun lsp-bridge-fw-last ()
  "Go to last candidate."
  (interactive)
  (lsp-bridge-fw--goto (1- lsp-bridge-fw--total)))

(defun lsp-bridge-fw-complete ()
  "Try to complete current input.
If a candidate is selected, insert it."
  (interactive)
  (pcase-let ((`(,beg ,end ,table ,pred) completion-in-region--data))
    (if (>= lsp-bridge-fw--index 0)
        ;; Continue completion with selected candidate
        (progn
          (lsp-bridge-fw--insert nil)
          ;; Exit with status 'finished if input is a valid match and no further
          ;; completion is possible. Furthermore treat the completion as
          ;; finished if we are at the end of a boundary, even if other longer
          ;; candidates would still match, since the user invoked `lsp-bridge-fw-complete'
          ;; with an explicitly selected candidate!
          (let ((newstr (buffer-substring-no-properties beg end)))
            (when (and (test-completion newstr table pred)
                       (or
                        (not (consp (completion-try-completion
                                     newstr table pred (length newstr)
                                     (completion-metadata newstr table pred))))
                        (equal (completion-boundaries newstr table pred "") '(0 . 0))))
              (lsp-bridge-fw--done newstr 'finished))))
      ;; Try to complete the current input string
      (let* ((pt (max 0 (- (point) beg)))
             (str (buffer-substring-no-properties beg end))
             (metadata (completion-metadata (substring str 0 pt) table pred)))
        (pcase (completion-try-completion str table pred pt metadata)
          ('t
           (goto-char end)
           (lsp-bridge-fw--done str 'finished))
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
             (lsp-bridge-fw--done newstr 'finished))))))))

(defun lsp-bridge-fw--insert (status)
  "Insert current candidate, exit with STATUS if non-nil."
  (pcase-let* ((`(,beg ,end . ,_) completion-in-region--data)
               (str (buffer-substring-no-properties beg end)))
    ;; XXX There is a small bug here, depending on interpretation.
    ;; When completing "~/emacs/master/li|/calc" where "|" is the
    ;; cursor, then the candidate only includes the prefix
    ;; "~/emacs/master/lisp/", but not the suffix "/calc". Default
    ;; completion has the same problem when selecting in the
    ;; *Completions* buffer. See bug#48356.
    (setq str (concat lsp-bridge-fw--base (substring-no-properties
                                           (nth lsp-bridge-fw--index lsp-bridge-fw--candidates))))
    ;; bug#55205: completion--replace removes properties!
    (completion--replace beg end (concat str))
    (lsp-bridge-fw--goto -1) ;; Reset selection, but continue completion.
    (when status (lsp-bridge-fw--done str status)))) ;; Exit with status

(defun lsp-bridge-fw--done (str status)
  "Call the `:exit-function' with STR and STATUS and exit completion."
  (let ((exit (plist-get lsp-bridge-fw--extra :exit-function)))
    ;; For successfull completions, amalgamate undo operations,
    ;; such that completion can be undone in a single step.
    (undo-amalgamate-change-group lsp-bridge-fw--change-group)
    (lsp-bridge-fw-quit)
    ;; XXX Is the :exit-function handling sufficient?
    (when exit (funcall exit str status))))

(defun lsp-bridge-fw-insert ()
  "Insert current candidate.
Quit if no candidate is selected."
  (interactive)
  (if (>= lsp-bridge-fw--index 0)
      (lsp-bridge-fw--insert 'finished)
    (lsp-bridge-fw-quit)))

(defun lsp-bridge-fw--setup ()
  "Setup Lsp-Bridge-Fw completion state."
  (setq lsp-bridge-fw--extra completion-extra-properties)
  (completion-in-region-mode 1)
  (undo-boundary) ;; Necessary to support `lsp-bridge-fw-reset'
  (activate-change-group (setq lsp-bridge-fw--change-group (prepare-change-group)))
  (setcdr (assq #'completion-in-region-mode minor-mode-overriding-map-alist) lsp-bridge-fw-map)
  (add-hook 'pre-command-hook #'lsp-bridge-fw--pre-command nil 'local)
  (add-hook 'post-command-hook #'lsp-bridge-fw--post-command)
  ;; Disable default post-command handling, since we have our own
  ;; checks in `lsp-bridge-fw--post-command'.
  (remove-hook 'post-command-hook #'completion-in-region--postch)
  (let ((sym (make-symbol "lsp-bridge-fw--teardown"))
        (buf (current-buffer)))
    (fset sym (lambda ()
                ;; Ensure that the teardown runs in the correct buffer, if still alive.
                (unless completion-in-region-mode
                  (remove-hook 'completion-in-region-mode-hook sym)
                  (with-current-buffer (if (buffer-live-p buf) buf (current-buffer))
                    (lsp-bridge-fw--teardown)))))
    (add-hook 'completion-in-region-mode-hook sym)))

(defun lsp-bridge-fw--teardown ()
  "Teardown Lsp-Bridge-Fw."
  ;; Redisplay such that the input becomes immediately visible before the popup
  ;; hiding, which is slow (Issue #48). See also corresponding vertico#89.
  (redisplay)
  (lsp-bridge-fw--popup-hide)
  (remove-hook 'pre-command-hook #'lsp-bridge-fw--pre-command 'local)
  (remove-hook 'post-command-hook #'lsp-bridge-fw--post-command)
  (when lsp-bridge-fw--preview-ov (delete-overlay lsp-bridge-fw--preview-ov))
  (when lsp-bridge-fw--echo-timer (cancel-timer lsp-bridge-fw--echo-timer))
  (lsp-bridge-fw--echo-show)
  (accept-change-group lsp-bridge-fw--change-group)
  (mapc #'kill-local-variable lsp-bridge-fw--state-vars))

(defun lsp-bridge-fw--in-region (&rest args)
  "Lsp-Bridge-Fw completion in region function called with ARGS."
  ;; XXX We can get an endless loop when `completion-in-region-function' is set
  ;; globally to `lsp-bridge-fw--in-region'. This should never happen.
  (apply (if (lsp-bridge-fw--popup-support-p) #'lsp-bridge-fw--in-region-1
           (default-value 'completion-in-region-function))
         args))

(defun lsp-bridge-fw--in-region-1 (beg end table &optional pred)
  "Complete in region, see `completion-in-region' for BEG, END, TABLE, PRED."
  (barf-if-buffer-read-only)
  ;; Restart the completion. This can happen for example if C-M-/
  ;; (`dabbrev-completion') is pressed while the Lsp-Bridge-Fw popup is already open.
  (when completion-in-region-mode (lsp-bridge-fw-quit))
  (let* ((pt (max 0 (- (point) beg)))
         (str (buffer-substring-no-properties beg end))
         (before (substring str 0 pt))
         (metadata (completion-metadata before table pred))
         (exit (plist-get completion-extra-properties :exit-function))
         (threshold (completion--cycle-threshold metadata))
         (completion-in-region-mode-predicate
          (or completion-in-region-mode-predicate (lambda () t))))
    (pcase (completion-try-completion str table pred pt metadata)
      ('nil (lsp-bridge-fw--message "No match") nil)
      ('t (goto-char end)
          (lsp-bridge-fw--message "Sole match")
          (when exit (funcall exit str 'finished))
          t)
      (`(,newstr . ,newpt)
       (pcase-let ((`(,base ,candidates ,total . ,_)
                    (lsp-bridge-fw--recompute-candidates str pt table pred)))
         (unless (markerp beg) (setq beg (copy-marker beg)))
         (setq end (copy-marker end t)
               completion-in-region--data (list beg end table pred))
         (unless (equal str newstr)
           ;; bug#55205: completion--replace removes properties!
           (completion--replace beg end (concat newstr)))
         (goto-char (+ beg newpt))
         (if (= total 1)
             ;; If completion is finished and cannot be further completed,
             ;; return 'finished. Otherwise setup the Lsp-Bridge-Fw popup.
             (cond
              ((consp (completion-try-completion
                       newstr table pred newpt
                       (completion-metadata newstr table pred)))
               (lsp-bridge-fw--setup))
              (exit (funcall exit newstr 'finished)))
           (if (or (= total 0) (not threshold)
                   (and (not (eq threshold t)) (< threshold total)))
               (lsp-bridge-fw--setup)
             (lsp-bridge-fw--cycle-candidates total candidates (+ (length base) beg) end)
             ;; Do not show Lsp-Bridge-Fw when "trivially" cycling, i.e.,
             ;; when the completion is finished after the candidate.
             (unless (equal (completion-boundaries (car candidates) table pred "")
                            '(0 . 0))
               (lsp-bridge-fw--setup)))))
       t))))

(defun lsp-bridge-fw--message (&rest msg)
  "Show completion MSG."
  (let (message-log-max) (apply #'message msg)))

(defun lsp-bridge-fw--cycle-candidates (total cands beg end)
  "Cycle between TOTAL number of CANDS.
See `completion-in-region' for the arguments BEG, END, TABLE, PRED."
  (let* ((idx 0)
         (map (make-sparse-keymap))
         (replace (lambda ()
                    (interactive)
                    ;; bug#55205: completion--replace removes properties!
                    (completion--replace beg end (concat (nth idx cands)))
                    (lsp-bridge-fw--message "Cycling %d/%d..." (1+ idx) total)
                    (setq idx (mod (1+ idx) total))
                    (set-transient-map map))))
    (define-key map [remap completion-at-point] replace)
    (define-key map [remap lsp-bridge-fw-complete] replace)
    (define-key map (vector last-command-event) replace)
    (funcall replace)))

(defun lsp-bridge-fw--auto-complete (tick)
  "Initiate auto completion if TICK did not change."
  (setq lsp-bridge-fw--auto-timer nil)
  (when (and (not completion-in-region-mode) (equal tick (lsp-bridge-fw--auto-tick)))
    (pcase (while-no-input ;; Interruptible capf query
             (run-hook-wrapped 'completion-at-point-functions #'lsp-bridge-fw--capf-wrapper))
      (`(,fun ,beg ,end ,table . ,plist)
       (let ((completion-in-region-mode-predicate
              (lambda () (eq beg (car-safe (funcall fun)))))
             (completion-extra-properties plist))
         (setq completion-in-region--data
               (list (if (markerp beg) beg (copy-marker beg))
                     (copy-marker end t)
                     table
                     (plist-get plist :predicate)))
         (lsp-bridge-fw--setup)
         (lsp-bridge-fw--update))))))

(defun lsp-bridge-fw--auto-post-command ()
  "Post command hook which initiates auto completion."
  (when lsp-bridge-fw--auto-timer
    (cancel-timer lsp-bridge-fw--auto-timer)
    (setq lsp-bridge-fw--auto-timer nil))
  (when (and (not completion-in-region-mode)
             (not defining-kbd-macro)
             (lsp-bridge-fw--match-symbol-p lsp-bridge-fw-auto-commands this-command)
             (lsp-bridge-fw--popup-support-p))
    ;; NOTE: Do not use idle timer since this leads to unacceptable slowdowns,
    ;; in particular if flyspell-mode is enabled.
    (setq lsp-bridge-fw--auto-timer
          (run-at-time lsp-bridge-fw-auto-delay nil
                       #'lsp-bridge-fw--auto-complete (lsp-bridge-fw--auto-tick)))))

(defun lsp-bridge-fw--auto-tick ()
  "Return the current tick/status of the buffer.
Auto completion is only performed if the tick did not change."
  (list (current-buffer) (buffer-chars-modified-tick) (point)))

;;;###autoload
(define-minor-mode lsp-bridge-fw-mode
  "Completion Overlay Region FUnction."
  :global nil :group 'lsp-bridge-fw
  (cond
   (lsp-bridge-fw-mode
    ;; FIXME: Install advice which fixes `completion--capf-wrapper', such that
    ;; it respects the completion styles for non-exclusive capfs. See FIXME in
    ;; the `completion--capf-wrapper' function in minibuffer.el, where the
    ;; issue has been mentioned. We never uninstall this advice since the
    ;; advice is active *globally*.
    (advice-add #'completion--capf-wrapper :around #'lsp-bridge-fw--capf-wrapper-advice)
    (advice-add #'eldoc-display-message-no-interference-p :before-while #'lsp-bridge-fw--allow-eldoc)
    (and lsp-bridge-fw-auto (add-hook 'post-command-hook #'lsp-bridge-fw--auto-post-command nil 'local))
    (setq-local completion-in-region-function #'lsp-bridge-fw--in-region))
   (t
    (remove-hook 'post-command-hook #'lsp-bridge-fw--auto-post-command 'local)
    (kill-local-variable 'completion-in-region-function))))

(defun lsp-bridge-fw--capf-wrapper (fun &optional prefix)
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
            (or (eq len t) (>= len lsp-bridge-fw-auto-prefix)))
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

(defun lsp-bridge-fw--capf-wrapper-advice (orig fun which)
  "Around advice for `completion--capf-wrapper'.
The ORIG function takes the FUN and WHICH arguments."
  (if lsp-bridge-fw-mode (lsp-bridge-fw--capf-wrapper fun t) (funcall orig fun which)))

;;;###autoload
(define-globalized-minor-mode global-lsp-bridge-fw-mode lsp-bridge-fw-mode lsp-bridge-fw--on :group 'lsp-bridge-fw)

(defun lsp-bridge-fw--on ()
  "Turn `lsp-bridge-fw-mode' on."
  (unless (or noninteractive
              (eq (aref (buffer-name) 0) ?\s)
              (memq major-mode lsp-bridge-fw-excluded-modes))
    (lsp-bridge-fw-mode 1)))

(defun lsp-bridge-fw--allow-eldoc ()
  "Return non-nil if Lsp-Bridge-Fw is currently not active."
  (not (and lsp-bridge-fw-mode completion-in-region-mode)))

;; Emacs 28: Do not show Lsp-Bridge-Fw commands with M-X
(dolist (sym '(lsp-bridge-fw-next lsp-bridge-fw-previous lsp-bridge-fw-first lsp-bridge-fw-last lsp-bridge-fw-quit lsp-bridge-fw-reset
                                  lsp-bridge-fw-complete lsp-bridge-fw-insert lsp-bridge-fw-scroll-up lsp-bridge-fw-scroll-down
                                  lsp-bridge-fw-insert-separator))
  (put sym 'completion-predicate #'ignore))


(defvar lsp-bridge-fw-icon--cache nil
  "The cache of styled and padded label (text or icon).
An alist.")

(defun lsp-bridge-fw-icon-reset-cache ()
  "Remove all cached icons from `lsp-bridge-fw-icon-mapping'."
  (interactive)
  (setq lsp-bridge-fw-icon--cache nil))

(defvar lsp-bridge-fw-icon--icons
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

(defsubst lsp-bridge-fw-icon--metadata-get (metadata type-name)
  (or
   (plist-get completion-extra-properties (intern (format ":%s" type-name)))
   (cdr (assq (intern type-name) metadata))))

(defun lsp-bridge-fw-icon-formatted (kind)
  "Format icon kind with all-the-icons"
  (or (alist-get kind lsp-bridge-fw-icon--cache)
      (let ((map (assq kind lsp-bridge-fw-icon--icons)))
        (let*  ((icon (if map
                          (cdr map)
                        (cdr (assq t lsp-bridge-fw-icon--icons))))
                (half (/ (default-font-width) 2))
                (pad (propertize " " 'display `(space :width (,half))))
                (disp (concat pad icon pad)))
          (setf (alist-get kind lsp-bridge-fw-icon--cache) disp)
          disp))))

(defun lsp-bridge-fw-icon-margin-formatter (metadata)
  "Return a margin-formatter function which produces kind icons.
METADATA is the completion metadata supplied by the caller (see
info node `(elisp)Programmed Completion').  To use, add this
function to the relevant margin-formatters list."
  (if-let ((kind-func (lsp-bridge-fw-icon--metadata-get metadata "company-kind")))
      (lambda (cand)
        (if-let ((kind (funcall kind-func cand)))
            (lsp-bridge-fw-icon-formatted kind)
          (lsp-bridge-fw-icon-formatted t))))) ;; as a backup

(add-to-list 'lsp-bridge-fw-margin-formatters #'lsp-bridge-fw-icon-margin-formatter)

(provide 'lsp-bridge-fw)

;;; lsp-bridge-fw.el ends here
