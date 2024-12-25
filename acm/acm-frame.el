;;; acm-frame.el --- Description -*- lexical-binding: t; no-byte-compile: t; -*-*-
;;
;; Copyright (C) 2022 royokong
;;
;; Author: royokong <j614023177@icloud.com>
;; Maintainer: royokong <j614023177@icloud.com>
;; Created: December 10, 2022
;; Modified: December 10, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defface acm-frame-default-face
  '()
  "Default face, foreground and background colors used for the popup.")

(defface acm-frame-border-face
  '((((class color) (min-colors 88) (background dark)) :background "#323232")
    (((class color) (min-colors 88) (background light)) :background "#d7d7d7")
    (t :background "gray"))
  "The background color used for the thin border.")

(defface acm-frame-select-face
  '()
  "Face used to highlight the currently selected candidate.")


(defvar acm-frame--mouse-ignore-map
  (let ((map (make-sparse-keymap)))
    (dotimes (i 7)
      (dolist (k '(mouse down-mouse drag-mouse double-mouse triple-mouse))
        (define-key map (vector (intern (format "%s-%s" k (1+ i)))) #'ignore)))
    map)
  "Ignore all mouse clicks.")

(defvar acm-frame--emacs-frame nil
  "The emacs frame.")

(defconst acm-frame-fit-frame-to-buffer
  (if (functionp 'fit-frame-to-buffer-1)
      'fit-frame-to-buffer-1
    'fit-frame-to-buffer)
  "Function used to fit frame to buffer.")

(defun acm-frame-border-face ()
  (if (facep 'child-frame-border) 'child-frame-border 'internal-border))

(defun acm-frame-set-frame-colors (frame)
  ;; Set frame border color.
  (let* ((face (acm-frame-border-face))
         (new (face-attribute 'acm-frame-border-face :background nil 'default)))
    (unless (equal (face-attribute face :background frame 'default) new)
      (set-face-background face new frame)))

  ;; Set frame background color.
  (let ((new (face-attribute 'acm-frame-default-face :background nil 'default)))
    (unless (equal (frame-parameter frame 'background-color) new)
      (set-frame-parameter frame 'background-color new))))

(defvar acm-frame-font nil)
(defvar x-gtk-resize-child-frames) ;; not present on non-gtk builds
(defun acm-frame-make-frame (frame-name &optional no-accept-focus)
  (let* ((after-make-frame-functions nil)
         (parent (selected-frame))
         (parent-font (with-selected-frame parent
                        (face-attribute 'default :font)))
         (border-width (if no-accept-focus 0 1)) ;; no border if no-accept-focus
         (x-gtk-resize-child-frames
          (let ((case-fold-search t))
            (and
             ;; Fix resizing frame on gtk3/gnome.
             (string-match-p "gtk3" system-configuration-features)
             (string-match-p "gnome\\|cinnamon"
                             (or (getenv "XDG_CURRENT_DESKTOP")
                                 (getenv "DESKTOP_SESSION") ""))
             'resize-mode)))
         frame)
    (setq frame (make-frame
                 `((name . ,frame-name)
                   (parent-frame . ,parent)
                   (no-accept-focus . ,no-accept-focus)
                   (no-focus-on-map . ,no-accept-focus)
                   (minibuffer . nil)
                   (min-width . t)
                   (min-height . t)
                   (width . 0)
                   (height . 0)
                   (border-width . 0)
                   (internal-border-width . 1)
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
                   (mode-line-format . nil)
                   )))

    ;; Make sure popup frame's font same as parent frame.
    (with-selected-frame frame
      (set-frame-font parent-font)

      ;; Record frame font to restore frame font after computer suspend.
      (unless acm-frame-font
        (setq acm-frame-font parent-font)))

    ;; Set popup frame colors.
    (acm-frame-set-frame-colors frame)

    ;; Record parent frame.
    (setq acm-frame--emacs-frame parent)

    ;; Reset to the input focus to the parent frame.
    (if no-accept-focus
        (redirect-frame-focus frame parent)
      (select-frame-set-input-focus frame))
    frame))

;; In Emacs 30, if you use 4k screen,
;; font will become small when system return from suspend.
;; I don't know why Emacs 30 does this,
;; so I call `acm-frame-restore-font' on hook `after-focus-change-function'
;; make sure font size restore to normal size when system return from suspend.
(defun acm-frame-restore-font ()
  (ignore-errors
    (with-selected-frame acm-menu-frame
      (set-frame-font acm-frame-font))

    (with-selected-frame acm-doc-frame
      (set-frame-font acm-frame-font))))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook
              (lambda ()
                (add-function :after after-focus-change-function #'acm-frame-restore-font)
                ))
  (add-function :after after-focus-change-function #'acm-frame-restore-font))

(cl-defmacro acm-frame-new (frame frame-buffer buffer-name &optional max-width max-height popup-pos)
  `(progn
     (when (and (frame-live-p ,frame)
                (not (eq (frame-parent ,frame) (selected-frame))))
       (acm-frame-delete-frame ,frame))

     (acm-frame-create-frame-if-not-exist ,frame ,frame-buffer ,buffer-name 1 t)

     (acm-frame-set-frame-max-size ,frame ,max-width ,max-height)

     (let ((pos (acm-frame-get-popup-position (point) 1)))
       (acm-frame-set-frame-position ,frame (car pos) (cdr pos)))

     (acm-frame-adjust-frame-pos ,frame ,popup-pos)))

(cl-defmacro acm-frame-create-frame-if-not-exist (frame frame-buffer frame-name margin no-accept-focus)
  `(unless (frame-live-p ,frame)
     (setq ,frame (acm-frame-make-frame ,frame-name ,no-accept-focus))

     (with-current-buffer (get-buffer-create ,frame-buffer)
       ;; Install mouse ignore map
       (use-local-map acm-frame--mouse-ignore-map)

       ;; Set buffer arguments.
       (dolist (var '((mode-line-format . nil)
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
                      (right-fringe-width . nil)
                      (left-margin-width . ,margin)
                      (right-margin-width . ,margin)
                      (fringes-outside-margins . 0)))
         (set (make-local-variable (car var)) (cdr var))))

     ;; Set frame window and buffer.
     (let ((win (frame-root-window ,frame)))
       (set-window-buffer win ,frame-buffer)
       ;; Mark window as dedicated to prevent frame reuse.
       (set-window-dedicated-p win t))
     ))

(defun acm-frame-set-frame-max-size (frame &optional max-width max-height)
  ;; Set the smallest window size value to ensure that frame adjusts to the accurate size of its content.
  (let* ((window-min-height 0)
         (window-min-width 0))
    (funcall acm-frame-fit-frame-to-buffer frame max-height nil max-width nil)))

(defun acm-frame-set-frame-size (frame width height)
  (set-frame-size frame width height))

(defun acm-frame-set-frame-position (frame x y)
  ;; Make sure frame visible before set position.
  (unless (frame-visible-p frame)
    ;; Force redisplay, otherwise the popup sometimes does not display content.
    (redisplay 'force)
    (make-frame-visible frame))

  (set-frame-position frame x y))

(defun acm-frame-set-frame-pos-center (frame)
  (let* ((parent-width (frame-pixel-width acm-frame--emacs-frame))
         (parent-height (frame-pixel-height acm-frame--emacs-frame))
         (frame-width (frame-pixel-width frame))
         (frame-height (frame-pixel-height frame))
         (x (+ (frame-parameter frame 'left) (/ (- parent-width frame-width) 2)))
         (y (+ (frame-parameter frame 'top) (/ (- parent-height frame-height) 2))))
    (acm-frame-set-frame-position frame x y)))

(defun acm-frame-get-popup-position (frame-popup-point &optional line-bias)
  (let* ((edges (window-pixel-edges))
         (window-left (+
                       ;; Edges fine-tuning.
                       (nth 0 edges)
                       ;; Icon fine-tuning.
                       (if acm-enable-icon
                           0
                         (* (frame-char-width) (- acm-icon-width 1))) ; acm will add left padding 1 char when icon is disable
                       ;; Index fine-tuning.
                       (if acm-enable-quick-access
                           (- (* (frame-char-width) 3)) ; 3 is index width
                         0)
                       ;; Margin fine-tuning for centering module.
                       (/ (- (window-pixel-width)
                             (window-body-width nil t))
                          2)))
         (window-top (nth 1 edges))
         (pos (posn-x-y (posn-at-point frame-popup-point)))
         (x (car pos))
         (y (+ (cdr pos) (* (or line-bias 0) (line-pixel-height))))
         (offset-y
          ;; We need move down to skip tab-line and header-line.
          (if (version< emacs-version "27.0")
              (window-header-line-height)
            (+ (window-tab-line-height)
               (window-header-line-height)))))
    (cons (+ x window-left)
          (+ y window-top offset-y))))

(cl-defmacro acm-frame-delete-frame (frame)
  `(when (frame-live-p ,frame)
     (if (and (not (frame-parameter ,frame 'no-accept-focus))
              (frame-live-p acm-frame--emacs-frame))
         (select-frame-set-input-focus acm-frame--emacs-frame))
     (delete-frame ,frame)
     (setq ,frame nil)))

(defun acm-frame-get-theme-mode ()
  "Get theme mode, dark or light."
  (prin1-to-string (frame-parameter nil 'background-mode)))

(defun acm-frame-color-blend (c1 c2 alpha)
  "Blend two colors C1 and C2 with ALPHA.
C1 and C2 are hexidecimal strings.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (apply (lambda (r g b)
           (format "#%02x%02x%02x"
                   (ash r -8)
                   (ash g -8)
                   (ash b -8)))
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          (color-values c1) (color-values c2))))

(defun acm-frame-init-colors (&optional force)
  (let* ((is-dark-mode (string-equal (acm-frame-get-theme-mode) "dark"))
         (blend-background (if is-dark-mode "#000000" "#AAAAAA"))
         (default-background (if (or force (equal (face-attribute 'acm-frame-default-face :background) 'unspecified))
                                 (face-attribute 'default :background)
                               (face-attribute 'acm-frame-default-face :background))))

    ;; Fallback to background of `default' face if `acm-frame-color-blend' test failed.
    (unless (ignore-errors
              (acm-frame-color-blend default-background blend-background 0.6))
      (setq default-background (if is-dark-mode "#000000" "#AAAAAA")))

    ;; Make sure menu follow the theme of Emacs.
    (when (or force (equal (face-attribute 'acm-frame-default-face :background) 'unspecified))
      (set-face-background 'acm-frame-default-face (acm-frame-color-blend default-background blend-background (if is-dark-mode 0.8 0.9))))
    (when (or force (equal (face-attribute 'acm-frame-select-face :background) 'unspecified))
      (set-face-background 'acm-frame-select-face (acm-frame-color-blend default-background blend-background 0.6)))
    (when (or force (equal (face-attribute 'acm-frame-select-face :foreground) 'unspecified))
      (set-face-foreground 'acm-frame-select-face (face-attribute 'font-lock-function-name-face :foreground)))))

(defun acm-frame-visible-p (frame)
  (and (frame-live-p frame)
       (frame-visible-p frame)))

(defun acm-frame-hide-frame (frame)
  (when (acm-frame-visible-p frame)
    (make-frame-invisible frame)))

(defun acm-frame-adjust-frame-pos (frame &optional popup-pos margin action-menu-p)
  "Adjust position to avoid out of screen.
ACTION-MENU-P is used to give code action menu a special treat to make it more useful.
Only when calling this function from code-action file should make this variable be true."
  (let* ((margin (or margin 50))
         (popup-pos (or popup-pos "point"))
         (main-window-x (car (frame-position acm-frame--emacs-frame)))
         (main-window-y (cdr (frame-position acm-frame--emacs-frame)))
         (main-window-width (frame-pixel-width acm-frame--emacs-frame))
         (main-window-height (frame-pixel-height acm-frame--emacs-frame))
         (main-window-right-limit (- (+ main-window-x main-window-width) margin))
         (main-window-bottom-limit (- (+ main-window-y main-window-height) margin))
         (frame-x (car (frame-position frame)))
         (frame-y (cdr (frame-position frame)))
         (frame-width (frame-pixel-width frame))
         (frame-height (frame-pixel-height frame))
         (frame-right-edge (+ frame-x frame-width))
         (frame-bottom-edge (+ frame-y frame-height)))
    (pcase popup-pos
      ("top-left"
       (set-frame-position frame main-window-x main-window-y))
      ("top-right"
       (set-frame-position frame (- main-window-width frame-width) main-window-y))
      ("bottom-left"
       (set-frame-position frame main-window-x (- main-window-height frame-height)))
      ("bottom-right"
       (set-frame-position frame (- main-window-width frame-width) (- main-window-height frame-height)))
      ("point"
       (when (> frame-right-edge main-window-right-limit)
         (set-frame-position frame
                             (- (+ main-window-x main-window-width) frame-width margin)
                             frame-y))
       (when (> frame-bottom-edge main-window-bottom-limit)
         (if action-menu-p
             (set-frame-position frame frame-x main-window-y)
           (set-frame-position frame
                               frame-x
                               (- (+ main-window-y main-window-height) frame-height margin))))))))

(defun acm-frame-can-display-p ()
  (not (or noninteractive
           emacs-basic-display
           (not (display-graphic-p)))))

(provide 'acm-frame)
;;; acm-frame.el ends here
