;;; lsp-bridge.el --- LSP bridge  -*- lexical-binding: t -*-

;; Filename: lsp-bridge.el
;; Description: LSP bridge
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.5
;; Last-Updated: 2022-10-10 15:23:53 +0800
;;           By: Gong Qijian
;; URL: https://github.com/manateelazycat/lsp-bridge
;; Keywords:
;; Compatibility: emacs-version >= 28
;; Package-Requires: ((emacs "28") (posframe "1.1.7") (markdown-mode "2.6"))
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
;; Lsp-Bridge
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
;;      M-x customize-group RET lsp-bridge RET
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
(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'posframe)
(require 'markdown-mode)
(require 'diff)

(defvar acm-library-path (expand-file-name "acm" (if load-file-name
                                                     (file-name-directory load-file-name)
                                                   default-directory)))
(add-to-list 'load-path acm-library-path t)
(require 'acm-frame)

(require 'lsp-bridge-epc)
(require 'lsp-bridge-ref)
(require 'lsp-bridge-jdtls)
(require 'lsp-bridge-call-hierarchy)
(require 'lsp-bridge-code-action)
(require 'lsp-bridge-diagnostic)
(require 'lsp-bridge-lsp-installer)

(defgroup lsp-bridge nil
  "LSP-Bridge group."
  :group 'applications)

(require 'acm)

(setq acm-backend-lsp-fetch-completion-item-func 'lsp-bridge-fetch-completion-item-info)

(defun lsp-bridge-fetch-completion-item-info (candidate)
  (let ((kind (plist-get candidate :icon))
        (server-name (plist-get candidate :server))
        (key (plist-get candidate :key)))
    ;; Try send `completionItem/resolve' request to fetch `documentation' and `additionalTextEdits' information.
    (unless (equal acm-backend-lsp-fetch-completion-item-ticker (list acm-backend-lsp-filepath key kind))
      (lsp-bridge-call-async "fetch_completion_item_info" acm-backend-lsp-filepath key server-name)
      (setq-local acm-backend-lsp-fetch-completion-item-ticker (list acm-backend-lsp-filepath key kind)))))

(defcustom lsp-bridge-completion-popup-predicates '(
                                                    lsp-bridge-not-follow-complete
                                                    lsp-bridge-not-match-stop-commands
                                                    lsp-bridge-not-match-hide-characters

                                                    lsp-bridge-not-only-blank-before-cursor
                                                    lsp-bridge-not-in-string
                                                    lsp-bridge-not-in-org-table

                                                    lsp-bridge-not-execute-macro
                                                    lsp-bridge-not-in-multiple-cursors
                                                    lsp-bridge-not-in-mark-macro

                                                    lsp-bridge-is-evil-state
                                                    lsp-bridge-is-meow-state

                                                    lsp-bridge-not-complete-manually
                                                    )
  "A list of predicate functions with no argument to enable popup completion in callback."
  :type '(repeat function)
  :group 'lsp-bridge)

(defcustom lsp-bridge-flash-region-delay .3
  "How many seconds to flash `lsp-bridge-font-lock-flash' after navigation.

Setting this to nil or 0 will turn off the indicator."
  :type 'number
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-mode-line t
  "Whether display LSP-bridge's server info in mode-line ."
  :type 'boolean
  :group 'lsp-bridge)

(defcustom lsp-bridge-completion-stop-commands
  '("undo-tree-undo" "undo-tree-redo"
    "kill-region" "delete-block-backward"
    "python-black-buffer" "acm-complete-or-expand-yas-snippet"
    "yank" "string-rectangle" "query-replace" "grammatical-edit-unwrap")
  "If last command is match this option, stop popup completion ui."
  :type 'cons
  :group 'lsp-bridge)

(defcustom lsp-bridge-completion-hide-characters '(":" ";" "(" ")" "[" "]" "{" "}" "," "\"")
  "If character before match this option, stop popup completion ui."
  :type 'cons
  :group 'lsp-bridge)

(defcustom lsp-bridge-completion-obey-trigger-characters-p nil
  "If non-nil makes trigger characters a higher priority than `lsp-bridge-completion-hide-characters'."
  :type 'boolean
  :group 'lsp-bridge)

(defcustom lsp-bridge-lookup-doc-tooltip " *lsp-bridge-hover*"
  "Buffer for display hover information."
  :type 'string
  :group 'lsp-bridge)

(defcustom lsp-bridge-lookup-doc-tooltip-border-width 20
  "The border width of lsp-bridge hover tooltip, in pixels."
  :type 'integer
  :group 'lsp-bridge)

(defcustom lsp-bridge-lookup-doc-tooltip-max-width 150
  "The max width of lsp-bridge hover tooltip."
  :type 'integer
  :group 'lsp-bridge)

(defcustom lsp-bridge-lookup-doc-tooltip-max-height 20
  "The max width of lsp-bridge hover tooltip."
  :type 'integer
  :group 'lsp-bridge)

(defcustom lsp-bridge-disable-backup t
  "Default disable backup feature, include `make-backup-files' `auto-save-default' and `create-lockfiles'."
  :type 'boolean
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-signature-help t
  "Whether to enable signature-help."
  :type 'boolean
  :group 'lsp-bridge)

(defcustom lsp-bridge-signature-help-fetch-idle 0.5
  "The idle seconds to fetch signature help.."
  :type 'float
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-search-words t
  "Whether to enable search words of files."
  :type 'boolean
  :group 'lsp-bridge)

(defcustom lsp-bridge-search-words-rebuild-cache-idle 1
  "The idle seconds to rebuild words cache."
  :type 'float
  :group 'lsp-bridge)

(defcustom lsp-bridge-search-words-prohibit-file-extensions '("png" "jpg" "jpeg" "gif" "pdf")
  "The file extensions to prohibit search words."
  :type 'list
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-auto-format-code nil
  "Whether to auto format code."
  :type 'boolean
  :group 'lsp-bridge)

(defcustom lsp-bridge-auto-format-code-idle 1
  "The idle seconds to auto format code."
  :type 'float
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-diagnostics t
  "Whether to enable diagnostics."
  :type 'boolean
  :group 'lsp-bridge)

(defcustom lsp-bridge-elisp-symbols-update-idle 3
  "The idle seconds to update elisp symbols."
  :type 'float
  :group 'lsp-bridge)

(defcustom lsp-bridge-user-langserver-dir nil
  "The directory where the user place langserver configuration."
  :type 'string
  :group 'lsp-bridge)

(defcustom lsp-bridge-user-multiserver-dir nil
  "The directory where the user place multiserver configuration."
  :type 'string
  :group 'lsp-bridge)

(defcustom lsp-bridge-symbols-enable-which-func nil
  "Wether use lsp-bridge in which-func"
  :type 'boolean
  :group 'lsp-bridge)

(defface lsp-bridge-font-lock-flash
  '((t (:inherit highlight)))
  "Face to flash the current line."
  :group 'lsp-bridge)

(defvar lsp-bridge-last-change-command nil)
(defvar lsp-bridge-last-change-position nil)
(defvar lsp-bridge-server nil
  "The LSP-Bridge Server.")

(defvar lsp-bridge-python-file (expand-file-name "lsp_bridge.py" (if load-file-name
                                                                     (file-name-directory load-file-name)
                                                                   default-directory)))

(defvar-local lsp-bridge-mark-ring nil
  "The list of saved lsp-bridge marks, most recent first.")

(defvar lsp-bridge-server-port nil)

;; org babel cache
(defvar-local lsp-bridge--org-babel-info-cache nil)
(defvar-local lsp-bridge--org-babel-block-bop nil)
(defvar-local lsp-bridge--org-babel-block-eop nil)

(defun lsp-bridge--start-epc-server ()
  "Function to start the EPC server."
  (unless (process-live-p lsp-bridge-server)
    (setq lsp-bridge-server
          (lsp-bridge-epc-server-start
           (lambda (mngr)
             (let ((mngr mngr))
               (lsp-bridge-epc-define-method mngr 'eval-in-emacs 'lsp-bridge--eval-in-emacs-func)
               (lsp-bridge-epc-define-method mngr 'get-emacs-var 'lsp-bridge--get-emacs-var-func)
               (lsp-bridge-epc-define-method mngr 'get-emacs-vars 'lsp-bridge--get-emacs-vars-func)
               (lsp-bridge-epc-define-method mngr 'get-project-path 'lsp-bridge--get-project-path-func)
               (lsp-bridge-epc-define-method mngr 'get-workspace-folder 'lsp-bridge--get-workspace-folder-func)
               (lsp-bridge-epc-define-method mngr 'get-multi-lang-server 'lsp-bridge--get-multi-lang-server-func)
               (lsp-bridge-epc-define-method mngr 'get-single-lang-server 'lsp-bridge--get-single-lang-server-func)
               (lsp-bridge-epc-define-method mngr 'get-emacs-version 'emacs-version)
               (lsp-bridge-epc-define-method mngr 'get-user-emacs-directory 'lsp-bridge--user-emacs-directory)
               (lsp-bridge-epc-define-method mngr 'is-snippet-support 'acm-backend-lsp-snippet-expansion-fn)
               (lsp-bridge-epc-define-method mngr 'get-buffer-content 'lsp-bridge--get-buffer-content-func)
               (lsp-bridge-epc-define-method mngr 'get-org-block-line-bias 'lsp-bridge--get-org-block-line-bias)
               ))))
    (if lsp-bridge-server
        (setq lsp-bridge-server-port (process-contact lsp-bridge-server :service))
      (error "[LSP-Bridge] lsp-bridge-server failed to start")))
  lsp-bridge-server)

(defun lsp-bridge--eval-in-emacs-func (sexp-string)
  (eval (read sexp-string))
  ;; Return nil to avoid epc error `Got too many arguments in the reply'.
  nil)

(defun lsp-bridge--get-emacs-var-func (var-name)
  (let* ((var-symbol (intern var-name))
         (var-value (symbol-value var-symbol))
         ;; We need convert result of booleanp to string.
         ;; Otherwise, python-epc will convert all `nil' to [] at Python side.
         (var-is-bool (prin1-to-string (booleanp var-value))))
    (list var-value var-is-bool)))

(defun lsp-bridge--get-emacs-vars-func (&rest vars)
  (mapcar #'lsp-bridge--get-emacs-var-func vars))

(defvar lsp-bridge-epc-process nil)

(defvar lsp-bridge-internal-process nil)
(defvar lsp-bridge-internal-process-prog nil)
(defvar lsp-bridge-internal-process-args nil)

(defcustom lsp-bridge-name "*lsp-bridge*"
  "Name of LSP-Bridge buffer."
  :type 'string)

(defcustom lsp-bridge-python-command (if (memq system-type '(cygwin windows-nt ms-dos)) "python.exe" "python3")
  "The Python interpreter used to run lsp_bridge.py."
  :type 'string)

(defcustom lsp-bridge-enable-debug nil
  "If you got segfault error, please turn this option.
Then LSP-Bridge will start by gdb, please send new issue with `*lsp-bridge*' buffer content when next crash."
  :type 'boolean)

(defcustom lsp-bridge-enable-log nil
  "Enable this option to print log message in `*lsp-bridge*' buffer, default only print message header."
  :type 'boolean)

(defcustom lsp-bridge-enable-profile nil
  "Enable this option to output performance data to ~/lsp-bridge.prof."
  :type 'boolean)

(defcustom lsp-bridge-multi-lang-server-extension-list
  '(
    (("vue")        . "volar_emmet")
    (("ts" "tsx")   . "typescript_eslint")
    )
  "The multi lang server rule for file extension."
  :type 'cons)

(defcustom lsp-bridge-single-lang-server-extension-list
  '(
    (("wxml") . "wxml-language-server")
    (("html") . "vscode-html-language-server")
    (("astro") . "astro-ls")
    )
  "The lang server rule for file extension."
  :type 'cons)

(defcustom lsp-bridge-c-lsp-server "clangd"
  "Default LSP server for C language, you can choose `clangd' or `ccls'."
  :type 'string)

(defcustom lsp-bridge-php-lsp-server "intelephense"
  "Default LSP server for PHP language, you can choose `intelephense' or `phpactor'."
  :type 'string
  :safe #'stringp)

(defcustom lsp-bridge-python-lsp-server "pyright"
  "Default LSP server for Python language, you can choose `pyright', `jedi', `python-ms', `pylsp'."
  :type 'string)

(defcustom lsp-bridge-python-multi-lsp-server "pyright-background-analysis_ruff"
  "Default Multi LSP server for Python, you can choose `pyright_ruff', `jedi_ruff', `python-ms_ruff', `pylsp_ruff'."
  :type 'string)

(defcustom lsp-bridge-tex-lsp-server "texlab"
  "Default LSP server for (la)tex, you can choose `texlab' or `digestif'."
  :type 'string)

(defcustom lsp-bridge-csharp-lsp-server "omnisharp-dotnet"
  "Default LSP server for C#, you can choose `omnisharp-mono' or `omnisharp-dotnet'."
  :type 'string)

(defcustom lsp-bridge-use-wenls-in-org-mode nil
  "Use `wen' lsp server in org-mode, default is disable.")

(defcustom lsp-bridge-use-ds-pinyin-in-org-mode nil
  "Use `ds-pinyin' lsp server in org-mode, default is disable.")

(defcustom lsp-bridge-enable-org-babel nil
  "Use `lsp-bridge' in org-babel, default is disable.")

(defcustom lsp-bridge-org-babel-lang-list nil
  "A list of org babel languages like (\"python\" \"bash\"), which enable lsp-bridge. if nil means enable all languages."
  :type '(repeat string))

(defcustom lsp-bridge-complete-manually nil
  "Only popup completion menu when user call `lsp-bridge-popup-complete-menu' command.")

(defcustom lsp-bridge-multi-lang-server-mode-list
  '(((python-mode python-ts-mode) . lsp-bridge-python-multi-lsp-server)
    ((qml-mode qml-ts-mode) . "qmlls_javascript"))
  "The multi lang server rule for file mode."
  :type 'cons)

(defcustom lsp-bridge-single-lang-server-mode-list
  '(
    ((c-mode c-ts-mode c++-mode c++-ts-mode objc-mode) . lsp-bridge-c-lsp-server)
    ((cmake-mode cmake-ts-mode) . "cmake-language-server")
    ((java-mode java-ts-mode) . "jdtls")
    ((julia-mode) . "julials")
    ((python-mode python-ts-mode) . lsp-bridge-python-lsp-server)
    (ruby-mode . "solargraph")
    ((rust-mode rustic-mode rust-ts-mode) . "rust-analyzer")
    ((elixir-mode elixir-ts-mode heex-ts-mode) . "elixirLS")
    ((go-mode go-ts-mode) . "gopls")
    (groovy-mode . "groovy-language-server")
    (haskell-mode . "hls")
    (lua-mode . "sumneko")
    (dart-mode . "dart-analysis-server")
    (scala-mode . "metals")
    ((js2-mode js-mode js-ts-mode rjsx-mode) . "javascript")
    ((typescript-tsx-mode tsx-ts-mode) . "typescriptreact")
    ((typescript-mode typescript-ts-mode) . "typescript")
    (tuareg-mode . "ocamllsp")
    (erlang-mode . "erlang-ls")
    ((latex-mode Tex-latex-mode texmode context-mode texinfo-mode bibtex-mode) . lsp-bridge-tex-lsp-server)
    ((clojure-mode clojurec-mode clojurescript-mode clojurex-mode) . "clojure-lsp")
    ((sh-mode bash-mode bash-ts-mode) . "bash-language-server")
    ((css-mode css-ts-mode) . "vscode-css-language-server")
    (elm-mode . "elm-language-server")
    (php-mode . lsp-bridge-php-lsp-server)
    ((yaml-mode yaml-ts-mode) . "yaml-language-server")
    (zig-mode . "zls")
    (dockerfile-mode . "docker-langserver")
    (d-mode . "serve-d")
    ((fortran-mode f90-mode) . "fortls")
    (nix-mode . "rnix-lsp")
    (ess-r-mode . "rlanguageserver")
    (graphql-mode . "graphql-lsp")
    (swift-mode . "swift-sourcekit")
    (csharp-mode . lsp-bridge-csharp-lsp-server)
    (kotlin-mode . "kotlin-language-server")
    (vhdl-mode . "vhdl-tool")
    )
  "The lang server rule for file mode."
  :type 'cons)

(defcustom lsp-bridge-default-mode-hooks
  '(c-mode-hook
    c++-mode-hook
    cmake-mode-hook
    java-mode-hook
    julia-mode-hook
    python-mode-hook
    ruby-mode-hook
    lua-mode-hook
    rust-mode-hook
    rustic-mode-hook
    erlang-mode-hook
    elixir-mode-hook
    go-mode-hook
    haskell-mode-hook
    haskell-literate-mode-hook
    dart-mode-hook
    scala-mode-hook
    typescript-mode-hook
    typescript-tsx-mode-hook
    js2-mode-hook
    js-mode-hook
    rjsx-mode-hook
    tuareg-mode-hook
    latex-mode-hook
    Tex-latex-mode-hook
    texmode-hook
    context-mode-hook
    texinfo-mode-hook
    bibtex-mode-hook
    clojure-mode-hook
    clojurec-mode-hook
    clojurescript-mode-hook
    clojurex-mode-hook
    sh-mode-hook
    bash-mode-hook
    web-mode-hook
    css-mode-hook
    elm-mode-hook
    emacs-lisp-mode-hook
    ielm-mode-hook
    lisp-interaction-mode-hook
    org-mode-hook
    php-mode-hook
    yaml-mode-hook
    zig-mode-hook
    groovy-mode-hook
    dockerfile-mode-hook
    d-mode-hook
    f90-mode-hook
    fortran-mode-hook
    nix-mode-hook
    ess-r-mode-hook
    verilog-mode-hook
    swift-mode-hook
    csharp-mode-hook
    telega-chat-mode-hook
    markdown-mode-hook
    kotlin-mode-hook
    vhdl-mode-hook

    c-ts-mode-hook
    c++-ts-mode-hook
    cmake-ts-mode-hook
    elixir-ts-mode-hook
    toml-ts-mode-hook
    css-ts-mode-hook
    js-ts-mode-hook
    json-ts-mode-hook
    python-ts-mode-hook
    bash-ts-mode-hook
    typescript-ts-mode-hook)
  "The default mode hook to enable lsp-bridge."
  :type '(repeat variable))

(defcustom lsp-bridge-get-single-lang-server-by-project nil
  "Get lang server with project path and file path.")

(defcustom lsp-bridge-get-multi-lang-server-by-project nil
  "Get lang server with project path and file path.")

(defcustom lsp-bridge-get-project-path-by-filepath nil
  "Default use command 'git rev-parse --show-toplevel' get project path,
you can customize `lsp-bridge-get-project-path-by-filepath' to return project path by give file path.")

(defcustom lsp-bridge-get-workspace-folder nil
  "In Java, sometimes, we need return same workspace folder for multiple projects,
you can customize `lsp-bridge-get-workspace-folder' to return workspace folder path by give project path.")

(defvar lsp-bridge-formatting-indent-alist
  '((c-mode                     . c-basic-offset) ; C
    (c++-mode                   . c-basic-offset) ; C++
    (csharp-mode                . c-basic-offset) ; C#
    (csharp-tree-sitter-mode    . csharp-tree-sitter-indent-offset) ; C#
    (d-mode                     . c-basic-offset)             ; D
    (julia-mode                 . c-basic-offset)             ; Julia
    (java-mode                  . c-basic-offset)             ; Java
    (java-ts-mode               . java-ts-mode-indent-offset) ; Java
    (jde-mode                   . c-basic-offset)     ; Java (JDE)
    (js-mode                    . js-indent-level)    ; JavaScript
    (js2-mode                   . js2-basic-offset)   ; JavaScript-IDE
    (js3-mode                   . js3-indent-level)   ; JavaScript-IDE
    (json-mode                  . js-indent-level)    ; JSON
    (json-ts-mode               . js-indent-level)    ; JSON
    (lua-mode                   . lua-indent-level)   ; Lua
    (objc-mode                  . c-basic-offset)     ; Objective C
    (php-mode                   . c-basic-offset)     ; PHP
    (perl-mode                  . perl-indent-level)  ; Perl
    (cperl-mode                 . cperl-indent-level) ; Perl
    (raku-mode                  . raku-indent-offset) ; Perl6/Raku
    (erlang-mode                . erlang-indent-level)     ; Erlang
    (ada-mode                   . ada-indent)              ; Ada
    (sgml-mode                  . sgml-basic-offset)       ; SGML
    (nxml-mode                  . nxml-child-indent)       ; XML
    (pascal-mode                . pascal-indent-level)     ; Pascal
    (typescript-mode            . typescript-indent-level) ; Typescript
    (typescript-ts-mode         . typescript-ts-mode-indent-offset) ; Typescript
    (tsx-ts-mode                . typescript-ts-mode-indent-offset) ; Typescript[TSX]
    (sh-mode                    . sh-basic-offset)   ; Shell Script
    (ruby-mode                  . ruby-indent-level) ; Ruby
    (enh-ruby-mode              . enh-ruby-indent-level) ; Ruby
    (crystal-mode               . crystal-indent-level) ; Crystal (Ruby)
    (css-mode                   . css-indent-offset)    ; CSS
    (rust-mode                  . rust-indent-offset)   ; Rust
    (rust-ts-mode               . rust-ts-mode-indent-offset) ; Rust
    (rustic-mode                . rustic-indent-offset)       ; Rust
    (scala-mode                 . scala-indent:step)          ; Scala
    (powershell-mode            . powershell-indent)  ; PowerShell
    (ess-mode                   . ess-indent-offset)  ; ESS (R)
    (yaml-mode                  . yaml-indent-offset) ; YAML
    (hack-mode                  . hack-indent-offset) ; Hack
    (kotlin-mode                . c-basic-offset)     ; Kotlin
    (vhdl-mode                  . vhdl-basic-offset)  ; VHDL
    (default                    . standard-indent)) ; default fallback
  "A mapping from `major-mode' to its indent variable.")

(defcustom lsp-bridge-string-interpolation-open-chars-alist
  '(;; For {}
    (python-mode . "[^\$]\{")
    (python-ts-mode . "[^\$]\{")
    ;; For ${}
    (js-mode . "\$\{")
    (js-ts-mode . "\$\{")
    (js2-mode . "\$\{")
    (js3-mode . "\$\{")
    (typescript-mode . "\$\{")
    (typescript-ts-mode . "\$\{")
    (sh-mode . "\$\{")
    (bash-mode . "\$\{")
    (bash-ts-mode . "\$\{")
    (elixir-mode . "\#\{")
    (elixir-ts-mode . "\#\{")
    ;; For #{}
    (ruby-mode . "\#\{")
    ;; For {{}}
    (yaml-mode . "\{\{"))
  "Open characters for string interpolation. The elements are cons cell (major-mode . open-char-regexp)"
  :type 'cons)

(defun lsp-bridge--get-indent-width (mode)
  "Get indentation offset for MODE."
  (or (alist-get mode lsp-bridge-formatting-indent-alist)
      (lsp-bridge--get-indent-width (or (get mode 'derived-mode-parent) 'default))))

(cl-defmacro lsp-bridge--with-file-buffer (dirname &rest body)
  "Evaluate BODY in buffer with FILEPATH."
  (declare (indent 1))
  `(when-let ((buffer (lsp-bridge-get-match-buffer ,dirname)))
     (with-current-buffer buffer
       ,@body)))

(cl-defmacro lsp-bridge-save-position (&rest body)
  "`save-excursion' not enough for LSP code format.
So we build this macro to restore postion after code format."
  `(let* ((current-buf (current-buffer))
          (current-line (line-number-at-pos))
          (current-column (lsp-bridge--calculate-column))
          (indent-column (save-excursion
                           (back-to-indentation)
                           (lsp-bridge--calculate-column))))
     ,@body
     (switch-to-buffer current-buf)
     (goto-line current-line)
     (back-to-indentation)
     (forward-char (max (- current-column indent-column) 0))))

(defun lsp-bridge-get-match-buffer (dirname)
  (cl-dolist (buffer (buffer-list))
    (when-let* ((file-name (buffer-file-name buffer))
                (match-buffer (or (string-equal file-name dirname)
                                  (string-equal (file-truename file-name) dirname))))
      (cl-return buffer))))

(defun lsp-bridge--get-project-path-func (dirname)
  (when lsp-bridge-get-project-path-by-filepath
    (funcall lsp-bridge-get-project-path-by-filepath dirname)))

(defun lsp-bridge--get-workspace-folder-func (project-path)
  (when lsp-bridge-get-workspace-folder
    (funcall lsp-bridge-get-workspace-folder project-path)))

(defun lsp-bridge--get-multi-lang-server-func (project-path dirname)
  "Get lang server with project path, file path or file extension."
  (let (lang-server-by-project
        lang-server-by-extension)
    ;; Step 1: Search lang server base on project rule provide by `lsp-bridge-get-multi-lang-server-by-project'.
    (when lsp-bridge-get-multi-lang-server-by-project
      (setq lang-server-by-project (funcall lsp-bridge-get-multi-lang-server-by-project project-path dirname)))

    (if lang-server-by-project
        lang-server-by-project
      ;; Step 2: search lang server base on extension rule provide by `lsp-bridge-multi-lang-server-extension-list'.
      (setq lang-server-by-extension (lsp-bridge-get-multi-lang-server-by-extension dirname))
      (if lang-server-by-extension
          lang-server-by-extension
        ;; Step 3: search lang server base on mode rule provide by `lsp-bridge-multi-lang-server-extension-list'.
        (lsp-bridge--with-file-buffer dirname
          (lsp-bridge-get-multi-lang-server-by-mode))))))

(defun lsp-bridge--get-single-lang-server-func (project-path dirname)
  "Get lang server with project path, file path or file extension."
  (let (lang-server-by-project
        lang-server-by-extension)
    ;; Step 1: Search lang server base on project rule provide by `lsp-bridge-get-single-lang-server-by-project'.
    (when lsp-bridge-get-single-lang-server-by-project
      (setq lang-server-by-project (funcall lsp-bridge-get-single-lang-server-by-project project-path dirname)))

    (if lang-server-by-project
        lang-server-by-project
      ;; Step 2: search lang server base on extension rule provide by `lsp-bridge-single-lang-server-extension-list'.
      (setq lang-server-by-extension (lsp-bridge-get-single-lang-server-by-extension dirname))
      (if lang-server-by-extension
          lang-server-by-extension
        ;; Step 3: search lang server base on mode rule provide by `lsp-bridge-single-lang-server-extension-list'.
        (lsp-bridge--with-file-buffer dirname
          (lsp-bridge-get-single-lang-server-by-mode))))))

(defun lsp-bridge--user-emacs-directory ()
  "Get lang server with project path, file path or file extension."
  (expand-file-name user-emacs-directory))

(defun lsp-bridge--get-buffer-content-func (buffer-name)
  "Get buffer content for lsp. BUFFER-NAME is name eval from (buffer-name)."
  (when-let* ((buf (get-buffer buffer-name)))
    (if (and lsp-bridge-enable-org-babel
             (eq major-mode 'org-mode))
        (and lsp-bridge--org-babel-info-cache (org-element-property :value lsp-bridge--org-babel-info-cache))
      (with-current-buffer buf
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun lsp-bridge-get-lang-server-by-extension (dirname extension-list)
  "Get lang server for file extension."
  (when-let* ((file-extension (file-name-extension dirname))
              (langserver-info (cl-find-if
                                (lambda (pair)
                                  (let ((extension (car pair)))
                                    (if (eq (type-of extension) 'string)
                                        (string-equal file-extension extension)
                                      (member file-extension extension))))
                                extension-list)))
    (cdr langserver-info)))

(defun lsp-bridge-get-multi-lang-server-by-extension (dirname)
  "Get lang server for file extension."
  (lsp-bridge-get-lang-server-by-extension dirname lsp-bridge-multi-lang-server-extension-list))

(defun lsp-bridge-get-single-lang-server-by-extension (dirname)
  "Get lang server for file extension."
  (lsp-bridge-get-lang-server-by-extension dirname lsp-bridge-single-lang-server-extension-list))

(defun lsp-bridge-lang-server-by-mode (mode-list)
  "Get lang server for file mode."
  (cl-find-if
   (lambda (pair)
     (let ((mode (car pair)))
       (if (symbolp mode)
           (eq major-mode mode)
         (member major-mode mode))))
   mode-list))

(defun lsp-bridge-get-symbol-string-value (info)
  (pcase (format "%s" (type-of info))
    ("string" info)
    ("symbol" (symbol-value info))
    ))

(defun lsp-bridge-get-multi-lang-server-by-mode ()
  "Get lang server for file mode."
  (when-let ((langserver-info (lsp-bridge-lang-server-by-mode lsp-bridge-multi-lang-server-mode-list)))
    (lsp-bridge-get-symbol-string-value (cdr langserver-info))))

(defun lsp-bridge-get-single-lang-server-by-mode ()
  "Get lang server for file mode."
  (let ((langserver-info (lsp-bridge-lang-server-by-mode lsp-bridge-single-lang-server-mode-list)))
    (cond (langserver-info
           (lsp-bridge-get-symbol-string-value (cdr langserver-info)))
          ((eq major-mode 'org-mode)
           (cond
            (lsp-bridge-use-wenls-in-org-mode
             "wen")
            (lsp-bridge-use-ds-pinyin-in-org-mode
             "ds-pinyin")
            (lsp-bridge-enable-org-babel
             ;; get lang server according to org babel
             (let* ((lang (org-element-property :language lsp-bridge--org-babel-info-cache))
                    (lang-name (symbol-name (cdr (assoc lang org-src-lang-modes))))
                    (mode-name (concat lang-name "-mode"))
                    (major-mode (intern mode-name)))
               (setq-local acm-is-elisp-mode-in-org (eq major-mode 'emacs-lisp-mode))
               ;; if `lsp-bridge-org-babel-lang-list' is set, only enable lsp when lang in it
               (when (or (eq lsp-bridge-org-babel-lang-list nil)
                         (member lang-name lsp-bridge-org-babel-lang-list))
                 (lsp-bridge-get-single-lang-server-by-mode)))))))))

(defvar-local lsp-bridge--org-update-file-before-change nil)
(defun lsp-bridge-check-org-babel-lsp-server ()
  "Check if current point is in org babel block. "
  (if (and lsp-bridge--org-babel-block-bop lsp-bridge--org-babel-block-eop lsp-bridge--org-babel-info-cache
           (>= (point) lsp-bridge--org-babel-block-bop) (<= (point) lsp-bridge--org-babel-block-eop))
      lsp-bridge--org-babel-info-cache
    (setq-local lsp-bridge--org-babel-info-cache (org-element-context))
    (if (not (eq (org-element-type lsp-bridge--org-babel-info-cache) 'src-block))
        (setq-local lsp-bridge--org-babel-info-cache nil)
      (save-excursion
        (goto-char (org-element-property :begin lsp-bridge--org-babel-info-cache))
        (setq-local lsp-bridge--org-babel-block-bop (1+ (point-at-eol))))
      (setq-local lsp-bridge--org-babel-block-eop (+ lsp-bridge--org-babel-block-bop
                                                     (length (org-element-property :value lsp-bridge--org-babel-info-cache))))
      ;; sync it in `lsp-bridge-monitor-before-change'
      (setq-local lsp-bridge--org-update-file-before-change t)))

  (and lsp-bridge--org-babel-info-cache
       (lsp-bridge-get-single-lang-server-by-mode)))

(defun lsp-bridge-has-lsp-server-p ()
  (cond ((and lsp-bridge-enable-org-babel (eq major-mode 'org-mode))
         (setq-local acm-is-elisp-mode-in-org nil)
         (lsp-bridge-check-org-babel-lsp-server))
        (t
         (when-let* ((dirname (ignore-errors (file-truename buffer-file-name))))
           (let* ((multi-lang-server-by-extension (or (lsp-bridge-get-multi-lang-server-by-extension dirname)
                                                      (lsp-bridge--with-file-buffer dirname
                                                        (lsp-bridge-get-multi-lang-server-by-mode))))
                  (lang-server-by-extension (or (lsp-bridge-get-single-lang-server-by-extension dirname)
                                                (lsp-bridge--with-file-buffer dirname
                                                  (lsp-bridge-get-single-lang-server-by-mode)))))
             (if multi-lang-server-by-extension
                 multi-lang-server-by-extension
               lang-server-by-extension)
             )))))

(defun lsp-bridge-call-async (method &rest args)
  "Call Python EPC function METHOD and ARGS asynchronously."
  (lsp-bridge-deferred-chain
    (lsp-bridge-epc-call-deferred lsp-bridge-epc-process (read method) args)))

(defvar-local lsp-bridge-buffer-file-deleted nil)

(defun lsp-bridge-call-file-api-p ()
  (and lsp-bridge-mode
       (lsp-bridge-has-lsp-server-p)
       (lsp-bridge-epc-live-p lsp-bridge-epc-process)))

(defun lsp-bridge-call-file-api (method &rest args)
  (when (lsp-bridge-call-file-api-p)
    (if (and (boundp 'acm-backend-lsp-filepath)
             (file-exists-p acm-backend-lsp-filepath))
        (if lsp-bridge-buffer-file-deleted
            ;; If buffer's file create again (such as switch branch back), we need save buffer first,
            ;; send the LSP request after the file is changed next time.
            (progn
              (save-buffer)
              (setq-local lsp-bridge-buffer-file-deleted nil)
              (message "[LSP-Bridge] %s is back, will send the LSP request after the file is changed next time." acm-backend-lsp-filepath))
          (when (and acm-backend-lsp-filepath
                     (not (string-equal acm-backend-lsp-filepath "")))
            (lsp-bridge-deferred-chain
              (lsp-bridge-epc-call-deferred lsp-bridge-epc-process (read method) (append (list acm-backend-lsp-filepath) args)))))
      ;; We need send `closeFile' request to lsp server if we found buffer's file is not exist,
      ;; it is usually caused by switching branch or other tools to delete file.
      ;;
      ;; We won't send any lsp request until buffer's file create again.
      (unless lsp-bridge-buffer-file-deleted
        (lsp-bridge-close-buffer-file)
        (setq-local lsp-bridge-buffer-file-deleted t)
        (message "[LSP-Bridge] %s is not exist, stop send the LSP request until file create again." acm-backend-lsp-filepath)))))

(defvar lsp-bridge-is-starting nil)

(defun lsp-bridge-restart-process ()
  "Stop and restart LSP-Bridge process."
  (interactive)
  (lsp-bridge-diagnostic-hide-overlays)

  (setq lsp-bridge-is-starting nil)

  (lsp-bridge-kill-process)
  (lsp-bridge-start-process)
  (message "[LSP-Bridge] Process restarted."))

(defun lsp-bridge-profile-dump ()
  (interactive)
  (lsp-bridge-call-async "profile_dump"))

(defun lsp-bridge-start-process ()
  "Start LSP-Bridge process if it isn't started."
  (setq lsp-bridge-is-starting t)
  (unless (lsp-bridge-epc-live-p lsp-bridge-epc-process)
    ;; start epc server and set `lsp-bridge-server-port'
    (lsp-bridge--start-epc-server)
    (let* ((lsp-bridge-args (append
                             (list lsp-bridge-python-file)
                             (list (number-to-string lsp-bridge-server-port))
                             (when lsp-bridge-enable-profile
                               (list "profile"))
                             )))

      ;; Set process arguments.
      (if lsp-bridge-enable-debug
          (progn
            (setq lsp-bridge-internal-process-prog "gdb")
            (setq lsp-bridge-internal-process-args (append (list "-batch" "-ex" "run" "-ex" "bt" "--args" lsp-bridge-python-command) lsp-bridge-args)))
        (setq lsp-bridge-internal-process-prog lsp-bridge-python-command)
        (setq lsp-bridge-internal-process-args lsp-bridge-args))

      ;; Start python process.
      (let ((process-connection-type (not (lsp-bridge--called-from-wsl-on-windows-p))))
        (setq lsp-bridge-internal-process
              (apply 'start-process
                     lsp-bridge-name lsp-bridge-name
                     lsp-bridge-internal-process-prog lsp-bridge-internal-process-args)))
      (set-process-query-on-exit-flag lsp-bridge-internal-process nil))))

(defun lsp-bridge--called-from-wsl-on-windows-p ()
  "Check whether lsp-bridge is called by Emacs on WSL and is running on Windows."
  (and (eq system-type 'gnu/linux)
       (string-match-p ".exe" lsp-bridge-python-command)))

(defvar lsp-bridge-stop-process-hook nil)

(defun lsp-bridge-kill-process ()
  "Stop LSP-Bridge process and kill all LSP-Bridge buffers."
  (interactive)

  ;; Run stop process hooks.
  (run-hooks 'lsp-bridge-stop-process-hook)

  ;; Kill process after kill buffer, make application can save session data.
  (lsp-bridge--kill-python-process))

(add-hook 'kill-emacs-hook #'lsp-bridge-kill-process)

(defun lsp-bridge--kill-python-process ()
  "Kill LSP-Bridge background python process."
  (when (lsp-bridge-epc-live-p lsp-bridge-epc-process)
    ;; Cleanup before exit LSP-Bridge server process.
    (lsp-bridge-call-async "cleanup")
    ;; Delete LSP-Bridge server process.
    (lsp-bridge-epc-stop-epc lsp-bridge-epc-process)
    ;; Kill *lsp-bridge* buffer.
    (when (get-buffer lsp-bridge-name)
      (kill-buffer lsp-bridge-name))
    (setq lsp-bridge-epc-process nil)
    (message "[LSP-Bridge] Process terminated.")))

(defun lsp-bridge--first-start (lsp-bridge-epc-port)
  "Call `lsp-bridge--open-internal' upon receiving `start_finish' signal from server."
  ;; Make EPC process.
  (setq lsp-bridge-epc-process (make-lsp-bridge-epc-manager
                                :server-process lsp-bridge-internal-process
                                :commands (cons lsp-bridge-internal-process-prog lsp-bridge-internal-process-args)
                                :title (mapconcat 'identity (cons lsp-bridge-internal-process-prog lsp-bridge-internal-process-args) " ")
                                :port lsp-bridge-epc-port
                                :connection (lsp-bridge-epc-connect "localhost" lsp-bridge-epc-port)
                                ))
  (lsp-bridge-epc-init-epc-layer lsp-bridge-epc-process)
  (setq lsp-bridge-is-starting nil)

  ;; Search words from opened files.
  (lsp-bridge-search-words-index-files)

  ;; Synchronize elisp symbol to Python side.
  (lsp-bridge-elisp-symbols-update))

(defvar-local lsp-bridge-last-cursor-position 0)
(defvar-local lsp-bridge-prohibit-completion nil)

(defun lsp-bridge-monitor-pre-command ()
  (when acm-filter-overlay
    (let ((this-command-string (format "%s" this-command)))
      (cond ((member this-command-string '("self-insert-command" "org-self-insert-command"))
             (setq this-command 'acm-filter-insert-char)
             (setq last-command 'acm-filter-insert-char))
            ((member this-command-string '("delete-block-backward" "grammatical-edit-backward-delete"))
             (setq this-command 'acm-filter-delete-char)
             (setq last-command 'acm-filter-delete-char))
            ))))

(defun lsp-bridge-monitor-post-command ()
  (let ((this-command-string (format "%s" this-command)))
    (when (and lsp-bridge-mode
               (member this-command-string '("self-insert-command" "org-self-insert-command")))
      (lsp-bridge-try-completion))

    (when (lsp-bridge-has-lsp-server-p)
      (unless (equal lsp-bridge-last-cursor-position
                     (setq-local lsp-bridge-last-cursor-position (point)))
        ;; Only show hover when cursor move.
        (when (and lsp-bridge-enable-hover-diagnostic
                   (not (member this-command-string
                                '("self-insert-command" "org-self-insert-command"
                                  "lsp-bridge-diagnostic-jump-next" "lsp-bridge-diagnostic-jump-prev"))))
          (lsp-bridge-diagnostic-maybe-display-error-at-point))

        ;; Only send `change_cursor' request when user change cursor, except cause by mouse wheel.
        (unless (eq last-command 'mwheel-scroll)
          (lsp-bridge-call-file-api "change_cursor" (lsp-bridge--position))
          (if (and lsp-bridge-symbols-enable-which-func
                   (featurep 'which-func) which-function-mode)
              (lsp-bridge-call-file-api "document_symbol" (lsp-bridge--position)))))

      ;; Hide hover tooltip.
      (unless (string-prefix-p "lsp-bridge-popup-documentation-scroll" this-command-string)
        (lsp-bridge-hide-doc-tooltip))

      ;; Hide diagnostic tooltip.
      (unless (member this-command-string '("lsp-bridge-diagnostic-jump-next"
                                            "lsp-bridge-diagnostic-jump-prev"))
        (lsp-bridge-diagnostic-hide-tooltip))

      ;; Hide signature tooltip.
      (lsp-bridge-hide-signature-tooltip)

      ;; Hide code action frame when Emacs got focus.
      (unless (string-prefix-p "lsp-bridge-code-action" this-command-string)
        (unless (member this-command-string '("handle-switch-frame"))
          (ignore-errors
            (lsp-bridge-code-action-popup-quit))))
      )))

(defun lsp-bridge-close-buffer-file ()
  (when (and (lsp-bridge-has-lsp-server-p)
             (lsp-bridge-epc-live-p lsp-bridge-epc-process)
             (boundp 'acm-backend-lsp-filepath))
    (lsp-bridge-call-async "close_file" acm-backend-lsp-filepath))

  (when (and buffer-file-name
             (lsp-bridge-epc-live-p lsp-bridge-epc-process))
    (lsp-bridge-call-async "search_file_words_close_file" buffer-file-name)))

(defun lsp-bridge-set-prefix-style (prefix-style)
  ;; Wen LSP server need `acm-get-input-prefix-bound' return ASCII keyword prefix,
  ;; other LSP server need use `bounds-of-thing-at-point' of symbol as keyword prefix.
  (setq-local acm-input-bound-style prefix-style))

(defun lsp-bridge-set-server-names (dirname server-names)
  (lsp-bridge--with-file-buffer dirname
    (setq-local acm-backend-lsp-server-names server-names)))

(defun lsp-bridge-completion--record-items (dirname
                                            candidates position
                                            server-name
                                            completion-trigger-characters
                                            server-names)
  (lsp-bridge--with-file-buffer dirname
    ;; Save completion items.
    (setq-local acm-backend-lsp-completion-position position)
    (setq-local acm-backend-lsp-completion-trigger-characters completion-trigger-characters)
    (setq-local acm-backend-lsp-server-names server-names)
    (setq-local acm-backend-lsp-fetch-completion-item-ticker nil)

    (let* ((lsp-items acm-backend-lsp-items)
           (completion-table (make-hash-table :test 'equal)))
      (dolist (item candidates)
        (plist-put item :annotation (capitalize (plist-get item :icon)))
        (puthash (plist-get item :key) item completion-table))
      (puthash server-name completion-table lsp-items)
      (setq-local acm-backend-lsp-items lsp-items))
    (lsp-bridge-try-completion)))

(defun lsp-bridge-try-completion ()
  (cond (lsp-bridge-prohibit-completion
         (setq-local lsp-bridge-prohibit-completion nil))
        (t
         ;; Don't popup completion menu when `lsp-bridge-last-change-position' (cursor before send completion request) is not equal current cursor position.
         (if (equal lsp-bridge-last-change-position
                    (list (current-buffer) (buffer-chars-modified-tick) (point)))
             ;; Try popup completion frame.
             (if (cl-every (lambda (pred)
                             (if (functionp pred) (funcall pred) t))
                           lsp-bridge-completion-popup-predicates)
                 (progn
                   (acm-template-candidate-init)
                   (acm-update))
               (acm-hide))))))

(defun lsp-bridge-popup-complete-menu ()
  (interactive)
  (acm-update))

(defun lsp-bridge-not-match-stop-commands ()
  "Hide completion if `lsp-bridge-last-change-command' match commands in `lsp-bridge-completion-stop-commands'."
  (not (or (member lsp-bridge-last-change-command lsp-bridge-completion-stop-commands)
           (member (format "%s" last-command) lsp-bridge-completion-stop-commands))))

(defun lsp-bridge-string-interpolation-p (string-interpolation-open-chars-alist)
  "Check if the cursor position is subject to string interpolation"
  (when-let* ((search-char (cdr (assoc (buffer-local-value 'major-mode (current-buffer))
                                       string-interpolation-open-chars-alist)))
              (open-pos (save-excursion (search-backward-regexp search-char nil t))))
    (not (save-excursion (search-backward-regexp "\}" open-pos t)))))

(defun lsp-bridge-not-in-string ()
  "Hide completion if cursor in string area."
  (or
   ;; Allow sdcv completion in string area
   acm-enable-search-sdcv-words
   ;; Allow volar popup completion menu in string.
   (and (boundp 'acm-backend-lsp-filepath)
        acm-backend-lsp-filepath
        (string-suffix-p ".vue" acm-backend-lsp-filepath))
   ;; Other language not allowed popup completion in string, it's annoy
   (not (acm-in-string-p))
   ;; Allow popup completion menu for string interpolation
   (lsp-bridge-string-interpolation-p lsp-bridge-string-interpolation-open-chars-alist)
   ;; Allow file path completion in string area
   (ignore-errors
     (and (thing-at-point 'filename)
          (file-exists-p (file-name-directory (thing-at-point 'filename)))))))

(defun lsp-bridge-not-execute-macro ()
  "Hide completion during executing macros."
  (not executing-kbd-macro))

(defun lsp-bridge-not-in-mark-macro ()
  "Hide completion markmacro enable."
  (not (and (featurep 'markmacro)
            markmacro-overlays)))

(defun lsp-bridge-not-follow-complete ()
  "Hide completion if last command is `acm-complete'."
  (or (not (member (format "%s" last-command) '("acm-complete" "acm-complete-quick-access")))
      (member (format "%s" this-command) '("self-insert-command" "org-self-insert-command"))))

(defun lsp-bridge-not-only-blank-before-cursor ()
  "Hide completion if only blank before cursor."
  (split-string (buffer-substring-no-properties
                 (max (1- (point)) (line-beginning-position))
                 (point))))

(defun lsp-bridge-not-match-hide-characters ()
  "Hide completion if char before cursor match `lsp-bridge-completion-hide-characters'."
  (let ((char (ignore-errors (char-to-string (char-before)))))
    (or (and lsp-bridge-completion-obey-trigger-characters-p
             (member char (if (boundp 'acm-backend-lsp-completion-trigger-characters)
                              (symbol-value 'acm-backend-lsp-completion-trigger-characters))))
        (not (member char lsp-bridge-completion-hide-characters)))))

(defun lsp-bridge-is-evil-state ()
  "If `evil' mode is enable, only show completion when evil is in insert mode."
  (or (not (featurep 'evil))
      (evil-insert-state-p)
      (evil-emacs-state-p)))

(defun lsp-bridge-is-meow-state ()
  "If `meow' mode is enable, only show completion when meow is in insert mode."
  (or (not (featurep 'meow))
      meow-insert-mode))

(defun lsp-bridge-not-in-multiple-cursors ()
  "If `multiple-cursors' mode is enable, hide completion menu."
  (not (and (featurep 'multiple-cursors)
            multiple-cursors-mode)))

(defun lsp-bridge-not-complete-manually ()
  "If `lsp-bridge-complete-manually' is non-nil, hide completion menu."
  (or
   ;; Always update candidate if completion menu is visible now.
   (acm-frame-visible-p acm-menu-frame)
   ;; Don't update candidate if `lsp-bridge-complete-manually' is non-nil.
   (not lsp-bridge-complete-manually)))

(defun lsp-bridge-not-in-org-table ()
  (not (and (boundp 'org-at-table-p)
            (org-at-table-p))))

(defun lsp-bridge--point-position (pos)
  "Get position of POS."
  (save-excursion
    (goto-char pos)
    (lsp-bridge--position)))

(defun lsp-bridge--calculate-column ()
  "Calculate character offset of cursor in current line."
  (/ (- (length (encode-coding-region (line-beginning-position)
                                      (min (point) (point-max)) 'utf-16 t))
        2)
     2))

(defun lsp-bridge--position ()
  "Get position of cursor."
  (list :line (1- (line-number-at-pos)) :character (lsp-bridge--calculate-column)))

(defvar-local lsp-bridge--before-change-begin-pos nil)
(defvar-local lsp-bridge--before-change-end-pos nil)

(defun lsp-bridge-monitor-before-change (begin end)
  (when (lsp-bridge-has-lsp-server-p)
    ;; send whole org src block to lsp server
    (when (and lsp-bridge-enable-org-babel (eq major-mode 'org-mode)
               lsp-bridge--org-babel-block-bop
               lsp-bridge--org-update-file-before-change)
      (setq-local lsp-bridge--org-update-file-before-change nil)
      (lsp-bridge-call-file-api "update_file" (buffer-name)
                                (1- (line-number-at-pos lsp-bridge--org-babel-block-bop))))

    (setq-local lsp-bridge--before-change-begin-pos (lsp-bridge--point-position begin))
    (setq-local lsp-bridge--before-change-end-pos (lsp-bridge--point-position end))))

(defun lsp-bridge-monitor-post-self-insert ()
  ;; Make sure this function be called after `electric-pair-mode'
  ;; `smartparens-mode' or something similar if they are enabled.
  ;;
  ;; Because pairing parethness or quotes will call `after-change-functions'
  ;; multiple times. For example: (| represents the cursor)
  ;;
  ;;         dic|
  ;;
  ;; Press `[', then the following situation will be observed in
  ;; `lsp-bridge-monitor-after-change':
  ;;
  ;;         dic[|
  ;;         dic|
  ;;         dic[|
  ;;         dic[]|
  ;;
  ;; The last position of cursor is wrong, that makes a misjudgment in
  ;; `lsp-bridge-try-completion'.
  (setq lsp-bridge-last-change-position
        (list (current-buffer) (buffer-chars-modified-tick) (point))))

(defun lsp-bridge-monitor-after-change (begin end length)
  (unless lsp-bridge-revert-buffer-flag
    (let ((this-command-string (format "%s" this-command)))
      ;; Record last command to `lsp-bridge-last-change-command'.
      (setq lsp-bridge-last-change-command (format "%s" this-command))

      ;; Record last change position to avoid popup outdate completions.
      (setq lsp-bridge-last-change-position (list (current-buffer) (buffer-chars-modified-tick) (point)))

      ;; estimate org block end point according change length
      (when (and lsp-bridge-enable-org-babel (eq major-mode 'org-mode)
                 (> begin lsp-bridge--org-babel-block-bop)
                 (< begin lsp-bridge--org-babel-block-eop))
        (setq-local lsp-bridge--org-babel-block-eop
                    (- lsp-bridge--org-babel-block-eop length (- begin end))))

      ;; Send change_file request to trigger LSP completion.
      (when (lsp-bridge-call-file-api-p)
        (lsp-bridge-call-file-api "change_file"
                                  lsp-bridge--before-change-begin-pos
                                  lsp-bridge--before-change-end-pos
                                  length
                                  (buffer-substring-no-properties begin end)
                                  (lsp-bridge--position)
                                  (acm-char-before)
                                  (buffer-name)
                                  (acm-get-input-prefix)))

      (when (and (lsp-bridge-epc-live-p lsp-bridge-epc-process)
                 ;; NOTE:
                 ;;
                 ;; Most org-mode commands will make lsp-bridge failed that casue (thing-at-point 'symbol t).
                 ;; We only allow `org-self-insert-command' trigger lsp-bridge action.
                 (not (and (or (string-prefix-p "org-" this-command-string)
                               (string-prefix-p "+org/" this-command-string))
                           (not (string-equal this-command-string "org-self-insert-command")))))
        (let* ((current-word (thing-at-point 'word t))
               (current-symbol (thing-at-point 'symbol t)))
          ;; TabNine search.
          (when acm-enable-tabnine
            (lsp-bridge-tabnine-complete))

          ;; Search sdcv dictionary.
          (when acm-enable-search-sdcv-words
            ;; Search words if current prefix is not empty.
            (unless (or (string-equal current-word "") (null current-word))
              (lsp-bridge-call-async "search_sdcv_words_search" current-word)))

          ;; Search elisp symbol.
          (lsp-bridge-elisp-symbols-search current-symbol)

          ;; Send change file to search-words backend.
          (unless lsp-bridge-prohibit-completion
            (when buffer-file-name
              (let ((current-word (acm-backend-search-file-words-get-point-string)))
                ;; Search words if current prefix is not empty.
                (unless (or (string-equal current-word "") (null current-word))
                  (lsp-bridge-call-async "search_file_words_search" current-word)))))

          ;; Send tailwind keyword search request just when cursor in class area.
          (when (and (derived-mode-p 'web-mode)
                     (acm-in-string-p)
                     (save-excursion
                       (search-backward-regexp "class=" (point-at-bol) t)))
            (unless (or (string-equal current-symbol "") (null current-symbol))
              (lsp-bridge-call-async "search_tailwind_keywords_search" buffer-file-name current-symbol)))

          ;; Send path search request when detect path string.
          (if (acm-in-string-p)
              (when-let* ((filename (thing-at-point 'filename t))
                          (dirname (ignore-errors (expand-file-name (file-name-directory filename)))))
                (when (file-exists-p dirname)
                  (lsp-bridge-call-async "search_paths_search"
                                         dirname
                                         (file-name-base filename)
                                         )))
            ;; We need cleanup `acm-backend-path-items' when cursor not in string.
            ;; Otherwise, other completion backend won't show up.
            (setq-local acm-backend-path-items nil))
          )))))

(defun lsp-bridge-elisp-symbols-update ()
  "We need synchronize elisp symbols to Python side when idle."
  (when (lsp-bridge-epc-live-p lsp-bridge-epc-process)
    (let* ((symbols (acm-backend-elisp-get-symbols))
           (symbols-size (length symbols)))
      ;; Only synchronize when new symbol created.
      (unless (equal acm-backend-elisp-symbols-update-size symbols-size)
        (lsp-bridge-call-async "search_list_update"
                               "elisp"
                               symbols
                               acm-backend-elisp-search-max-number
                               "lsp-bridge-elisp-symbols-record")
        (setq acm-backend-elisp-symbols-update-size symbols-size)))))

(defun lsp-bridge-elisp-symbols-search (current-symbol)
  (when (and (acm-is-elisp-mode-p)
             (not (acm-in-comment-p)))
    ;; Search words if current prefix is not empty.
    (unless (or (string-equal current-symbol "") (null current-symbol))
      (lsp-bridge-call-async "search_list_search" "elisp" current-symbol))))

(defun lsp-bridge-elisp-symbols-record (candidates)
  (setq-local acm-backend-elisp-items candidates)
  (lsp-bridge-try-completion))

(defun lsp-bridge-search-words-index-files ()
  "Index files when lsp-bridge python process finish."
  (let ((files (cl-remove-if (lambda (elt)
                               (or (null elt)
                                   (member (file-name-extension elt)
                                           lsp-bridge-search-words-prohibit-file-extensions)))
                             (mapcar #'buffer-file-name (buffer-list)))))
    (lsp-bridge-call-async "search_file_words_index_files" files)))

(defun lsp-bridge-search-words-update ()
  (when (and buffer-file-name
             (lsp-bridge-epc-live-p lsp-bridge-epc-process)
             (not (member (file-name-extension buffer-file-name)
                          lsp-bridge-search-words-prohibit-file-extensions)))
    (lsp-bridge-call-async "search_file_words_change_file"
                           buffer-file-name
                           (base64-encode-string (encode-coding-string (buffer-string) 'utf-8))
                           )))

(defun lsp-bridge-search-words-rebuild-cache ()
  "Rebuild words cache when idle."
  (when (lsp-bridge-epc-live-p lsp-bridge-epc-process)
    ;; Update file search words when idle.
    (lsp-bridge-search-words-update)

    (unless (eq last-command 'mwheel-scroll)
      (lsp-bridge-call-async "search_file_words_rebuild_cache"))))

(defun lsp-bridge-completion-ui-visible-p ()
  (acm-frame-visible-p acm-menu-frame))

(defun lsp-bridge-monitor-after-save ()
  (lsp-bridge-call-file-api "save_file" (buffer-name)))

(defvar-local lsp-bridge-jump-to-def-in-other-window nil)

(defun lsp-bridge-find-def ()
  (interactive)
  (setq-local lsp-bridge-jump-to-def-in-other-window nil)
  (lsp-bridge-call-file-api "find_define" (lsp-bridge--position)))

(defun lsp-bridge-find-def-other-window ()
  (interactive)
  (setq-local lsp-bridge-jump-to-def-in-other-window t)
  (lsp-bridge-call-file-api "find_define" (lsp-bridge--position)))

(defun lsp-bridge-find-def-return ()
  "Pop off lsp-bridge-mark-ring and jump to the top location."
  (interactive)
  ;; Pop entries that refer to non-existent buffers.
  (while (and lsp-bridge-mark-ring (not (marker-buffer (car lsp-bridge-mark-ring))))
    (setq-local lsp-bridge-mark-ring (cdr lsp-bridge-mark-ring)))
  (or lsp-bridge-mark-ring
      (error "[LSP-Bridge] No lsp-bridge mark set"))
  (let* ((this-buffer (current-buffer))
         (marker (pop lsp-bridge-mark-ring))
         (buffer (marker-buffer marker))
         (position (marker-position marker)))
    (set-buffer buffer)
    (or (and (>= position (point-min))
             (<= position (point-max)))
        (if widen-automatically
            (widen)
          (error "[LSP-Bridge] mark position is outside accessible part of buffer %s"
                 (buffer-name buffer))))
    (goto-char position)
    (unless (equal buffer this-buffer)
      (switch-to-buffer buffer))))

(defun lsp-bridge-find-impl ()
  (interactive)
  (setq-local lsp-bridge-jump-to-def-in-other-window nil)
  (lsp-bridge-call-file-api "find_implementation" (lsp-bridge--position)))

(defun lsp-bridge-find-impl-other-window ()
  (interactive)
  (setq-local lsp-bridge-jump-to-def-in-other-window t)
  (lsp-bridge-call-file-api "find_implementation" (lsp-bridge--position)))

(defun lsp-bridge-find-references ()
  (interactive)
  (lsp-bridge-call-file-api "find_references" (lsp-bridge--position)))

(defun lsp-bridge-references--popup (references-content references-counter)
  (if (> references-counter 0)
      (progn
        (lsp-bridge-ref-popup references-content references-counter)
        (message "[LSP-Bridge] Found %s references" references-counter))
    (message "[LSP-Bridge] No references found.")))

(defun lsp-bridge-rename ()
  (interactive)
  (lsp-bridge-call-file-api "prepare_rename" (lsp-bridge--position))
  (let ((new-name (read-string "Rename to: " (thing-at-point 'symbol 'no-properties))))
    (lsp-bridge-call-file-api "rename" (lsp-bridge--position) new-name)))

(defun lsp-bridge-flash-region (start-pos end-pos)
  (require 'pulse)
  (let ((pulse-iterations 1)
        (pulse-delay lsp-bridge-flash-region-delay))
    (pulse-momentary-highlight-region start-pos end-pos 'lsp-bridge-font-lock-flash)))

(defun lsp-bridge-flash-line ()
  (lsp-bridge-flash-region
   (save-excursion
     (vertical-motion 0) (point))
   (save-excursion
     (vertical-motion 1) (point))))

(defun lsp-bridge-rename--highlight (dirname bound-start bound-end)
  (lsp-bridge--with-file-buffer dirname
    (lsp-bridge-flash-region
     (acm-backend-lsp-position-to-point bound-start)
     (acm-backend-lsp-position-to-point bound-end))))

(defun lsp-bridge-popup-documentation ()
  (interactive)
  (lsp-bridge-call-file-api "hover" (lsp-bridge--position)))

(defun lsp-bridge-signature-help-fetch ()
  (interactive)
  (if lsp-bridge-code-action-notify
      (setq-local lsp-bridge-code-action-notify nil)
    (unless (eq last-command 'mwheel-scroll)
      (lsp-bridge-call-file-api "signature_help" (lsp-bridge--position)))))

(defun lsp-bridge-pick-file-path (dirname)
  ;; Remove `file://' and `:file://' prefix.
  (cond ((string-prefix-p "file://" dirname)
         (setq dirname (string-remove-prefix "file://" dirname)))
        ((string-prefix-p ":file://" dirname)
         (setq dirname (string-remove-prefix ":file://" dirname))))

  ;; Convert `%XX' sequences to `:'
  (setq dirname (url-unhex-string dirname))

  ;; Remove / before drive letter on Windows
  (when (string-match "^/[A-Za-z]:" dirname)
    (setq dirname (substring dirname 1)))

  dirname)

(defun lsp-bridge-file-apply-edits (dirname edits &optional temp-buffer)
  (if temp-buffer
      ;; Apply edits to temp buffer.
      (with-current-buffer temp-buffer
        (acm-backend-lsp-apply-text-edits edits))

    ;; Pick dirname from LSP return file string.
    (setq dirname (lsp-bridge-pick-file-path dirname))

    (find-file-noselect dirname)
    (save-excursion
      (find-file dirname)
      (acm-backend-lsp-apply-text-edits edits)))

  (setq-local lsp-bridge-prohibit-completion t))

(defun lsp-bridge-define--jump (dirname position)
  (let (position-before-jump)
    ;; Record postion.
    (set-marker (mark-marker) (point) (current-buffer))
    (setq position-before-jump (copy-marker (mark-marker)))
    (setq mark-ring lsp-bridge-mark-ring)

    ;; Jump to define.
    ;; Show define in other window if `lsp-bridge-jump-to-def-in-other-window' is non-nil.
    (if lsp-bridge-jump-to-def-in-other-window
        (find-file-other-window dirname)
      (find-file dirname))

    ;; Init jump history in new buffer.
    (setq-local lsp-bridge-mark-ring (append (list position-before-jump) mark-ring))

    ;; Jump to define postion.
    (goto-char (acm-backend-lsp-position-to-point position))
    (recenter)

    ;; Flash define line.
    (lsp-bridge-flash-line)))

(defun lsp-bridge-popup-documentation-scroll-up (&optional arg)
  (interactive)
  (posframe-funcall lsp-bridge-lookup-doc-tooltip
                    #'scroll-up-command arg))

(defun lsp-bridge-popup-documentation-scroll-down (&optional arg)
  (interactive)
  (posframe-funcall lsp-bridge-lookup-doc-tooltip
                    #'scroll-down-command arg))

(defun lsp-bridge-popup-documentation--show (value)
  (with-current-buffer (get-buffer-create lsp-bridge-lookup-doc-tooltip)
    (read-only-mode -1)
    (erase-buffer)
    (insert value)
    (acm-markdown-render-content))
  (when (posframe-workable-p)
    (posframe-show lsp-bridge-lookup-doc-tooltip
                   :position (point)
                   :internal-border-width lsp-bridge-lookup-doc-tooltip-border-width
                   :background-color (acm-frame-background-color)
                   :max-width lsp-bridge-lookup-doc-tooltip-max-width
                   :max-height lsp-bridge-lookup-doc-tooltip-max-height)))

(defun lsp-bridge-hide-doc-tooltip ()
  (posframe-hide lsp-bridge-lookup-doc-tooltip)

  (when acm-markdown-render-background
    (set-face-background 'markdown-code-face acm-markdown-render-background)
    (setq acm-markdown-render-background nil))

  (when acm-markdown-render-height
    (set-face-attribute 'markdown-code-face nil :height acm-markdown-render-height)))

(defvar lsp-bridge-signature-posframe-params
  (list :poshandler #'posframe-poshandler-point-bottom-left-corner-upward
        :internal-border-width 20
        :max-width 60
        :max-height 12)
  "Params for signature and `posframe-show'.")

(defcustom lsp-bridge-signature-show-function 'message
  "Function to render signature help. Set to `lsp-bridge-signature-posframe' to use the posframe."
  :type 'function
  :group 'lsp-bridge)

(defcustom lsp-bridge-signature-tooltip " *lsp-bridge-signature*"
  "Buffer for display signature information."
  :type 'string
  :group 'lsp-bridge)

(defun lsp-bridge-hide-signature-tooltip ()
  (posframe-hide lsp-bridge-signature-tooltip))

(defun lsp-bridge-signature-posframe (str)
  "Use posframe to show the STR signatureHelp string."
  (if (not (string-empty-p str))
      (apply #'posframe-show
             (with-current-buffer (get-buffer-create lsp-bridge-signature-tooltip)
               (erase-buffer)
               (insert str)
               (visual-line-mode 1)
               (current-buffer))
             (append
              lsp-bridge-signature-posframe-params
              (list :position (point)
                    :background-color (acm-frame-background-color))))
    (lsp-bridge-hide-signature-tooltip)))

(defun lsp-bridge-signature-help--update (help-infos help-index)
  (let ((index 0)
        (help ""))
    (dolist (help-info help-infos)
      (setq help (concat help
                         (propertize help-info 'face (if (equal index help-index) 'font-lock-function-name-face 'default))
                         (if (equal index (1- (length help-infos))) "" ", ")))
      (setq index (1+ index)))

    (unless (string-equal help "")
      (let ((message-log-max nil))
        (funcall lsp-bridge-signature-show-function help)))))

(defvar lsp-bridge--last-buffer nil)

(defun lsp-bridge-monitor-window-buffer-change ()
  ;; Hide completion, diagnostic and signature frame when buffer or window changed.
  (unless (eq (current-buffer)
              lsp-bridge--last-buffer)
    (lsp-bridge-hide-doc-tooltip)
    (lsp-bridge-diagnostic-hide-tooltip)
    (lsp-bridge-hide-signature-tooltip))

  (unless (or (minibufferp)
              (string-equal (buffer-name) "*Messages*"))
    (setq lsp-bridge--last-buffer (current-buffer))))

(add-hook 'post-command-hook 'lsp-bridge-monitor-window-buffer-change)

(defconst lsp-bridge--internal-hooks
  '((before-change-functions lsp-bridge-monitor-before-change nil t)
    (after-change-functions lsp-bridge-monitor-after-change nil t)
    (pre-command-hook lsp-bridge-monitor-pre-command nil t)
    (post-command-hook lsp-bridge-monitor-post-command nil t)
    (after-save-hook lsp-bridge-monitor-after-save nil t)
    (kill-buffer-hook lsp-bridge-close-buffer-file nil t)
    (find-file-hook lsp-bridge-search-words-update nil t)
    (before-revert-hook lsp-bridge-close-buffer-file nil t)
    (post-self-insert-hook lsp-bridge-monitor-post-self-insert 90 t)
    ))

(defvar lsp-bridge-mode-map (make-sparse-keymap))

(defvar lsp-bridge-signature-help-timer nil)

(defvar lsp-bridge-search-words-timer nil)

(defvar lsp-bridge-auto-format-code-timer nil)

;;;###autoload
(define-minor-mode lsp-bridge-mode
  "LSP Bridge mode."
  :keymap lsp-bridge-mode-map
  :lighter " 橋"
  :init-value nil
  (if lsp-bridge-mode
      (lsp-bridge--enable)
    (lsp-bridge--disable)))

(defun lsp-bridge--enable ()
  "Enable LSP Bridge mode."

  ;; Disable backup file.
  ;; Please use my another plugin `https://github.com/manateelazycat/auto-save' and use git for file version management.
  (when lsp-bridge-disable-backup
    (setq make-backup-files nil)
    (setq auto-save-default nil)
    (setq create-lockfiles nil))

  ;; Add `lsp-bridge-symbols--current-defun' to `whic-func-functions'.
  (if (and lsp-bridge-symbols-enable-which-func
           (featurep 'which-func) which-function-mode)
      (setq-local which-func-functions
                  (add-to-list 'which-func-functions #'lsp-bridge-symbols--current-defun)))

  (setq-local lsp-bridge-revert-buffer-flag nil)

  (acm-run-idle-func acm-backend-elisp-symbols-update-timer lsp-bridge-elisp-symbols-update-idle 'lsp-bridge-elisp-symbols-update)

  (when (or (lsp-bridge-has-lsp-server-p)
            ;; init acm backend for org babel
            (and lsp-bridge-enable-org-babel (eq major-mode 'org-mode)))
    ;; When user open buffer by `ido-find-file', lsp-bridge will throw `FileNotFoundError' error.
    ;; So we need save buffer to disk before enable `lsp-bridge-mode'.
    (unless (file-exists-p (buffer-file-name))
      (save-buffer))

    (setq-local acm-backend-lsp-completion-trigger-characters nil)
    (setq-local acm-backend-lsp-completion-position nil)
    (setq-local acm-backend-lsp-filepath (file-truename buffer-file-name))
    (setq-local acm-backend-lsp-items (make-hash-table :test 'equal))
    (setq-local acm-backend-lsp-server-names nil)

    (when lsp-bridge-enable-signature-help
      (acm-run-idle-func lsp-bridge-signature-help-timer lsp-bridge-signature-help-fetch-idle 'lsp-bridge-signature-help-fetch))
    (when lsp-bridge-enable-search-words
      (acm-run-idle-func lsp-bridge-search-words-timer lsp-bridge-search-words-rebuild-cache-idle 'lsp-bridge-search-words-rebuild-cache))
    (when lsp-bridge-enable-auto-format-code
      (acm-run-idle-func lsp-bridge-auto-format-code-timer lsp-bridge-auto-format-code-idle 'lsp-bridge-auto-format-code)))

  (dolist (hook lsp-bridge--internal-hooks)
    (apply #'add-hook hook))

  (advice-add #'acm-hide :after #'lsp-bridge--completion-hide-advisor)

  ;; Flag `lsp-bridge-is-starting' make sure only call `lsp-bridge-start-process' once.
  (unless lsp-bridge-is-starting
    (lsp-bridge-start-process)))

(defun lsp-bridge--disable ()
  "Disable LSP Bridge mode."
  ;; Remove hooks.
  (dolist (hook lsp-bridge--internal-hooks)
    (remove-hook (nth 0 hook) (nth 1 hook) (nth 3 hook)))

  ;; Cancel idle timer.
  (acm-cancel-timer lsp-bridge-signature-help-timer)
  (acm-cancel-timer lsp-bridge-search-words-timer)
  (acm-cancel-timer lsp-bridge-auto-format-code-timer)
  (acm-cancel-timer acm-backend-elisp-symbols-update-timer)

  ;; Reset `acm-backend-elisp-symbols-update-size' to zero.
  (setq acm-backend-elisp-symbols-update-size 0)

  ;; Remove hide advice.
  (advice-remove #'acm-hide #'lsp-bridge--completion-hide-advisor))

(defun lsp-bridge--turn-off (dirname)
  (lsp-bridge--with-file-buffer dirname
    (lsp-bridge--disable)))

(defcustom lsp-bridge-workspace-symbol-kind-to-face
  [("    " . nil)                          ; Unknown - 0
   ("File" . font-lock-builtin-face)       ; File - 1
   ("Modu" . font-lock-keyword-face)       ; Module - 2
   ("Nmsp" . font-lock-keyword-face)       ; Namespace - 3
   ("Pack" . font-lock-keyword-face)       ; Package - 4
   ("Clss" . font-lock-type-face)          ; Class - 5
   ("Meth" . font-lock-function-name-face) ; Method - 6
   ("Prop" . font-lock-constant-face)      ; Property - 7
   ("Fld " . font-lock-constant-face)      ; Field - 8
   ("Cons" . font-lock-function-name-face) ; Constructor - 9
   ("Enum" . font-lock-type-face)          ; Enum - 10
   ("Intf" . font-lock-type-face)          ; Interface - 11
   ("Func" . font-lock-function-name-face) ; Function - 12
   ("Var " . font-lock-variable-name-face) ; Variable - 13
   ("Cnst" . font-lock-constant-face)      ; Constant - 14
   ("Str " . font-lock-string-face)        ; String - 15
   ("Num " . font-lock-builtin-face)       ; Number - 16
   ("Bool " . font-lock-builtin-face)      ; Boolean - 17
   ("Arr " . font-lock-builtin-face)       ; Array - 18
   ("Obj " . font-lock-builtin-face)       ; Object - 19
   ("Key " . font-lock-constant-face)      ; Key - 20
   ("Null" . font-lock-builtin-face)       ; Null - 21
   ("EmMm" . font-lock-constant-face)      ; EnumMember - 22
   ("Srct" . font-lock-type-face)          ; Struct - 23
   ("Evnt" . font-lock-builtin-face)       ; Event - 24
   ("Op  " . font-lock-function-name-face) ; Operator - 25
   ("TPar" . font-lock-type-face)]         ; TypeParameter - 26
  "Mapping between eacho of LSP's SymbolKind and a face.
A vector of 26 cons cells, where the ith cons cell contains
the string representation and face to use for the i+1th
SymbolKind (defined in the LSP)."
  :group 'lsp-bridge
  :type '(vector
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)))

(defun lsp-bridge-workspace-list-symbols (query)
  (interactive "sWorkspace symbol query: ")
  (lsp-bridge-call-file-api "workspace_symbol" query))

(defun lsp-bridge-workspace--list-symbols (info)
  (if (zerop (length info))
      (message "LSP server did not return any symbols.")
    (let* ((symbols (mapcar #'lsp-bridge-workspace-transform-info info))
           (match-symbol (completing-read "Workspace symbol: " symbols))
           (match-info (seq-filter (lambda (i) (string-equal match-symbol (lsp-bridge-workspace-transform-info i))) info)))
      (when match-info
        (let* ((symbol-info (plist-get (car match-info) :location))
               (symbol-file (lsp-bridge-pick-file-path (format "%s" (plist-get symbol-info :uri))))
               (symbol-position (plist-get (plist-get symbol-info :range) :start)))
          (find-file symbol-file)
          (goto-char (acm-backend-lsp-position-to-point symbol-position))
          )))))

(defun lsp-bridge-workspace-transform-info(info)
  (let* ((name (plist-get info :name))
         (uri (plist-get (plist-get info :location) :uri))
         (kind (plist-get info :kind))
         (sanitized-kind (if (< kind (length lsp-bridge-workspace-symbol-kind-to-face)) kind 0))
         (type (elt lsp-bridge-workspace-symbol-kind-to-face sanitized-kind))
         (typestr (propertize (format "[%s] " (car type)) 'face (cdr type)))
         (project-root (locate-dominating-file default-directory ".git"))
         (pathstr (propertize (format " · %s" (file-relative-name (substring uri 7 nil) project-root))
                              'face font-lock-comment-face)))
    (concat typestr name pathstr)))

(defun lsp-bridge-auto-format-code ()
  (unless (eq last-command 'mwheel-scroll)
    (lsp-bridge-code-format)))

(defun lsp-bridge-code-format ()
  (interactive)
  (when (and
         ;; Current buffer has LSP server.
         (lsp-bridge-has-lsp-server-p)
         ;; Completion menu not show.
         (not (lsp-bridge-completion-ui-visible-p))
         ;; Yasnippet not active.
         (or (not (boundp 'yas--active-snippets))
             (not yas--active-snippets))
         ;; Tempel not active.
         (or (not (boundp 'tempel--active))
             (not tempel--active)))
    (lsp-bridge-call-file-api "formatting" (symbol-value (lsp-bridge--get-indent-width major-mode)))))

(defun lsp-bridge-format--update (dirname edits)
  ;; We need set `inhibit-modification-hooks' to t to avoid GC freeze Emacs.
  (lsp-bridge-save-position
   (let ((inhibit-modification-hooks t))
     ;; Apply code format edits, not sort, just reverse order.
     (lsp-bridge-file-apply-edits dirname edits)
     ;; Make LSP server update full content.
     (lsp-bridge-call-file-api "update_file" (buffer-name))
     ;; Notify format complete.
     (message "[LSP-BRIDGE] Complete code formatting.")
     )))

(defvar lsp-bridge-sdcv-helper-dict nil)

(defun lsp-bridge-workspace-apply-edit (info &optional temp-buffer)
  (lsp-bridge-save-position
   (cond ((plist-get info :documentChanges)
          (dolist (change (plist-get info :documentChanges))
            (lsp-bridge-file-apply-edits
             (plist-get (plist-get change :textDocument) :uri)
             (plist-get change :edits)  temp-buffer)))
         ((plist-get info :changes)
          (let* ((changes (plist-get info :changes))
                 (changes-number (/ (length changes) 2)))
            (dotimes (changes-index changes-number)
              (lsp-bridge-file-apply-edits
               (format "%s" (nth (* changes-index 2) changes))
               (nth (+ (* changes-index 2) 1) changes) temp-buffer))))))

  (setq-local lsp-bridge-prohibit-completion t))

(defun lsp-bridge-completion-item--update (info)
  (let* ((dirname (plist-get info :filepath))
         (key (plist-get info :key))
         (server-name (plist-get info :server))
         (additional-text-edits (plist-get info :additionalTextEdits))
         (documentation (plist-get info :documentation))
         (has-doc-p (and documentation
                         (not (string-equal documentation "")))))
    (lsp-bridge--with-file-buffer dirname
      ;; Update `documentation' and `additionalTextEdits'
      (when-let (item (gethash key (gethash server-name acm-backend-lsp-items)))
        (when additional-text-edits
          (plist-put item :additionalTextEdits additional-text-edits))

        (when has-doc-p
          (plist-put item :documentation documentation))

        (puthash key item (gethash server-name acm-backend-lsp-items)))

      (if has-doc-p
          ;; Show doc frame if `documentation' exist and not empty.
          (acm-doc-try-show t)
        ;; Hide doc frame immediately.
        (acm-doc-hide)))))

(defun lsp-bridge-toggle-sdcv-helper ()
  "Toggle sdcv helper."
  (interactive)
  (unless lsp-bridge-sdcv-helper-dict
    (setq lsp-bridge-sdcv-helper-dict (make-hash-table :test 'equal)))

  (if acm-enable-search-sdcv-words
      (progn
        ;; Disable `lsp-bridge-mode' if it is enable temporality.
        (when (gethash (buffer-name) lsp-bridge-sdcv-helper-dict)
          (lsp-bridge-mode -1)
          (remhash (buffer-name) lsp-bridge-sdcv-helper-dict))

        (message "Turn off SDCV helper."))
    ;; We enable `lsp-bridge-mode' temporality if current-mode not enable `lsp-bridge-mode' yet.
    (unless lsp-bridge-mode
      (puthash (buffer-name) t lsp-bridge-sdcv-helper-dict)
      (lsp-bridge-mode 1))

    (message "Turn on SDCV helper."))
  (setq-local acm-enable-search-sdcv-words (not acm-enable-search-sdcv-words)))

;;;###autoload
(defun global-lsp-bridge-mode ()
  (interactive)

  (dolist (hook lsp-bridge-default-mode-hooks)
    (add-hook hook (lambda ()
                     ;; Except `mind-wave-chat-mode'.
                     (when (or
                            (not (buffer-file-name))
                            (not (string-equal (file-name-extension (buffer-file-name)) "chat")))
                       (lsp-bridge-mode 1))
                     ))))

(with-eval-after-load 'evil
  (evil-add-command-properties #'lsp-bridge-find-def :jump t)
  (evil-add-command-properties #'lsp-bridge-find-references :jump t)
  (evil-add-command-properties #'lsp-bridge-find-impl :jump t))

(defun lsp-bridge--rename-file-advisor (orig-fun &optional arg &rest args)
  (when (and lsp-bridge-mode
             (boundp 'acm-backend-lsp-filepath))
    (let ((new-name (expand-file-name (nth 0 args))))
      (lsp-bridge-call-file-api "rename_file" new-name)
      (lsp-bridge-call-async "close_file" acm-backend-lsp-filepath)
      (set-visited-file-name new-name t t)
      (setq-local acm-backend-lsp-filepath new-name)))
  (apply orig-fun arg args))
(advice-add #'rename-file :around #'lsp-bridge--rename-file-advisor)

;; We use `lsp-bridge-revert-buffer-flag' var avoid lsp-bridge send change_file request while execute `revert-buffer' command.
(defun lsp-bridge--revert-buffer-advisor (orig-fun &optional arg &rest args)
  (setq-local lsp-bridge-revert-buffer-flag t)
  (apply orig-fun arg args)
  (setq-local lsp-bridge-revert-buffer-flag nil))
(advice-add #'revert-buffer :around #'lsp-bridge--revert-buffer-advisor)

(defun lsp-bridge--completion-hide-advisor (&rest args)
  (when lsp-bridge-mode
    ;; Clean LSP backend completion tick.
    (setq-local acm-backend-lsp-fetch-completion-item-ticker nil)))

(defun lsp-bridge-tabnine-complete ()
  (interactive)
  (let* ((buffer-min 1)
         (buffer-max (1+ (buffer-size)))
         (chars-number-before-point 3000) ; the number of chars before point to send for completion.
         (chars-number-after-point 1000) ; the number of chars after point to send for completion.
         (max-num-results 10)     ; maximum number of results to show.
         (before-point (max (point-min) (- (point) chars-number-before-point)))
         (after-point (min (point-max) (+ (point) chars-number-after-point))))
    (lsp-bridge-call-async "tabnine_complete"
                           (buffer-substring-no-properties before-point (point))
                           (buffer-substring-no-properties (point) after-point)
                           (or (buffer-file-name) nil)
                           (= before-point buffer-min)
                           (= after-point buffer-max)
                           max-num-results)))

(defun lsp-bridge-search-backend--record-items (backend-name items)
  (pcase backend-name
    ("file-words" (setq-local acm-backend-search-file-words-items items))
    ("sdcv-words" (setq-local acm-backend-search-sdcv-words-items items))
    ("tabnine" (setq-local acm-backend-tabnine-items items))
    ("tailwind-keywords" (setq-local acm-backend-tailwind-items items))
    ("paths" (setq-local acm-backend-path-items items)))
  (lsp-bridge-try-completion))


;;; support which-func-mode
;;;

(defvar-local lsp-bridge-symbols-current-defun nil)

(defun lsp-bridge-symbols--record-current-defun (current-defun)
  "Record `CURRENT-DEFUN' from lsp."
  (setq-local lsp-bridge-symbols-current-defun current-defun))

(defun lsp-bridge-symbols--current-defun ()
  "Add `lsp-bridge-symbols-current-defun' to `which-func-functions'."
  lsp-bridge-symbols-current-defun)

;;; Mode-line
;;;

(defface lsp-bridge-alive-mode-line
  '((t (:inherit font-lock-constant-face :weight bold)))
  "Face for activity in LSP-bridge's mode line.")

(defface lsp-bridge-kill-mode-line
  '((t (:inherit font-lock-comment-face :weight bold)))
  "Face for kill process in LSP-bridge's mode line.")

(defvar lsp-bridge--mode-line-format `(:eval (lsp-bridge--mode-line-format)))

(put 'lsp-bridge--mode-line-format 'risky-local-variable t)

(defun lsp-bridge--mode-line-format ()
  "Compose the LSP-bridge's mode-line."
  (setq-local mode-face
              (if (lsp-bridge-epc-live-p lsp-bridge-epc-process)
                  'lsp-bridge-alive-mode-line
                'lsp-bridge-kill-mode-line))

  (when lsp-bridge-server
    (propertize "lsp-bridge"'face mode-face)))

(when lsp-bridge-enable-mode-line
  (add-to-list 'mode-line-misc-info
               `(lsp-bridge-mode ("" lsp-bridge--mode-line-format " "))))

(provide 'lsp-bridge)

;;; lsp-bridge.el ends here
