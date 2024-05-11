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
;; Package-Requires: ((emacs "28") (markdown-mode "2.6"))
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
(require 'lsp-bridge-peek)
(require 'lsp-bridge-call-hierarchy)
(require 'lsp-bridge-code-action)
(require 'lsp-bridge-diagnostic)
(require 'lsp-bridge-lsp-installer)
(require 'lsp-bridge-org-babel)
(require 'lsp-bridge-inlay-hint)
(require 'lsp-bridge-dart)
(require 'lsp-bridge-semantic-tokens)

(defgroup lsp-bridge nil
  "LSP-Bridge group."
  :group 'applications)

(require 'acm)
(require 'tramp)
(defvar lsp-bridge-tramp-alias-alist nil)

(defvar lsp-bridge-tramp-connection-info nil)

(setq acm-backend-lsp-fetch-completion-item-func 'lsp-bridge-fetch-completion-item-info)

(defun lsp-bridge-fetch-completion-item-info (candidate)
  (let ((kind (plist-get candidate :icon))
        (server-name (plist-get candidate :server))
        (key (plist-get candidate :key)))
    ;; Try send `completionItem/resolve' request to fetch `documentation' and `additionalTextEdits' information.
    (unless (equal acm-backend-lsp-fetch-completion-item-ticker (list acm-backend-lsp-filepath key kind))
      (if (lsp-bridge-is-remote-file)
          (lsp-bridge-remote-send-func-request "fetch_completion_item_info" (list acm-backend-lsp-filepath key server-name))
        (lsp-bridge-call-async "fetch_completion_item_info" acm-backend-lsp-filepath key server-name))
      (setq-local acm-backend-lsp-fetch-completion-item-ticker (list acm-backend-lsp-filepath key kind)))))

(defcustom lsp-bridge-completion-popup-predicates '(
                                                    lsp-bridge-not-delete-command
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

                                                    lsp-brige-not-in-chatgpt-response

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

(defcustom lsp-bridge-remote-heartbeat-interval nil
  "Interval for sending heartbeat to server in seconds.

Setting this to nil or 0 will turn off the heartbeat mechanism."
  :type 'number
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-mode-line t
  "Whether display LSP-bridge's server info in mode-line ."
  :type 'boolean
  :group 'lsp-bridge)

(defcustom lsp-bridge-completion-stop-commands
  '("undo-tree-undo" "undo-tree-redo"
    "kill-region" "delete-block-backward"
    "python-black-buffer" "acm-complete-or-expand-yas-snippet" "acm-insert-number-or-complete-candiate"
    "yank" "string-rectangle" "query-replace" "grammatical-edit-unwrap")
  "If last command is match this option, stop popup completion ui."
  :type '(repeat string)
  :safe #'listp
  :group 'lsp-bridge)

(defcustom lsp-bridge-completion-hide-characters '(":" ";" "(" ")" "[" "]" "{" "}" "," "\"")
  "If character before match this option, stop popup completion ui.

To make this option works, you need set option `lsp-bridge-completion-obey-trigger-characters-p' with nil first.

After set `lsp-bridge-completion-obey-trigger-characters-p' to nil, you need use `setq' set this value, don't use `custom-set-variables'."
  :type '(repeat string)
  :safe #'listp
  :group 'lsp-bridge)

(defcustom lsp-bridge-completion-obey-trigger-characters-p t
  "If non-nil makes trigger characters a higher priority than `lsp-bridge-completion-hide-characters'."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-bridge)

(defcustom lsp-bridge-popup-documentation-buffer " *lsp-bridge-hover*"
  "Buffer for display hover information."
  :type 'string
  :safe #'stringp
  :group 'lsp-bridge)

(defcustom lsp-bridge-buffer-documentation-buffer "*lsp-bridge-doc*"
  "Buffer for display documentation information."
  :type 'string
  :safe #'stringp
  :group 'lsp-bridge)

(defcustom lsp-bridge-disable-backup t
  "Default disable backup feature, include `make-backup-files' `auto-save-default' and `create-lockfiles'."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-signature-help t
  "Whether to enable signature-help."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-bridge)

(defcustom lsp-bridge-signature-help-fetch-idle 0.5
  "The idle seconds to fetch signature help.."
  :type 'float
  :safe #'floatp
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-search-words t
  "Whether to enable search words of files."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-bridge)

(defcustom lsp-bridge-search-words-prohibit-file-extensions '("png" "jpg" "jpeg" "gif" "pdf")
  "The file extensions to prohibit search words."
  :type '(repeat string)
  :safe #'listp
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-auto-format-code nil
  "Whether to auto format code."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-bridge)

(defcustom lsp-bridge-auto-format-code-idle 1
  "The idle seconds to auto format code."
  :type 'float
  :safe #'floatp
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-diagnostics t
  "Whether to enable diagnostics."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-inlay-hint nil
  "Whether to enable inlay hint."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-bridge)

(defcustom lsp-bridge-elisp-symbols-update-idle 3
  "The idle seconds to update elisp symbols."
  :type 'float
  :safe #'floatp
  :group 'lsp-bridge)

(defcustom lsp-bridge-user-langserver-dir nil
  "The directory where the user place langserver configuration."
  :type '(choice (const nil)
                 (string))
  :safe (lambda (v) (or (null v) (stringp v)))
  :group 'lsp-bridge)

(defcustom lsp-bridge-user-multiserver-dir nil
  "The directory where the user place multiserver configuration."
  :type '(choice (const nil)
                 (string))
  :safe (lambda (v) (or (null v) (stringp v)))
  :group 'lsp-bridge)

(defcustom lsp-bridge-user-ssh-private-key nil
  "custom SSH private key path for use in SSH connections."
  :type '(choice (const nil)
                 (string))
  :safe (lambda (v) (or (null v) (stringp v)))
  :group 'lsp-bridge)

(defcustom lsp-bridge-symbols-enable-which-func nil
  "Wether use lsp-bridge in which-func."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-completion-in-string nil
  "Whether to enable completion in string, default is disable."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-bridge)

(defface lsp-bridge-font-lock-flash
  '((t (:inherit highlight)))
  "Face to flash the current line."
  :group 'lsp-bridge)

(defcustom lsp-bridge-tramp-blacklist nil
  "tramp hosts that don't use lsp-bridge"
  :type '(repeat string)
  :safe #'listp
  :group 'lsp-bridge)

(defcustom lsp-bridge-remote-python-command "python3"
  "Python command on remote host."
  :type 'string
  :safe #'stringp
  :group 'lsp-bridge)

(defcustom lsp-bridge-remote-python-file "~/lsp-bridge/lsp_bridge.py"
  "Full path of lsp_bridge.py file on remote host."
  :type 'string
  :safe #'stringp
  :group 'lsp-bridge)

(defcustom lsp-bridge-remote-log "~/lsp-bridge/lbr_log.txt"
  "Full path of log file on remote host."
  :type 'string
  :safe #'stringp
  :group 'lsp-bridge)

(defcustom lsp-bridge-remote-start-automatically nil
  "Whether start remote lsp-bridge.py automatically."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-bridge)

(defvar lsp-bridge-last-change-command nil)
(defvar lsp-bridge-last-change-position nil)
(defvar lsp-bridge-last-change-is-delete-command-p nil)

(defvar lsp-bridge-server nil
  "The LSP-Bridge Server.")

(defvar lsp-bridge-python-file (expand-file-name "lsp_bridge.py" (if load-file-name
                                                                     (file-name-directory load-file-name)
                                                                   default-directory)))

(defvar-local lsp-bridge-mark-ring nil
  "The list of saved lsp-bridge marks, most recent first.")

(defvar lsp-bridge-server-port nil)

(defun lsp-bridge--start-epc-server ()
  "Function to start the EPC server."
  (unless (process-live-p lsp-bridge-server)
    (setq lsp-bridge-server
          (lsp-bridge-epc-server-start
           (lambda (mngr)
             (let ((mngr mngr))
               (lsp-bridge-epc-define-method mngr 'eval-in-emacs 'lsp-bridge--eval-in-emacs-func)
               (lsp-bridge-epc-define-method mngr 'get-emacs-vars 'lsp-bridge--get-emacs-vars-func)
               (lsp-bridge-epc-define-method mngr 'get-project-path 'lsp-bridge--get-project-path-func)
               (lsp-bridge-epc-define-method mngr 'get-workspace-folder 'lsp-bridge--get-workspace-folder-func)
               (lsp-bridge-epc-define-method mngr 'get-multi-lang-server 'lsp-bridge--get-multi-lang-server-func)
               (lsp-bridge-epc-define-method mngr 'get-single-lang-server 'lsp-bridge--get-single-lang-server-func)
               (lsp-bridge-epc-define-method mngr 'get-user-emacs-directory 'lsp-bridge--user-emacs-directory-func)
               (lsp-bridge-epc-define-method mngr 'get-buffer-content 'lsp-bridge--get-buffer-content-func)
               (lsp-bridge-epc-define-method mngr 'get-current-line 'lsp-bridge--get-current-line-func)
               (lsp-bridge-epc-define-method mngr 'get-ssh-password 'lsp-bridge--get-ssh-password-func)
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
(defvar lsp-bridge-position-before-jump nil)

(defcustom lsp-bridge-name "*lsp-bridge*"
  "Name of LSP-Bridge buffer."
  :type 'string)

(defcustom lsp-bridge-python-command (cond ((memq system-type '(cygwin windows-nt ms-dos))
                                            (cond ((executable-find "pypy3.exe")
                                                   "pypy3.exe")
                                                  ((executable-find "python3.exe")
                                                   "python3.exe")
                                                  ((executable-find "python.exe")
                                                   "python.exe")))
                                           (t (cond ((executable-find "pypy3")
                                                     "pypy3")
                                                    ((executable-find "python3")
                                                     "python3")
                                                    ((executable-find "python")
                                                     "python"))))
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

(defcustom lsp-bridge-enable-completion-in-minibuffer nil
  "Enable this option to completion in minibuffer."
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
    (("typ") . "typst-lsp")
    )
  "The lang server rule for file extension."
  :type 'cons)

(defcustom lsp-bridge-c-lsp-server "clangd"
  "Default LSP server for C language, you can choose `clangd' or `ccls'."
  :type 'string)

(defcustom lsp-bridge-elixir-lsp-server "elixirLS"
  "Default LSP server for Elixir language, you can choose `elixirLS', `lexical' or `nextls'."
  :type 'string
  :safe #'stringp)

(defcustom lsp-bridge-php-lsp-server "intelephense"
  "Default LSP server for PHP language, you can choose `intelephense' or `phpactor'."
  :type 'string
  :safe #'stringp)

(defcustom lsp-bridge-python-lsp-server "pyright"
  "Default LSP server for Python.
Possible choices are pyright, pyright-background-analysis, jedi, python-ms, pylsp, and ruff."
  :type 'string)

(defcustom lsp-bridge-python-multi-lsp-server "pyright-background-analysis_ruff"
  "Default Multi LSP server for Python.
Possible choices are pyright_ruff, pyright-background-analysis_ruff, jedi_ruff, python-ms_ruff, and pylsp_ruff."
  :type 'string)

(defcustom lsp-bridge-tex-lsp-server "texlab"
  "Default LSP server for (la)tex, you can choose `texlab' or `digestif'."
  :type 'string)

(defcustom lsp-bridge-csharp-lsp-server "omnisharp-dotnet"
  "Default LSP server for C#, you can choose `omnisharp-mono' or `omnisharp-dotnet'."
  :type 'string)

(defcustom lsp-bridge-nix-lsp-server "rnix-lsp"
  "Default LSP server for nix, you can choose `rnix-lsp' or `nil'."
  :type 'string)

(defcustom lsp-bridge-markdown-lsp-server nil
  "Default LSP server for markdown, you can choose `vale-ls' or `nil'."
  :type 'string)

(defcustom lsp-bridge-use-wenls-in-org-mode nil
  "Use `wen' lsp server in org-mode, default is disable.")

(defcustom lsp-bridge-use-ds-pinyin-in-org-mode nil
  "Use `ds-pinyin' lsp server in org-mode, default is disable.")

(defcustom lsp-bridge-complete-manually nil
  "Only popup completion menu when user call `lsp-bridge-popup-complete-menu' command.")

(defcustom lsp-bridge-multi-lang-server-mode-list
  '(((python-mode python-ts-mode) . lsp-bridge-python-multi-lsp-server)
    ((qml-mode qml-ts-mode) . "qmlls_javascript"))
  "The multi lang server rule for file mode."
  :type 'cons)

(defcustom lsp-bridge-single-lang-server-mode-list
  '(
    ((c-mode c-ts-mode c++-mode c++-ts-mode objc-mode c-or-c++-ts-mode) .        lsp-bridge-c-lsp-server)
    ((cmake-mode cmake-ts-mode) .                                                "cmake-language-server")
    ((java-mode java-ts-mode) .                                                  "jdtls")
    ((julia-mode) .                                                              "julials")
    ((python-mode python-ts-mode) .                                              lsp-bridge-python-lsp-server)
    ((ruby-mode ruby-ts-mode) .                                                  "solargraph")
    ((rust-mode rustic-mode rust-ts-mode) .                                      "rust-analyzer")
	(move-mode .                                                                 "move-analyzer")
    ((elixir-mode elixir-ts-mode heex-ts-mode) .                                 lsp-bridge-elixir-lsp-server)
    ((go-mode go-ts-mode) .                                                      "gopls")
    (groovy-mode .                                                               "groovy-language-server")
    (haskell-mode .                                                              "hls")
    (lua-mode .                                                                  "sumneko")
    (markdown-mode .                                                             lsp-bridge-markdown-lsp-server)
    (dart-mode .                                                                 "dart-analysis-server")
    (scala-mode .                                                                "metals")
    ((js2-mode js-mode js-ts-mode rjsx-mode) .                                   "javascript")
    ((typescript-tsx-mode tsx-ts-mode) .                                         "typescriptreact")
    ((typescript-mode typescript-ts-mode) .                                      "typescript")
    (tuareg-mode .                                                               "ocamllsp")
    (erlang-mode .                                                               "erlang-ls")
    ((LaTeX-mode latex-mode Tex-latex-mode texmode context-mode texinfo-mode bibtex-mode) . lsp-bridge-tex-lsp-server)
    ((clojure-mode
      clojurec-mode
      clojurescript-mode
      clojurex-mode
      clojure-ts-mode
      clojure-ts-clojurec-mode
      clojure-ts-clojurescript-mode
      clojure-ts-clojuredart-mode)  .                                                   "clojure-lsp")
    ((sh-mode bash-mode bash-ts-mode) .                                          "bash-language-server")
    ((css-mode css-ts-mode) .                                                    "vscode-css-language-server")
    (elm-mode   .                                                                "elm-language-server")
    ((php-mode php-ts-mode) .                                                    lsp-bridge-php-lsp-server)
    ((yaml-mode yaml-ts-mode) .                                                  "yaml-language-server")
    (zig-mode .                                                                  "zls")
    (dockerfile-mode .                                                           "docker-langserver")
    (d-mode .                                                                    "serve-d")
    ((fortran-mode f90-mode) .                                                   "fortls")
    (nix-mode .                                                                  lsp-bridge-nix-lsp-server)
    (nickel-mode .                                                               "nls")
    (ess-r-mode .                                                                "rlanguageserver")
    ((graphql-mode graphql-ts-mode) .                                            "graphql-lsp")
    (swift-mode .                                                                "swift-sourcekit")
    (csharp-mode .                                                               lsp-bridge-csharp-lsp-server)
    (kotlin-mode .                                                               "kotlin-language-server")
    (verilog-mode .                                                              "verible")
    (vhdl-mode .                                                                 "vhdl-tool")
    (svelte-mode .                                                               "svelteserver")
    (fsharp-mode .                                                               "fsautocomplete")
    (beancount-mode .                                                            "beancount-language-server")
    (racket-mode    .                                                            "racket-langserver")
    (mojo-mode    .                                                              "mojo-lsp-server")
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
    ruby-ts-mode-hook
    lua-mode-hook
    move-mode-hook
    rust-mode-hook
    markdown-mode-hook
    rust-ts-mode-hook
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
    LaTeX-mode-hook
    Tex-latex-mode-hook
    texmode-hook
    context-mode-hook
    texinfo-mode-hook
    bibtex-mode-hook
    clojure-mode-hook
    clojurec-mode-hook
    clojurescript-mode-hook
    clojurex-mode-hook
    clojure-ts-mode-hook
    clojurec-ts-mode-hook
    clojurescript-ts-mode-hook
    clojure-dart-ts-mode-hook
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
    php-ts-mode-hook
    yaml-mode-hook
    zig-mode-hook
    groovy-mode-hook
    dockerfile-mode-hook
    d-mode-hook
    f90-mode-hook
    fortran-mode-hook
    nix-mode-hook
    nickel-mode-hook
    ess-r-mode-hook
    verilog-mode-hook
    swift-mode-hook
    csharp-mode-hook
    telega-chat-mode-hook
    markdown-mode-hook
    kotlin-mode-hook
    vhdl-mode-hook
    typst-mode-hook
    graphql-mode-hook
    graphql-ts-mode-hook
    c-ts-mode-hook
    c++-ts-mode-hook
    cmake-ts-mode-hook
    elixir-ts-mode-hook
    toml-ts-mode-hook
    css-ts-mode-hook
    java-ts-mode-hook
    js-ts-mode-hook
    json-ts-mode-hook
    python-ts-mode-hook
    bash-ts-mode-hook
    typescript-ts-mode-hook
    tsx-ts-mode-hook
    go-ts-mode-hook
    yaml-ts-mode-hook
    svelte-mode-hook
    fsharp-mode-hook
    beancount-mode-hook
    mojo-mode-hook
    )
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
    (c-ts-mode                  . c-basic-offset) ; C
    (c++-mode                   . c-basic-offset) ; C++
    (markdown-mode              . c-basic-offset) ; Markdown.
    (csharp-mode                . c-basic-offset) ; C#
    (csharp-tree-sitter-mode    . csharp-tree-sitter-indent-offset) ; C#
    (d-mode                     . c-basic-offset)             ; D
    (julia-mode                 . c-basic-offset)             ; Julia
    (java-mode                  . c-basic-offset)             ; Java
    (java-ts-mode               . java-ts-mode-indent-offset) ; Java
    (jde-mode                   . c-basic-offset)   ; Java (JDE)
    (js-mode                    . js-indent-level)  ; JavaScript
    (js2-mode                   . js2-basic-offset) ; JavaScript-IDE
    (js3-mode                   . js3-indent-level) ; JavaScript-IDE
    (json-mode                  . js-indent-level)  ; JSON
    (json-ts-mode               . js-indent-level)  ; JSON
    (lua-mode                   . lua-indent-level) ; Lua
    (objc-mode                  . c-basic-offset)   ; Objective C
    (php-mode                   . c-basic-offset)   ; PHP
    (php-ts-mode                . php-ts-mode-indent-offset) ; PHP
    (perl-mode                  . perl-indent-level)         ; Perl
    (cperl-mode                 . cperl-indent-level)        ; Perl
    (raku-mode                  . raku-indent-offset)  ; Perl6/Raku
    (erlang-mode                . erlang-indent-level) ; Erlang
    (ada-mode                   . ada-indent)          ; Ada
    (sgml-mode                  . sgml-basic-offset)   ; SGML
    (nxml-mode                  . nxml-child-indent)   ; XML
    (nickel-mode                . c-basic-offset)
    (pascal-mode                . pascal-indent-level)     ; Pascal
    (typescript-mode            . typescript-indent-level) ; Typescript
    (typescript-ts-mode         . typescript-ts-mode-indent-offset) ; Typescript
    (tsx-ts-mode                . typescript-ts-mode-indent-offset) ; Typescript[TSX]
    (sh-mode                    . sh-basic-offset)   ; Shell Script
    (ruby-mode                  . ruby-indent-level) ; Ruby
    (ruby-ts-mode               . ruby-indent-level) ; Ruby
    (enh-ruby-mode              . enh-ruby-indent-level) ; Ruby
    (crystal-mode               . crystal-indent-level) ; Crystal (Ruby)
    (css-mode                   . css-indent-offset)    ; CSS
	(move-mode                  . move-indent-offset)   ; Move
    (rust-mode                  . rust-indent-offset)   ; Rust
    (rust-ts-mode               . rust-ts-mode-indent-offset) ; Rust
    (rustic-mode                . rustic-indent-offset)       ; Rust
    (scala-mode                 . scala-indent:step)          ; Scala
    (powershell-mode            . powershell-indent)    ; PowerShell
    (ess-mode                   . ess-indent-offset)    ; ESS (R)
    (yaml-mode                  . yaml-indent-offset)   ; YAML
    (hack-mode                  . hack-indent-offset)   ; Hack
    (kotlin-mode                . c-basic-offset)       ; Kotlin
    (verilog-mode               . verilog-indent-level) ; Verilog
    (vhdl-mode                  . vhdl-basic-offset)    ; VHDL
    (go-mode                    . c-basic-offset)       ;Golang
    (go-ts-mode                 . c-basic-offset)       ;Golang
    (svelte-mode                . js-indent-level)      ;Svelte
    (fsharp-mode                . fsharp-indent-offset) ; F#
    (default                    . standard-indent)) ; default fallback
  "A mapping from `major-mode' to its indent variable.")

(defcustom lsp-bridge-string-interpolation-open-chars-alist
  '(;; For {}
    (python-mode .        "[^\$]\{")
    (python-ts-mode .     "[^\$]\{")
    ;; For ${}
    (js-mode .            "\$\{")
    (js-ts-mode .         "\$\{")
    (js2-mode .           "\$\{")
    (js3-mode .           "\$\{")
    (typescript-mode .    "\$\{")
    (typescript-ts-mode . "\$\{")
    (sh-mode .            "\$\{")
    (bash-mode .          "\$\{")
    (bash-ts-mode .       "\$\{")
    (typst--base-mode .   "\$\{")
    (typst--code-mode .   "\$\{")
    (typst--math-mode .   "\$\{")
    (typst--markup-mode . "\$\{")
    ;; For #{}
    (elixir-mode .        "\#\{")
    (elixir-ts-mode .     "\#\{")
    (ruby-mode .          "\#\{")
    (ruby-ts-mode .       "\#\{")
    ;; For {{}}
    (yaml-mode .          "\{\{"))
  "Open characters for string interpolation. The elements are cons cell (major-mode . open-char-regexp)"
  :type 'cons)

(defvar lsp-bridge-enable-with-tramp t
  "Whether enable lsp-bridge when editing tramp file.")

(defvar lsp-bridge-remote-save-password nil
  "Whether save password in netrc file.")

(defun lsp-bridge-find-file-hook-function ()
  (when (and lsp-bridge-enable-with-tramp (file-remote-p (buffer-file-name)))
    (lsp-bridge-sync-tramp-remote)))

(add-hook 'find-file-hook #'lsp-bridge-find-file-hook-function)

(defun lsp-bridge--get-indent-width (mode)
  "Get indentation offset for MODE."
  (or (alist-get mode lsp-bridge-formatting-indent-alist)
      (lsp-bridge--get-indent-width (or (get mode 'derived-mode-parent) 'default))))

(cl-defmacro lsp-bridge--with-file-buffer (filename filehost &rest body)
  "Evaluate BODY in buffer with FILEPATH."
  (declare (indent 1))
  `(when-let ((buffer (pcase ,filehost
                        ("" (lsp-bridge-get-match-buffer-by-filepath ,filename))
                        (_ (lsp-bridge-get-match-buffer-by-remote-file ,filehost ,filename)))))
     (with-current-buffer buffer
       ,@body)))

(cl-defmacro lsp-bridge-save-position (&rest body)
  "`save-excursion' not enough for LSP code format.
So we build this macro to restore postion after code format."
  `(let* ((current-buf (current-buffer))
          (current-line (line-number-at-pos nil t))
          (current-column (lsp-bridge--calculate-column))
          (indent-column (save-excursion
                           (back-to-indentation)
                           (lsp-bridge--calculate-column))))
     ,@body
     (switch-to-buffer current-buf)
     (goto-line current-line)
     (back-to-indentation)
     (forward-char (max (- current-column indent-column) 0))))

(defun lsp-bridge-is-remote-file ()
  (and (boundp 'lsp-bridge-remote-file-flag)
       lsp-bridge-remote-file-flag))

(defun lsp-bridge-get-buffer-file-name-text ()
  (lsp-bridge-buffer-file-name buffer-file-name))

(defun lsp-bridge-buffer-file-name (name)
  ;; `buffer-file-name' may contain face property, we need use `substring-no-properties' remove those face from buffer name.
  (when (stringp name)
    (substring-no-properties name)))

(defun lsp-bridge-get-buffer-truename (&optional filename)
  (if (lsp-bridge-is-remote-file)
      lsp-bridge-remote-file-path
    (let ((name (or filename
                    (lsp-bridge-get-buffer-file-name-text))))
      (when name
        (file-truename name)))))

(defun lsp-bridge-get-match-buffer-by-remote-file (host path)
  (cl-dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (boundp 'lsp-bridge-remote-file-path)
                 (string-equal lsp-bridge-remote-file-path path)
                 (boundp 'lsp-bridge-remote-file-host)
                 (string-equal lsp-bridge-remote-file-host host))
        (cl-return buffer)))))

(defun lsp-bridge-get-match-buffer-by-filepath (name)
  (cl-dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when-let* ((file-name (buffer-file-name buffer))
                  (match-buffer (or (string-equal file-name name)
                                    (string-equal (file-truename file-name) name))))
        (cl-return buffer)))))

(defun lsp-bridge--get-project-path-func (filename)
  "Get project root path, search order:

1. Follow the rule of `lsp-bridge-get-project-path-by-filepath'
2. Search up `.dir-locals.el'
3. Search up `.git'"
  (if lsp-bridge-get-project-path-by-filepath
      ;; Fetch project root path by `lsp-bridge-get-project-path-by-filepath' if it set by user.
      (funcall lsp-bridge-get-project-path-by-filepath filename)
    ;; Otherwise try to search up `.dir-locals.el' file
    (car (dir-locals-find-file filename))))

(defun lsp-bridge--get-workspace-folder-func (project-path)
  (when lsp-bridge-get-workspace-folder
    (funcall lsp-bridge-get-workspace-folder project-path)))

(defun lsp-bridge--get-multi-lang-server-func (project-path filename)
  "Get lang server with project path, file path or file extension."
  (or (when lsp-bridge-get-multi-lang-server-by-project
        (funcall lsp-bridge-get-multi-lang-server-by-project project-path filename))
      (lsp-bridge-get-multi-lang-server-by-extension filename)
      (lsp-bridge-get-multi-lang-server-by-file-mode filename)))

(defun lsp-bridge--get-single-lang-server-func (project-path filename)
  "Get lang server with project path, file path or file extension."
  (or (when lsp-bridge-get-single-lang-server-by-project
        (funcall lsp-bridge-get-single-lang-server-by-project project-path filename))
      (lsp-bridge-get-single-lang-server-by-extension filename)
      (lsp-bridge-get-single-lang-server-by-file-mode filename)))

(defun lsp-bridge--user-emacs-directory-func ()
  "Get lang server with project path, file path or file extension."
  (expand-file-name user-emacs-directory))

(defun lsp-bridge--get-buffer-content-func (buffer-name &optional no-org-babel)
  "Get buffer content for lsp. BUFFER-NAME is name eval from (buffer-name)."
  (when-let* ((buf (get-buffer buffer-name)))
    (if (and lsp-bridge-enable-org-babel
             (eq major-mode 'org-mode) (not no-org-babel))
        (and lsp-bridge-org-babel--info-cache
             (org-element-property :value lsp-bridge-org-babel--info-cache))
      (with-current-buffer buf
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun lsp-bridge--get-current-line-func ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defvar-local lsp-bridge-tramp-sync-var nil)

(defun lsp-bridge--get-ssh-password-func (user host port)
  (condition-case nil
      (let* ((auth-source-creation-prompts
              '((secret . "password for %u@%h: ")))
             (found (nth 0 (auth-source-search :max 1
                                               :host host
                                               :user user
                                               :port port
                                               :require '(:secret)
                                               :create t))))
        (when (and lsp-bridge-remote-save-password (plist-get found :save-function))
          (funcall (plist-get found :save-function)))
        (if found
            (auth-info-password found)
          nil))
    (quit (progn
            (message "Cancelled password input.")
            nil))))

(defun lsp-bridge-get-lang-server-by-extension (filename extension-list)
  "Get lang server for file extension."
  (when-let* ((file-extension (file-name-extension filename))
              (langserver-info (cl-find-if
                                (lambda (pair)
                                  (let ((extension (car pair)))
                                    (if (eq (type-of extension) 'string)
                                        (string-equal file-extension extension)
                                      (member file-extension extension))))
                                extension-list)))
    (cdr langserver-info)))

(defun lsp-bridge-get-multi-lang-server-by-extension (filename)
  "Get lang server for file extension."
  (lsp-bridge-get-lang-server-by-extension filename lsp-bridge-multi-lang-server-extension-list))

(defun lsp-bridge-get-single-lang-server-by-extension (filename)
  "Get lang server for file extension."
  (lsp-bridge-get-lang-server-by-extension filename lsp-bridge-single-lang-server-extension-list))

(defun lsp-bridge-lang-server-by-mode (target-mode mode-list)
  "Get lang server for file mode."
  (cl-find-if
   (lambda (pair)
     (let ((mode (car pair)))
       (if (symbolp mode)
           (eq target-mode mode)
         (member target-mode mode))))
   mode-list))

(defun lsp-bridge-get-symbol-string-value (info)
  (pcase (format "%s" (type-of info))
    ("string" info)
    ("symbol" (symbol-value info))
    ))

(defun lsp-bridge-get-mode-name-from-file-path (file-path)
  (cdr (assoc file-path
              auto-mode-alist
              'string-match-p)))

(defun lsp-brige-get-mode (filepath)
  (let ((buffer (lsp-bridge-get-match-buffer-by-filepath filepath)))
    (if buffer
        (with-current-buffer buffer
          major-mode)
      (lsp-bridge-get-mode-name-from-file-path filepath))))

(defun lsp-bridge-get-multi-lang-server-by-file-mode (filename)
  "Get lang server for file mode."
  (when-let* ((mode (lsp-brige-get-mode filename))
              (langserver-info (lsp-bridge-lang-server-by-mode mode lsp-bridge-multi-lang-server-mode-list)))
    (lsp-bridge-get-symbol-string-value (cdr langserver-info))))

(defun lsp-bridge-get-single-lang-server-by-file-mode (filename)
  "Get lang server for file mode."
  (let* ((mode (lsp-brige-get-mode filename))
         (langserver-info (lsp-bridge-lang-server-by-mode mode lsp-bridge-single-lang-server-mode-list)))
    (cond (langserver-info
           (lsp-bridge-get-symbol-string-value (cdr langserver-info)))
          ((eq mode 'org-mode)
           (cond
            (lsp-bridge-use-wenls-in-org-mode
             "wen")
            (lsp-bridge-use-ds-pinyin-in-org-mode
             "ds-pinyin")
            (lsp-bridge-enable-org-babel
             (lsp-bridge-org-babel-check-lsp-server)))))))

(defun lsp-bridge-has-lsp-server-p ()
  (cond ((and lsp-bridge-enable-org-babel (eq major-mode 'org-mode))
         (setq-local acm-is-elisp-mode-in-org nil)
         (lsp-bridge-org-babel-check-lsp-server))
        (t
         (when-let* ((filename (or (ignore-errors (file-truename
                                                   (lsp-bridge-get-buffer-file-name-text)))
                                   (when (lsp-bridge-is-remote-file)
                                     lsp-bridge-remote-file-path))))
           (let* ((multi-lang-server-by-extension (or (lsp-bridge-get-multi-lang-server-by-extension filename)
                                                      (lsp-bridge-get-multi-lang-server-by-file-mode filename)))
                  (lang-server-by-extension (or (lsp-bridge-get-single-lang-server-by-extension filename)
                                                (lsp-bridge-get-single-lang-server-by-file-mode filename))))
             (if multi-lang-server-by-extension
                 multi-lang-server-by-extension
               lang-server-by-extension)
             )))))

(defun lsp-bridge-call-async (method &rest args)
  "Call Python EPC function METHOD and ARGS asynchronously."
  (lsp-bridge-deferred-chain
    (lsp-bridge-epc-call-deferred lsp-bridge-epc-process (read method) args)))

(defvar-local lsp-bridge-buffer-file-deleted nil)

(defun lsp-bridge-process-live-p ()
  (lsp-bridge-epc-live-p lsp-bridge-epc-process))

(defun lsp-bridge-call-file-api-p ()
  (and lsp-bridge-mode
       (lsp-bridge-has-lsp-server-p)
       (if (boundp 'acm-backend-lsp-server-command-exist)
           acm-backend-lsp-server-command-exist
         t)
       (lsp-bridge-process-live-p)))

(defun lsp-bridge-call-file-api (method &rest args)
  (if (lsp-bridge-is-remote-file)
      (lsp-bridge-remote-send-lsp-request method args)
    (if (and buffer-file-name (file-remote-p (buffer-file-name)))
        (message "[LSP-Bridge] remote file \"%s\" is updating info... skip call %s."
                 (buffer-file-name) method)
      (when (lsp-bridge-call-file-api-p)
        (if (and (boundp 'acm-backend-lsp-filepath)
                 (file-exists-p acm-backend-lsp-filepath))
            (if lsp-bridge-buffer-file-deleted
                ;; If buffer's file create again (such as switch branch back), we need save buffer first,
                ;; send the LSP request after the file is changed next time.
                (progn
                  (save-buffer)
                  (setq-local lsp-bridge-buffer-file-deleted nil)
                  (message "[LSP-Bridge] %s is back, will send the %s LSP request after the file is changed next time." acm-backend-lsp-filepath method))
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
            (message "[LSP-Bridge] %s is not exist, stop send the %s LSP request until file create again." acm-backend-lsp-filepath method)))))))

(defvar lsp-bridge-log-buffer-window nil)

(defun lsp-bridge-restart-process ()
  "Stop and restart LSP-Bridge process."
  (interactive)
  ;; Record log buffer window before restart lsp-bridge process.
  (setq lsp-bridge-log-buffer-window
        (cl-dolist (buffer (buffer-list))
          (when (string-equal (buffer-name buffer) lsp-bridge-name)
            (cl-return (cons (get-buffer-window buffer) (selected-window)))
            )))

  ;; Hide diagnostics.
  (lsp-bridge-diagnostic-hide-overlays)

  ;; Restart lsp-bridge process.
  (lsp-bridge-kill-process)
  (lsp-bridge-start-process)

  ;; Try restore lsp-bridge log buffer after restart.
  (when lsp-bridge-log-buffer-window
    (save-excursion
      (when (window-live-p (car lsp-bridge-log-buffer-window))
        (select-window (car lsp-bridge-log-buffer-window))
        (switch-to-buffer lsp-bridge-name))
      (select-window (cdr lsp-bridge-log-buffer-window))))

  (message "[LSP-Bridge] Process restarted."))

(defun lsp-bridge-profile-dump ()
  (interactive)
  (lsp-bridge-call-async "profile_dump"))

(defun lsp-bridge-start-process ()
  "Start LSP-Bridge process if it isn't started."
  (if (lsp-bridge-process-live-p)
      (remove-hook 'post-command-hook #'lsp-bridge-start-process)
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
  (when (lsp-bridge-process-live-p)
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
                                :connection (lsp-bridge-epc-connect "127.0.0.1" lsp-bridge-epc-port)
                                ))
  (lsp-bridge-epc-init-epc-layer lsp-bridge-epc-process)

  ;; Search words from opened files.
  (lsp-bridge-search-words-index-files)

  ;; Synchronize elisp symbol to Python side.
  (lsp-bridge-elisp-symbols-update))

(defvar-local lsp-bridge-last-cursor-position 0)
(defvar-local lsp-bridge-prohibit-completion nil)

(defvar-local lsp-bridge-cursor-before-command 0)
(defvar-local lsp-bridge-cursor-after-command 0)

(defvar-local lsp-bridge-inlay-hint-last-update-pos nil)

(defun lsp-bridge-monitor-pre-command ()
  (setq-local lsp-bridge-cursor-before-command (point))

  ;; Tab-and-go
  (when (and acm-preview-overlay
             (not (string-prefix-p "acm" (format "%s" this-command))))
    (acm-complete))

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
  (setq-local lsp-bridge-cursor-after-command (point))

  (let ((this-command-string (format "%s" this-command)))
    (when (and lsp-bridge-mode
               (member this-command-string '("self-insert-command" "org-self-insert-command" "lsp-bridge-popup-complete-menu")))
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

      ;; Try send inlay hint if window scroll.
      (when lsp-bridge-enable-inlay-hint
        (let ((window-pos (window-end nil t)))
          (when (not (equal lsp-bridge-inlay-hint-last-update-pos window-pos))
            (lsp-bridge-try-send-inlay-hint-request)
            (setq-local lsp-bridge-inlay-hint-last-update-pos window-pos)))))))

(defun lsp-bridge-close-buffer-file ()
  (if (lsp-bridge-is-remote-file)
      (progn
        (lsp-bridge-remote-send-func-request "close_file" (list lsp-bridge-remote-file-path))
        (lsp-bridge-remote-send-func-request "search_file_words_close_file" (list lsp-bridge-remote-file-path)))

    (when (lsp-bridge-process-live-p)
      (when (and (lsp-bridge-has-lsp-server-p)
                 (boundp 'acm-backend-lsp-filepath))
        (lsp-bridge-call-async "close_file" acm-backend-lsp-filepath))

      (when buffer-file-name
        (lsp-bridge-call-async "search_file_words_close_file" (lsp-bridge-get-buffer-file-name-text))))))

(defun lsp-bridge-set-prefix-style (prefix-style)
  ;; Wen LSP server need `acm-get-input-prefix-bound' return ASCII keyword prefix,
  ;; other LSP server need use `bounds-of-thing-at-point' of symbol as keyword prefix.
  (setq-local acm-input-bound-style prefix-style))

(defun lsp-bridge-set-server-names (filename filehost server-names)
  (lsp-bridge--with-file-buffer filename filehost
                                (setq-local acm-backend-lsp-server-names server-names)))

(defun lsp-bridge-completion--record-items (filename
                                            filehost
                                            candidates
                                            position
                                            server-name
                                            completion-trigger-characters
                                            server-names)
  (lsp-bridge--with-file-buffer filename filehost
                                ;; Save completion items.
                                (setq-local acm-backend-lsp-cache-candidates nil)
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
         (when (equal lsp-bridge-last-change-position
                      (list (current-buffer) (buffer-chars-modified-tick) (point)))
           ;; Try popup completion frame.
           (if (cl-every (lambda (pred)
                           (if (functionp pred)
                               (let ((result (funcall pred)))
                                 (when lsp-bridge-enable-log
                                   (unless result
                                     (with-current-buffer (get-buffer-create lsp-bridge-name)
                                       (save-excursion
                                         (goto-char (point-max))
                                         (insert (format "\n*** lsp-bridge-try-completion execute predicate '%s' failed with result: '%s'\n"
                                                         pred result))))))
                                 result)
                             t))
                         lsp-bridge-completion-popup-predicates)
               (progn
                 (acm-template-candidate-init)
                 (acm-update)

                 ;; We need reset `lsp-bridge-manual-complete-flag' if completion menu popup by `lsp-bridge-popup-complete-menu'.
                 (when lsp-bridge-complete-manually
                   (setq-local lsp-bridge-manual-complete-flag nil)))
             (acm-hide)
             )))))

(defun lsp-bridge-record-last-change-position ()
  (setq lsp-bridge-last-change-position
        (list (current-buffer) (buffer-chars-modified-tick) (point))))

(defun lsp-bridge-popup-complete-menu ()
  (interactive)
  ;; Set `lsp-bridge-manual-complete-flag' to non-nil, make sure poup completion menu once.
  (setq-local lsp-bridge-manual-complete-flag t)

  ;; Record last change position make sure `lsp-bridge-try-completion' will popup completion menu.
  (lsp-bridge-record-last-change-position)

  ;; Sync src block content to lsp server make sure lsp-bridge can popup completion menu even user don't change code in src block.
  (when (and lsp-bridge-enable-org-babel (eq major-mode 'org-mode))
    (setq-local lsp-bridge-org-babel--update-file-before-change t)
    (lsp-bridge-org-babel-send-src-block-to-lsp-server))

  ;; We send `try_completion' request directly, because user input nothing before call command `lsp-bridge-popup-complete-menu'.
  (lsp-bridge-call-file-api "try_completion"
                            (lsp-bridge--position)
                            (acm-char-before)
                            (acm-get-input-prefix))

  ;; Complete other non-LSP backends.
  (lsp-bridge-complete-other-backends))

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
   ;; Allow completion in string.
   lsp-bridge-enable-completion-in-string
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
     (and (lsp-bridge-elisp-get-filepath)
          (or (file-exists-p (file-name-directory (lsp-bridge-elisp-get-filepath)))
              ;; Allow string in lsp-bridge-remote file.
              (lsp-bridge-is-remote-file))))))

(defun lsp-bridge-not-execute-macro ()
  "Hide completion during executing macros."
  (not executing-kbd-macro))

(defun lsp-bridge-not-in-mark-macro ()
  "Hide completion markmacro enable."
  (not (and (featurep 'markmacro)
            markmacro-overlays)))

(defun lsp-bridge-not-delete-command ()
  "Hide completion menu if last command is delete command."
  (not lsp-bridge-last-change-is-delete-command-p))

(defun lsp-bridge-not-follow-complete ()
  "Hide completion if last command is `acm-complete'."
  (or
   (not (member (format "%s" last-command) '("acm-complete" "acm-complete-quick-access")))
   (member (format "%s" this-command) '("self-insert-command" "org-self-insert-command"))
   ))

(defun lsp-bridge-not-only-blank-before-cursor ()
  "Hide completion if only blank before cursor, except in roam bracket."
  (or (not
       (null
        (split-string (buffer-substring-no-properties
                       (max (1- (point)) (line-beginning-position))
                       (point)))))
      ;; Keep completion after space in Org roam bracket.
      (acm-in-roam-bracket-p)))

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
      meow-insert-mode
      (minibufferp)))

(defun lsp-bridge-not-in-multiple-cursors ()
  "If `multiple-cursors' mode is enable, hide completion menu."
  (not (and (featurep 'multiple-cursors)
            multiple-cursors-mode)))

(defun lsp-brige-not-in-chatgpt-response ()
  "Don't popup completion menu if ChatGPT is responsing."
  (not (and (boundp 'mind-wave-is-response-p)
            mind-wave-is-response-p)))

(defvar-local lsp-bridge-manual-complete-flag nil)

(defun lsp-bridge-not-complete-manually ()
  "If `lsp-bridge-complete-manually' is non-nil, hide completion menu."
  (or
   ;; Don't hide completion menu if it has show up.
   (acm-frame-visible-p acm-menu-frame)

   ;; Show completion menu when `lsp-bridge-complete-manually' and `lsp-bridge-manual-complete-flag' are non-nil.
   ;; If `lsp-bridge-complete-manually' is nil, not check `lsp-bridge-manual-complete-flag'.
   (not lsp-bridge-complete-manually)
   lsp-bridge-manual-complete-flag
   ))

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
  ;; we should use ABSOLUTE line number to be compatible with narrowed buffer
  (list :line (1- (line-number-at-pos nil t))
        :character (lsp-bridge--calculate-column)))

(defun lsp-bridge--position-in-org ()
  (list :line (1- (- (line-number-at-pos nil t)
                     (line-number-at-pos (plist-get (car (cdr (org-element-context))) :begin) t)))
        :character (lsp-bridge--calculate-column)))

(defvar-local lsp-bridge--before-change-begin-pos nil)
(defvar-local lsp-bridge--before-change-end-pos nil)
(defvar-local lsp-bridge--before-change-begin-point nil)
(defvar-local lsp-bridge--before-change-end-point nil)

(defun lsp-bridge-monitor-before-change (begin end)
  ;; Use `save-match-data' protect match data, avoid conflict with command call `search-regexp'.
  (save-match-data
    (when (lsp-bridge-has-lsp-server-p)
      ;; Send whole org src block to lsp server.
      (lsp-bridge-org-babel-send-src-block-to-lsp-server))

    ;; Set `lsp-bridge--before-change-begin-pos' and `lsp-bridge--before-change-end-pos'
    ;; if `lsp-bridge-has-lsp-server-p' or `lsp-bridge-is-remote-file'
    (when (or (lsp-bridge-has-lsp-server-p)
              (lsp-bridge-is-remote-file))
      (setq-local lsp-bridge--before-change-begin-point begin)
      (setq-local lsp-bridge--before-change-end-point end)

      (setq-local lsp-bridge--before-change-begin-pos (lsp-bridge--point-position begin))
      (setq-local lsp-bridge--before-change-end-pos (lsp-bridge--point-position end))
      )))

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
  (lsp-bridge-record-last-change-position))

(defun lsp-bridge-monitor-after-change (begin end length)
  ;; Nothing change actual if `begin' and `end' equal `lsp-bridge--before-change-begin-point' and `lsp-bridge--before-change-end-point'
  ;; Then we should not send any request to search backend.
  (unless (and (equal begin lsp-bridge--before-change-begin-point)
               (equal end lsp-bridge--before-change-end-point))
    ;; Use `save-match-data' protect match data, avoid conflict with command call `search-regexp'.
    (save-match-data
      (unless lsp-bridge-revert-buffer-flag
        (let ((change-text (buffer-substring-no-properties begin end)))
          ;; Record last command to `lsp-bridge-last-change-command'.
          (setq lsp-bridge-last-change-command (format "%s" this-command))

          ;; Record last change position to avoid popup outdate completions.
          (lsp-bridge-record-last-change-position)

          ;; Set `lsp-bridge-last-change-is-delete-command-p'
          (setq lsp-bridge-last-change-is-delete-command-p (> length 0))

          ;; Sync change for org babel if we enable it
          (lsp-bridge-org-babel-monitor-after-change begin end length)

          ;; Send LSP requests.
          (when (or (lsp-bridge-call-file-api-p)
                    (lsp-bridge-is-remote-file))

            ;; Uncomment below code to debug `change_file' protocol.
            ;; (message (format "change_file: '%s' '%s' '%s' '%s' '%s' '%s'"
            ;;                  length
            ;;                  lsp-bridge--before-change-begin-pos
            ;;                  lsp-bridge--before-change-end-pos
            ;;                  (lsp-bridge--position)
            ;;                  change-text
            ;;                  (buffer-substring-no-properties (line-beginning-position) (point))
            ;;                  ))

            ;; Send change_file request to trigger LSP completion.
            (lsp-bridge-call-file-api "change_file"
                                      lsp-bridge--before-change-begin-pos
                                      lsp-bridge--before-change-end-pos
                                      length
                                      change-text
                                      (lsp-bridge--position)
                                      (acm-char-before)
                                      (buffer-name)
                                      (acm-get-input-prefix))

            ;; Send inlay hint request.
            (lsp-bridge-try-send-inlay-hint-request))

          ;; Complete other non-LSP backends.
          (lsp-bridge-complete-other-backends)

          ;; Update search words backend.
          (lsp-bridge-search-words-update
           lsp-bridge--before-change-begin-pos
           lsp-bridge--before-change-end-pos
           change-text)
          )))))

(defun lsp-bridge-try-send-inlay-hint-request ()
  (when lsp-bridge-enable-inlay-hint
    (lsp-bridge-inlay-hint)))

(defun lsp-bridge-complete-other-backends ()
  (let* ((this-command-string (format "%s" this-command))
         (current-word (thing-at-point 'word t))
         (current-symbol (thing-at-point 'symbol t)))
    ;; TabNine search.
    (when (and acm-enable-tabnine
               (lsp-bridge-process-live-p))
      (lsp-bridge-tabnine-complete))

    ;; Copilot search.
    (when (and acm-enable-copilot
               (lsp-bridge-process-live-p)
               ;; Copilot backend not support remote file now, disable it temporary.
               (not (lsp-bridge-is-remote-file))
               ;; Don't enable copilot on Markdown mode, Org mode, ielm and minibuffer, very disruptive to writing.
               (not (or (derived-mode-p 'markdown-mode)
                        (eq major-mode 'org-mode)
                        (derived-mode-p 'inferior-emacs-lisp-mode)
                        (minibufferp))))
      (lsp-bridge-copilot-complete))

    ;; Codeium search.
    (when (and acm-enable-codeium
               (lsp-bridge-process-live-p)
               ;; Codeium backend not support remote file now, disable it temporary.
               (or (not (lsp-bridge-is-remote-file)) lsp-bridge-use-local-codeium)
               ;; Don't enable codeium on Markdown mode, Org mode, ielm and minibuffer, very disruptive to writing.
               (not (or (derived-mode-p 'markdown-mode)
                        (eq major-mode 'org-mode)
                        (derived-mode-p 'inferior-emacs-lisp-mode)
                        (minibufferp))))
      (lsp-bridge-codeium-complete))

    ;; emacs jupyter
    (when (and acm-enable-jupyter
               (lsp-bridge-process-live-p)
               (and lsp-bridge-enable-org-babel
                    (eq major-mode 'org-mode)
                    (not (lsp-bridge-is-org-temp-buffer-p)))
               (org-in-src-block-p 'inside)
               (string-prefix-p "jupyter" (plist-get (car (cdr (org-element-context))) :language)))
      (acm-backend-jupyter-record current-symbol))

    (when (and acm-enable-ctags
               (lsp-bridge-process-live-p))
      (unless (or (string-equal current-word "") (null current-word))
        (if (lsp-bridge-is-remote-file)
            ;; remote file buffer do not associate with an actual file on the disk
            ;; the buffer is created by lsp-bridge-opne-remote-file--response
            ;; hence (buffer-file-name) will return nil
            ;; should use buffer local variable lsp-bridge-remote-file-path
            (with-current-buffer (buffer-name)
              (lsp-bridge-remote-send-func-request "ctags_complete"
                                                   (list
                                                    current-word
                                                    (tramp-file-local-name lsp-bridge-remote-file-path)
                                                    (1- (point)))))
          (lsp-bridge-call-async "ctags_complete" current-word (buffer-file-name) (1- (point))))))

    ;; Search sdcv dictionary.
    (when acm-enable-search-sdcv-words
      ;; Search words if current prefix is not empty.
      (unless (or (string-equal current-word "") (null current-word))
        (lsp-bridge-call-async "search_sdcv_words_search" current-word)))

    ;; Search elisp symbol.
    (lsp-bridge-elisp-symbols-search current-symbol)

    ;; Send change file to search-words backend.
    (unless lsp-bridge-prohibit-completion
      (when (or buffer-file-name
                (lsp-bridge-is-remote-file))
        (let ((current-word (acm-backend-search-file-words-get-point-string)))
          ;; Search words if current prefix is not empty.
          (unless (or (string-equal current-word "") (null current-word))
            (if (lsp-bridge-is-remote-file)
                (lsp-bridge-remote-send-func-request "search_file_words_search" (list current-word))
              (lsp-bridge-call-async "search_file_words_search" current-word))))))

    ;; Send path search request when detect path string.
    (if (acm-in-string-p)
        (when-let* ((filename (lsp-bridge-elisp-get-filepath))
                    (dirname (ignore-errors (expand-file-name (file-name-directory filename)))))
          (if (lsp-bridge-is-remote-file)
              (let ((path (if (tramp-tramp-file-p dirname)
                              (tramp-file-name-localname (tramp-dissect-file-name dirname))
                            dirname)))
                (lsp-bridge-remote-send-func-request "search_paths_search"
                                                     (list path (file-name-base filename))))

            (when (file-exists-p dirname)
              (lsp-bridge-call-async "search_paths_search"
                                     dirname
                                     (file-name-base filename)
                                     ))))
      ;; We need cleanup `acm-backend-path-items' when cursor not in string.
      ;; Otherwise, other completion backend won't show up.
      (setq-local acm-backend-path-items nil))))

(defun lsp-bridge-elisp-get-filepath ()
  " Supports obtaining paths with spaces "
  (let* ((file-end (point))
         (filepath (save-excursion
                     (catch 'break
                       (let* ((file-path "")
                              (file-beg 0))
                         (while (acm-in-string-p)
                           (setq file-beg (car (bounds-of-thing-at-point 'filename)))
                           (if file-beg
                               (progn
                                 (setq file-path (buffer-substring file-beg file-end))
                                 (if (and (file-name-directory file-path) (file-exists-p (file-name-directory file-path)))
                                     (progn
                                       (throw 'break file-path))
                                   (goto-char (1- file-beg))))
                             (throw 'break nil))))))))
    filepath))

(defun lsp-bridge-elisp-symbols-update ()
  "We need synchronize elisp symbols to Python side when idle."
  (when (lsp-bridge-process-live-p)
    (let* ((symbols (acm-backend-elisp-get-symbols))
           (symbols-size (length symbols)))
      ;; Only synchronize when new symbol created.
      (unless (equal acm-backend-elisp-symbols-update-size symbols-size)
        (lsp-bridge-call-async "search_list_update"
                               "elisp"
                               symbols
                               acm-backend-elisp-search-max-number)
        (setq acm-backend-elisp-symbols-update-size symbols-size)))))

(defun lsp-bridge-elisp-symbols-search (current-symbol)
  (when (and (acm-is-elisp-mode-p)
             (not (acm-in-comment-p)))
    ;; Search words if current prefix is not empty.
    (unless (or (string-equal current-symbol "") (null current-symbol))
      (lsp-bridge-call-async "search_list_search" "elisp" current-symbol))))

(defun lsp-bridge-search-words-index-files ()
  "Index files when lsp-bridge python process finish."
  (if (lsp-bridge-is-remote-file)
      (let* ((host lsp-bridge-remote-file-host)
             (buffers (cl-remove-if-not (lambda (buf)
                                          (with-current-buffer buf
                                            (and (lsp-bridge-is-remote-file)
                                                 (string-equal lsp-bridge-remote-file-host host))))
                                        (buffer-list)))
             (files (mapcar (lambda (buf)
                              (with-current-buffer buf
                                lsp-bridge-remote-file-path))
                            buffers)))
        (lsp-bridge-remote-send-func-request "search_file_words_index_files" (list files)))
    (let ((files (cl-remove-if (lambda (elt)
                                 (or (null elt)
                                     (file-remote-p elt)
                                     (member (file-name-extension elt)
                                             lsp-bridge-search-words-prohibit-file-extensions)))
                               (mapcar (lambda (b) (lsp-bridge-buffer-file-name (buffer-file-name b))) (buffer-list)))))
      (lsp-bridge-call-async "search_file_words_index_files" files))))

(defun lsp-bridge-search-words-update (begin-pos end-pos change-text)
  (if (lsp-bridge-is-remote-file)
      (progn
        (lsp-bridge-remote-send-func-request "search_file_words_load_file" (list lsp-bridge-remote-file-path t)))
    (when (lsp-bridge-process-live-p)
      (lsp-bridge-call-async "search_file_words_change_buffer"
                             (substring-no-properties (buffer-name))
                             begin-pos
                             end-pos
                             change-text
                             ))))

(defun lsp-bridge-completion-ui-visible-p ()
  (acm-frame-visible-p acm-menu-frame))

(defun lsp-bridge-monitor-after-save ()
  (lsp-bridge-call-file-api "save_file" (buffer-name)))

(defcustom lsp-bridge-find-def-fallback-function nil
  "Fallback for find definition failure."
  :type 'function
  :group 'lsp-bridge)

(defcustom lsp-bridge-find-ref-fallback-function nil
  "Fallback for find referecences failure."
  :type 'function
  :group 'lsp-bridge)

(defcustom lsp-bridge-find-def-select-in-open-windows nil
  "If this option is turned on, when searching for function definitions,
already open windows will be selected instead of switching buffers.

Off by default."
  :type 'boolean
  :group 'lsp-bridge)

(defvar-local lsp-bridge-jump-to-def-in-other-window nil)

(defun lsp-bridge-find-def ()
  (interactive)
  (cond
   ((acm-is-elisp-mode-p)
    (lsp-bridge--record-mark-ring)
    (acm-backend-elisp-find-def)
    (lsp-bridge--set-mark-ring-in-new-buffer))
   (t
    (setq-local lsp-bridge-jump-to-def-in-other-window nil)
    (lsp-bridge-call-file-api "find_define" (lsp-bridge--position)))))

(defun lsp-bridge-find-def-other-window ()
  (interactive)
  (cond
   ((acm-is-elisp-mode-p)
    (lsp-bridge--record-mark-ring)
    (acm-backend-elisp-find-def)
    (lsp-bridge--set-mark-ring-in-new-buffer))
   (t
    (setq-local lsp-bridge-jump-to-def-in-other-window t)
    (lsp-bridge-call-file-api "find_define" (lsp-bridge--position)))))

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
      (switch-to-buffer buffer))
    (recenter)
    ))

(defun lsp-bridge-find-type-def ()
  (interactive)
  (setq-local lsp-bridge-jump-to-def-in-other-window nil)
  (lsp-bridge-call-file-api "find_type_define" (lsp-bridge--position)))

(defun lsp-bridge-find-type-def-other-window ()
  (interactive)
  (setq-local lsp-bridge-jump-to-def-in-other-window t)
  (lsp-bridge-call-file-api "find_type_define" (lsp-bridge--position)))

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

(defun lsp-bridge-find-def-fallback (position)
  (if (not (= (length lsp-bridge-peek-ace-list) 0))
      (progn
	    (if (nth 0 lsp-bridge-peek-ace-list)
	        (kill-buffer (nth 0 lsp-bridge-peek-ace-list)))
	    (switch-to-buffer (nth 2 lsp-bridge-peek-ace-list))
	    (goto-char (nth 1 lsp-bridge-peek-ace-list))))
  (message "[LSP-Bridge] No definition found.")
  (if (functionp lsp-bridge-find-def-fallback-function)
      (funcall lsp-bridge-find-def-fallback-function position)))

(defun lsp-bridge-find-ref-fallback (position)
  (message "[LSP-Bridge] No references found.")
  (if (functionp lsp-bridge-find-ref-fallback-function)
      (funcall lsp-bridge-find-ref-fallback-function position)))

(defun lsp-bridge-references--popup (references-content references-counter position)
  (if (> references-counter 0)
      (progn
        (lsp-bridge-ref-popup references-content references-counter)
        (message "[LSP-Bridge] Found %s references" references-counter))
    (lsp-bridge-find-ref-fallback position)))

(defun lsp-bridge-rename ()
  (interactive)
  (lsp-bridge-call-file-api "prepare_rename" (lsp-bridge--position))
  (let ((new-name (substring-no-properties (read-string "Rename to: " (thing-at-point 'symbol 'no-properties)))))
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

(defun lsp-bridge-rename--highlight (filename filehost bound-start bound-end)
  (lsp-bridge--with-file-buffer filename filehost
                                (lsp-bridge-flash-region
                                 (acm-backend-lsp-position-to-point bound-start)
                                 (acm-backend-lsp-position-to-point bound-end))))

(defun lsp-bridge-show-documentation ()
  (interactive)
  (lsp-bridge-call-file-api "hover" (lsp-bridge--position) "buffer"))

(defun lsp-bridge-popup-documentation ()
  (interactive)
  (lsp-bridge-call-file-api "hover" (lsp-bridge--position) "popup"))

(defun lsp-bridge-signature-help-fetch ()
  (interactive)
  (when (lsp-bridge-has-lsp-server-p)
    (unless (equal lsp-bridge-cursor-before-command lsp-bridge-cursor-after-command)
      (lsp-bridge-call-file-api "signature_help"
                                (if (and lsp-bridge-enable-org-babel (eq major-mode 'org-mode))
                                    (lsp-bridge--position-in-org)
                                  (lsp-bridge--position))
                                ))))

(defun lsp-bridge-pick-file-path (filename)
  ;; Remove `file://' and `:file://' prefix.
  (cond ((string-prefix-p "file://" filename)
         (setq filename (string-remove-prefix "file://" filename)))
        ((string-prefix-p ":file://" filename)
         (setq filename (string-remove-prefix ":file://" filename))))

  ;; Convert `%XX' sequences to `:'
  (setq filename (url-unhex-string filename))

  ;; Remove / before drive letter on Windows
  (when (string-match "^/[A-Za-z]:" filename)
    (setq filename (substring filename 1)))

  filename)

(defun lsp-bridge-file-apply-edits (filename edits &optional temp-buffer)
  (if temp-buffer
      ;; Apply edits to temp buffer.
      (with-current-buffer temp-buffer
        (acm-backend-lsp-apply-text-edits edits))

    ;; Pick filename from LSP return file string.
    (setq filename (lsp-bridge-pick-file-path filename))

    (find-file-noselect filename)
    (save-excursion
      (find-file filename)
      (acm-backend-lsp-apply-text-edits edits)))

  (setq-local lsp-bridge-prohibit-completion t))

(defun lsp-bridge--record-mark-ring ()
  "For implement jump and return back, we need call `lsp-bridge--record-mark-ring' in old buffer before jump.

Then we need call `lsp-bridge--set-mark-ring-in-new-buffer' in new buffer after jump.

`lsp-bridge--record-mark-ring' and `lsp-bridge--set-mark-ring-in-new-buffer' functions must be used in pairs."
  ;; Record postion.
  (let ((marker (set-marker (mark-marker) (point) (current-buffer))))
    (setq lsp-bridge-position-before-jump (copy-marker marker)))
  (setq mark-ring lsp-bridge-mark-ring))

(defun lsp-bridge--set-mark-ring-in-new-buffer ()
  (setq-local lsp-bridge-mark-ring (append (list lsp-bridge-position-before-jump) mark-ring)))

(defun lsp-bridge-find-window-match-filename (filename)
  (cl-dolist (window (window-list))
    (when (string-equal filename (buffer-file-name (window-buffer window)))
      (cl-return window))))

(defun lsp-bridge-define--jump (filename filehost position)
  (let (lsp-bridge-position-before-jump)
    (lsp-bridge--record-mark-ring)

    (if (or (string-equal filehost "") lsp-bridge-enable-with-tramp)
        (progn
          (setq filename (concat (cdr (assoc filehost lsp-bridge-tramp-alias-alist)) filename))
          (let ((match-window (lsp-bridge-find-window-match-filename filename)))
            (if (and lsp-bridge-find-def-select-in-open-windows
                     match-window)
                ;; Try select to window if `lsp-bridge-find-def-select-in-open-windows' is non-nil.
                (select-window match-window)
              ;; Jump to define.
              ;; Show define in other window if `lsp-bridge-jump-to-def-in-other-window' is non-nil.
              (if lsp-bridge-jump-to-def-in-other-window
                  (find-file-other-window filename)
                (find-file filename))
              ))

          ;; Init jump history in new buffer.
          (lsp-bridge--set-mark-ring-in-new-buffer)

          (lsp-bridge-define--jump-flash position))
      (lsp-bridge-call-async "open_remote_file" (format "%s:%s" filehost filename) position))
    ))

(defun lsp-bridge-define--jump-flash (position)
  ;; Jump to define postion.
  (goto-char (acm-backend-lsp-position-to-point position))
  (recenter)

  ;; Flash define line.
  (lsp-bridge-flash-line))

(defun lsp-bridge-switch-to-documentation-buffer (norecord)
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create lsp-bridge-buffer-documentation-buffer) norecord))

(defun lsp-bridge-show-documentation--callback (value)
  (let ((buffer (get-buffer-create lsp-bridge-buffer-documentation-buffer)))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert value)
      (setq-local truncate-lines nil)
      (acm-markdown-render-content t)
      (read-only-mode 1))
    (display-buffer buffer 'display-buffer-reuse-window)))

(defvar lsp-bridge-popup-documentation-frame nil)

(defun lsp-bridge-popup-documentation-scroll-up (&optional arg)
  (interactive)
  (when (frame-live-p lsp-bridge-popup-documentation-frame)
    (with-selected-frame lsp-bridge-popup-documentation-frame
      (apply #'scroll-up-command arg))))

(defun lsp-bridge-popup-documentation-scroll-down (&optional arg)
  (interactive)
  (when (frame-live-p lsp-bridge-popup-documentation-frame)
    (with-selected-frame lsp-bridge-popup-documentation-frame
      (apply #'scroll-down-command arg))))

(defun lsp-bridge-popup-documentation--callback (value)
  (let ((emacs-frame (or acm-frame--emacs-frame (selected-frame))))
    (with-current-buffer (get-buffer-create lsp-bridge-popup-documentation-buffer)
      (read-only-mode -1)
      (erase-buffer)
      (insert value)
      (setq-local truncate-lines nil)
      (acm-markdown-render-content))

    (acm-frame-new lsp-bridge-popup-documentation-frame
                   lsp-bridge-popup-documentation-buffer
                   "lsp bridge popup documentation frame"
                   (/ (frame-width emacs-frame) 2)
                   (/ (frame-height emacs-frame) 2)
                   )))

(defun lsp-bridge-hide-doc-tooltip ()
  (acm-frame-hide-frame lsp-bridge-popup-documentation-frame))

(defcustom lsp-bridge-signature-show-function 'message
  "Function to render signature help. Set to `lsp-bridge-signature-show-with-frame' to use the popup frame."
  :type 'function
  :group 'lsp-bridge)

(defcustom lsp-bridge-signature-show-with-frame-position "bottom-right"
  "The popup position of signature frame.

Default is `bottom-right', you can choose other value: `top-left', `top-right', `bottom-left', 'point'."
  :type 'string
  :group 'lsp-bridge)

(defcustom lsp-bridge-signature-buffer " *lsp-bridge-signature*"
  "Buffer for display signature information."
  :type 'string
  :group 'lsp-bridge)

(defvar lsp-bridge-signature-frame nil)

(defun lsp-bridge-hide-signature-tooltip ()
  (acm-frame-hide-frame lsp-bridge-signature-frame))

(defun lsp-bridge-signature-show-with-frame (str)
  "Use popup frame to show the STR signatureHelp string."
  (if (not (string-empty-p str))
      (progn
        (with-current-buffer (get-buffer-create lsp-bridge-signature-buffer)
          (erase-buffer)
          (insert str)
          (visual-line-mode 1)
          (current-buffer))

        (acm-frame-new lsp-bridge-signature-frame
                       lsp-bridge-signature-buffer
                       "lsp bridge signature frame"
                       nil
                       nil
                       lsp-bridge-signature-show-with-frame-position
                       ))
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

(defun lsp-bridge-enable-in-minibuffer ()
  (when (and lsp-bridge-enable-completion-in-minibuffer
             (where-is-internal #'completion-at-point (list (current-local-map))))
    (lsp-bridge-mode 1)
    ))

(add-hook 'minibuffer-setup-hook #'lsp-bridge-enable-in-minibuffer)

(defconst lsp-bridge--internal-hooks
  '((before-change-functions lsp-bridge-monitor-before-change nil t)
    (after-change-functions lsp-bridge-monitor-after-change nil t)
    (pre-command-hook lsp-bridge-monitor-pre-command nil t)
    (post-command-hook lsp-bridge-monitor-post-command nil t)
    (after-save-hook lsp-bridge-monitor-after-save nil t)
    (kill-buffer-hook lsp-bridge-close-buffer-file nil t)
    (before-revert-hook lsp-bridge-close-buffer-file nil t)
    (post-self-insert-hook lsp-bridge-monitor-post-self-insert 90 t)
    ))

(defvar lsp-bridge-mode-map (make-sparse-keymap))

(defvar lsp-bridge-signature-help-timer nil)

(defvar lsp-bridge-search-words-timer nil)

(defvar lsp-bridge-auto-format-code-timer nil)

(defcustom lsp-bridge-mode-lighter " LSPB"
  "Mode line lighter for LSP Bridge Mode."
  :type 'string
  :group 'lsp-bridge)

;;;###autoload
(define-minor-mode lsp-bridge-mode
  "LSP Bridge mode."
  :keymap lsp-bridge-mode-map
  :lighter lsp-bridge-mode-lighter
  :init-value nil
  (if lsp-bridge-mode
      (lsp-bridge--enable)
    (lsp-bridge--disable)))

(defun lsp-bridge--enable ()
  "Enable LSP Bridge mode."

  (add-hook 'post-command-hook #'lsp-bridge-start-process)

  (when lsp-bridge-disable-electric-indent
    ;; NOTE:
    ;; Don't enable `electric-indent-mode' in lsp-bridge, `electric-indent-post-self-insert-function'
    ;;
    ;; It will try adjust line indentation *AFTER* user insert character,
    ;; this additional indent action send excessive `change_file' request to lsp server.
    ;; LSP server will confused those indent action and return wrong completion candidates.
    ;;
    ;; Example, when you enable `electric-indent-mode', when you type `std::', you will got wrong completion candidates from LSP server.
    (electric-indent-local-mode -1))

  ;; Don't enable lsp-bridge when current buffer is acm buffer.
  (unless (or (equal (buffer-name (current-buffer)) acm-buffer)
              (equal (buffer-name (current-buffer)) acm-doc-buffer))
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
              ;; Init acm backend for org babel, and need elimination org temp buffer.
              (and lsp-bridge-enable-org-babel
                   (eq major-mode 'org-mode)
                   (not (lsp-bridge-is-org-temp-buffer-p))))
      ;; When user open buffer by `ido-find-file', lsp-bridge will throw `FileNotFoundError' error.
      ;; So we need save buffer to disk before enable `lsp-bridge-mode'.
      (when (not (lsp-bridge-is-remote-file))
        (when (and (buffer-file-name)
                   (not (file-exists-p (buffer-file-name))))
          (save-buffer)))

      (setq-local acm-backend-lsp-server-command-exist t)
      (setq-local acm-backend-lsp-cache-candidates nil)
      (setq-local acm-backend-lsp-completion-position nil)
      (setq-local acm-backend-lsp-completion-trigger-characters nil)
      (setq-local acm-backend-lsp-server-names nil)
      (setq-local acm-backend-lsp-filepath (lsp-bridge-get-buffer-truename))
      (setq-local acm-backend-lsp-items (make-hash-table :test 'equal))

      (when lsp-bridge-enable-signature-help
        (acm-run-idle-func lsp-bridge-signature-help-timer lsp-bridge-signature-help-fetch-idle 'lsp-bridge-signature-help-fetch))
      (when lsp-bridge-enable-auto-format-code
        (acm-run-idle-func lsp-bridge-auto-format-code-timer lsp-bridge-auto-format-code-idle 'lsp-bridge-auto-format-code)))

    (dolist (hook lsp-bridge--internal-hooks)
      (apply #'add-hook hook))

    (advice-add #'acm-hide :after #'lsp-bridge--completion-hide-advisor)))

(defun lsp-bridge-is-org-temp-buffer-p ()
  (or (string-match "\*org-src-fontification:" (buffer-name))
      (string-match "\*Org Src" (buffer-name))
      (string-match ".org-src-babel" (buffer-name))
      (equal "*Capture*" (buffer-name))))

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

(defun lsp-bridge--turn-off-lsp-feature (filename filehost)
  (lsp-bridge--with-file-buffer filename filehost
                                (setq-local acm-backend-lsp-server-command-exist nil)
                                ))

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

(defun lsp-bridge-workspace-list-symbol-at-point ()
  "Looks up the symbol under the cursor. If there's a marked region, use that instead."
  (interactive)
  (if (region-active-p)
	  (lsp-bridge-workspace-list-symbols (buffer-substring-no-properties (mark) (point)))
	(lsp-bridge-workspace-list-symbols (substring-no-properties (symbol-name (symbol-at-point))))))

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
          (lsp-bridge-jump-to-file symbol-file symbol-position)
          )))))

(defun lsp-bridge-jump-to-file (file position)
  (cond ((string-prefix-p "jdt://" file)
         (lsp-bridge-call-file-api "jdt_uri_resolver" (url-encode-url file) position))
        (t
         (find-file file)
         (goto-char (acm-backend-lsp-position-to-point position)))))

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
    (let* ((indent (symbol-value (lsp-bridge--get-indent-width major-mode)))
           (start (lsp-bridge--point-position (if (region-active-p) (region-beginning) (point))))
           (end (lsp-bridge--point-position (if (region-active-p) (region-end) (point)))))
      (lsp-bridge-call-file-api "try_formatting"
                                start
                                end
                                ;; Sometimes `c-basic-offset' return string `set-from-style', make some lsp server broken, such as, gopls,
                                ;; so we need convert indent to integer `4' to make sure code format works expectantly.
                                (if (eq indent 'set-from-style)
                                    4
                                  indent)))))

(defun lsp-bridge-format--update (filename edits)
  ;; We need set `inhibit-modification-hooks' to t to avoid GC freeze Emacs.
  (lsp-bridge-save-position
   (let ((inhibit-modification-hooks t))
     ;; Apply code format edits, not sort, just reverse order.
     (lsp-bridge-file-apply-edits filename edits)
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
             (plist-get change :edits)
             temp-buffer)))
         ((plist-get info :changes)
          (let* ((changes (plist-get info :changes))
                 (changes-number (/ (length changes) 2)))
            (dotimes (changes-index changes-number)
              (lsp-bridge-file-apply-edits
               (format "%s" (nth (* changes-index 2) changes))
               (nth (+ (* changes-index 2) 1) changes) temp-buffer))))))

  (setq-local lsp-bridge-prohibit-completion t))

(defun lsp-bridge-completion-item--update (info filehost)
  (let* ((filename (plist-get info :filepath))
         (key (plist-get info :key))
         (server-name (plist-get info :server))
         (additional-text-edits (plist-get info :additionalTextEdits))
         (documentation (plist-get info :documentation))
         (has-doc-p (and documentation
                         (not (string-equal documentation "")))))
    (lsp-bridge--with-file-buffer
        filename filehost
        ;; When lsp-bridge running in server, `acm-backend-lsp-items' maybe nil when receive `lsp-bridge-completion-item--update' response.
        ;; So we need check `acm-backend-lsp-items' value before update item documentation.
        (when-let ((server-lsp-items (gethash server-name acm-backend-lsp-items)))
          ;; Update `documentation' and `additionalTextEdits'
          (when-let (item (gethash key server-lsp-items))
            (when additional-text-edits
              (plist-put item :additionalTextEdits additional-text-edits))

            (when has-doc-p
              (plist-put item :documentation documentation))

            (puthash key item (gethash server-name acm-backend-lsp-items)))

          (if has-doc-p
              ;; Show doc frame if `documentation' exist and not empty.
              (acm-doc-try-show t)
            ;; Hide doc frame immediately.
            (acm-doc-hide))
          ))))

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

(defun lsp-bridge--not-acm-doc-markdown-buffer ()
  "`markdown-code-fontification:*' buffer will create when we render markdown code in acm doc buffer.

We need exclude `markdown-code-fontification:*' buffer in `lsp-bridge-monitor-before-change' and `lsp-bridge-monitor-after-change'."
  (not (string-prefix-p " markdown-code-fontification:" (buffer-name (current-buffer)))))

(defun lsp-bridge--not-mind-wave-chat-buffer ()
  "Not mind-wave's `*.chat' buffer."
  (or
   (not (buffer-file-name))
   (not (string-equal (file-name-extension (buffer-file-name)) "chat"))))

;;;###autoload
(defun global-lsp-bridge-mode ()
  (interactive)

  (dolist (hook lsp-bridge-default-mode-hooks)
    (add-hook hook (lambda ()
                     (when (and (lsp-bridge--not-mind-wave-chat-buffer)
                                (lsp-bridge--not-acm-doc-markdown-buffer))
                       (lsp-bridge-mode 1))))))

(with-eval-after-load 'evil
  (evil-add-command-properties #'lsp-bridge-find-def :jump t)
  (evil-add-command-properties #'lsp-bridge-find-references :jump t)
  (evil-add-command-properties #'lsp-bridge-find-impl :jump t))

(defun lsp-bridge--rename-file-advisor (orig-fun &optional arg &rest args)
  (let* ((current-file-name (buffer-file-name))
         (current-file-name (and current-file-name
                                 (expand-file-name current-file-name)))
         (new-name (expand-file-name (nth 0 args))))
    (when (and lsp-bridge-mode
               (boundp 'acm-backend-lsp-filepath)
               current-file-name
               (string-equal current-file-name new-name))
      (lsp-bridge-call-file-api "rename_file" new-name)
      (if (lsp-bridge-is-remote-file)
          (lsp-bridge-remote-send-func-request "close_file" (list acm-backend-lsp-filepath))
        (lsp-bridge-call-async "close_file" acm-backend-lsp-filepath))
      (set-visited-file-name new-name t t)
      (setq-local acm-backend-lsp-filepath new-name)))
  (apply orig-fun arg args))
(advice-add #'rename-file :around #'lsp-bridge--rename-file-advisor)

;; We use `lsp-bridge-revert-buffer-flag' var avoid lsp-bridge send change_file request while execute `revert-buffer' command.
(defun lsp-bridge--revert-buffer-advisor (orig-fun &optional arg &rest args)
  ;; We need clean diagnostic overlays before revert action,
  ;; otherwise some diagnostic overlays will keep in buffer after revert.
  (lsp-bridge-diagnostic-hide-overlays)

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
    (if (lsp-bridge-is-remote-file)
        (lsp-bridge-remote-send-func-request "tabnine_complete"
                                             (list (buffer-substring-no-properties before-point (point))
                                                   (buffer-substring-no-properties (point) after-point)
                                                   (or lsp-bridge-remote-file-path nil)
                                                   (= before-point buffer-min)
                                                   (= after-point buffer-max)
                                                   max-num-results))
      (lsp-bridge-call-async "tabnine_complete"
                             (buffer-substring-no-properties before-point (point))
                             (buffer-substring-no-properties (point) after-point)
                             (or (buffer-file-name) nil)
                             (= before-point buffer-min)
                             (= after-point buffer-max)
                             max-num-results))))

(defvar lsp-bridge-use-local-codeium nil
  "Whether use local codeium when editing remote file.")

(defun lsp-bridge-codeium-complete ()
  (interactive)
  (let ((all-text (buffer-substring-no-properties (point-min) (point-max)))
        (language
         ;; https://github.com/Exafunction/codeium.el/blob/0240805690c685de9b75c953af2867b6fcc61208/codeium.el#L306
         (let ((mode major-mode))
           (while (not (alist-get mode acm-backend-codeium-language-alist))
             (setq mode (get mode 'derived-mode-parent)))
           (alist-get mode acm-backend-codeium-language-alist))))
    (if (and (lsp-bridge-is-remote-file) (not lsp-bridge-use-local-codeium))
        (lsp-bridge-remote-send-func-request "codeium_complete"
                                             (list
                                              (1- (point))
                                              (symbol-name major-mode)
                                              tab-width
                                              all-text
                                              (not indent-tabs-mode)
                                              (acm-get-input-prefix)
                                              language))
      (lsp-bridge-call-async "codeium_complete"
                             (1- (point))
                             (symbol-name major-mode)
                             tab-width
                             all-text
                             (not indent-tabs-mode)
                             (acm-get-input-prefix)
                             language))))

(defun lsp-bridge-copilot-complete ()
  (interactive)
  (setq-local acm-backend-lsp-fetch-completion-item-ticker nil)
  (let ((all-text (buffer-substring-no-properties (point-min) (point-max)))
        (relative-path
         ;; from copilot.el
         (cond
          ((not buffer-file-name)
           "")
          ((fboundp 'projectile-project-root)
           (file-relative-name
            (lsp-bridge-get-buffer-file-name-text)
            (projectile-project-root)))
          ((boundp 'vc-root-dir)
           (file-relative-name
            (lsp-bridge-get-buffer-file-name-text)
            (vc-root-dir)))
          (t
           (file-name-nondirectory buffer-file-name)))))
    (if (lsp-bridge-is-remote-file)
        (lsp-bridge-remote-send-func-request "copilot_complete"
                                             (list
                                              (lsp-bridge--position)
                                              (symbol-name major-mode)
                                              (buffer-file-name)
                                              relative-path
                                              tab-width
                                              all-text
                                              (not indent-tabs-mode)))
      (lsp-bridge-call-async "copilot_complete"
                             (lsp-bridge--position)
                             (symbol-name major-mode)
                             (buffer-file-name)
                             relative-path
                             tab-width
                             all-text
                             (not indent-tabs-mode)))))

(defun lsp-bridge-search-backend--record-items (backend-name items)
  (set (make-local-variable (intern (format "acm-backend-%s-items" backend-name))) items)
  (set (make-local-variable (intern (format "acm-backend-%s-cache-candiates" backend-name))) nil)

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

(defun lsp-bridge--record-work-done-progress (progress)
  (when acm-backend-lsp-show-progress
    (unless (active-minibuffer-window)
      (message progress))))

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
              (if (lsp-bridge-process-live-p)
                  'lsp-bridge-alive-mode-line
                'lsp-bridge-kill-mode-line))

  (when lsp-bridge-server
    (propertize "lsp-bridge"'face mode-face)))

(when lsp-bridge-enable-mode-line
  (add-to-list 'mode-line-misc-info
               `(lsp-bridge-mode ("" lsp-bridge--mode-line-format " "))))

(defvar-local lsp-bridge-remote-file-flag nil)
(defvar-local lsp-bridge-remote-file-tramp-method nil)
(defvar-local lsp-bridge-remote-file-user nil)
(defvar-local lsp-bridge-remote-file-host nil)
(defvar-local lsp-bridge-remote-file-port nil)
(defvar-local lsp-bridge-remote-file-path nil)
(defvar lsp-bridge-remote-file-pattern
  (rx bos (? "/")
      ;; username
      (? (seq (any "a-z_") (* (any "a-z0-9_.-"))) "@")
      ;; host ip
      (repeat 3 (seq (repeat 1 3 (any "0-9")) ".")) (repeat 1 3 (any "0-9"))
      ;; ssh port
      (? ":" (+ digit))
      ;; path
      (? ":") (? "~") (* nonl) eol
      ))

(defun lsp-bridge-open-or-create-file (filepath)
  "Open FILEPATH, if it exists. If not, create it and its parent directories."
  (if (file-exists-p filepath)
      (with-temp-buffer
        (insert-file-contents filepath)
        (buffer-string))
    (make-directory (file-name-directory filepath) t)
    (with-temp-file filepath)
    ""))

(defun lsp-bridge-open-remote-file ()
  (interactive)
  (let* ((ip-file (concat (lsp-bridge--user-emacs-directory-func)
                          (file-name-as-directory "lsp-bridge")
                          (file-name-as-directory "remote_file")
                          "ip.txt"))
         (path (completing-read "Open remote file (ip:path): "
                                (with-temp-buffer
                                  (insert (lsp-bridge-open-or-create-file ip-file))
                                  (split-string (buffer-string) "\n" t)))))
    (lsp-bridge-call-async "open_remote_file" path (list :line 0 :character 0))))

(cl-defmacro lsp-bridge--conditional-update-tramp-file-info (tramp-file-name path host &rest body)
  "Conditionally execute BODY with the buffer associated with TRAMP-FILE-NAME.

If the buffer is created by TRAMP with TRAMP-FILE-NAME, BODY is executed within
the context of that buffer. If the buffer is created by
`lsp-bridge-open-remote-file--response', `lsp-bridge--with-file-buffer' is used
 with PATH and HOST to find the buffer, then BODY is executed within that buffer."

  `(if (get-file-buffer ,tramp-file-name)
       (with-current-buffer (get-file-buffer ,tramp-file-name)
         ,@body)
     ;; lsp-bridge--with-file-buffer is another macro, carefully expand it
     ,(macroexpand `(lsp-bridge--with-file-buffer ,path ,host ,@body))))

(defun lsp-bridge-update-tramp-file-info (tramp-file-name tramp-connection-info host path)
  (unless (assoc host lsp-bridge-tramp-alias-alist)
    (push `(,host . ,tramp-connection-info) lsp-bridge-tramp-alias-alist))

  (unless (assoc tramp-connection-info lsp-bridge-tramp-connection-info)
    (push `(,tramp-connection-info . ,host) lsp-bridge-tramp-connection-info))

  (lsp-bridge--conditional-update-tramp-file-info tramp-file-name path host
                                                  (setq-local lsp-bridge-remote-file-flag t)
                                                  (setq-local lsp-bridge-remote-file-host host)
                                                  (setq-local lsp-bridge-remote-file-path path)

                                                  (read-only-mode -1)

                                                  (add-hook 'kill-buffer-hook 'lsp-bridge-remote-kill-buffer nil t)
                                                  (setq lsp-bridge-tramp-sync-var t)
                                                  (message "[LSP-Bridge] remote file %s updated info successfully."
                                                           (buffer-file-name))))

(defun lsp-bridge-tramp-show-hostnames ()
  (interactive)
  (lsp-bridge-call-async "message_hostnames"))


(defcustom lsp-bridge-disable-electric-indent nil
  "`electric-indent-post-self-insert-function' will cause return wrong completion candidates from LSP server, such as type `std::' in C++.

Please turn this option on if you want fix `std::' completion candidates, at same time, `electric-indent-mode' will disable by lsp-bridge.

Please keep this option off if you need `electric-indent-mode' feature more.

It's a bug of `electric-indent-mode' that it will try adjust line indentation *AFTER* user insert character,
this additional indent action send excessive `change_file' request to lsp server.
LSP server will confused those indent action and return wrong completion candidates.

I haven't idea how to make lsp-bridge works with `electric-indent-mode', PR are welcome.")


(defun lsp-bridge-sync-tramp-remote ()
  (interactive)
  (let* ((file-name (lsp-bridge-get-buffer-file-name-text))
         (tramp-vec (tramp-dissect-file-name file-name))
         (tramp-method (tramp-file-name-method tramp-vec))
         (user (tramp-file-name-user tramp-vec))
         (host (tramp-file-name-host tramp-vec))
         (port (tramp-file-name-port tramp-vec))
         (path (tramp-file-name-localname tramp-vec))
         (tramp-connection-info (substring file-name 0 (+ 1 (string-match ":" file-name (+ 1 (string-match ":" file-name))))))
         (ip-host (cdr (assoc tramp-connection-info lsp-bridge-tramp-connection-info))))

    (if (not ip-host)
        (when (and (not (member tramp-method '("sudo" "sudoedit" "su" "doas")))
                   (not (member host lsp-bridge-tramp-blacklist)))
          (read-only-mode 1)
          (lsp-bridge-call-async "sync_tramp_remote" file-name user host port path))
      (lsp-bridge--conditional-update-tramp-file-info file-name path ip-host
                                                      (setq-local lsp-bridge-remote-file-flag t)
                                                      (setq-local lsp-bridge-remote-file-host ip-host)
                                                      (setq-local lsp-bridge-remote-file-path path)

                                                      (add-hook 'kill-buffer-hook 'lsp-bridge-remote-kill-buffer nil t)
                                                      (setq lsp-bridge-tramp-sync-var t)))))

(defun lsp-bridge-get-match-buffer-by-filehost (remote-file-host)
  (cl-dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when-let* ((match-buffer (string-equal lsp-bridge-remote-file-host remote-file-host)))
        (cl-return buffer)))))

(defun lsp-bridge-construct-tramp-file-name (method user host port path)
  "Construct a TRAMP file name from components: METHOD USER HOST PORT PATH.

SSH tramp file name is like /ssh:user@host#port:path"
  (concat "/"
          method ":"
          (when user (concat user "@"))
          host
          (when port (concat "#" port))
          ":" path))

(defun lsp-bridge-remote-reconnect (remote-file-host)
  "Restore TRAMP connection infomation of REMOTE-FILE-HOST."
  (when lsp-bridge-remote-start-automatically
    (with-current-buffer (lsp-bridge-get-match-buffer-by-filehost remote-file-host)
      ;; unify handling logic for buffer created by `lsp-bridge-open-remote-file' or open by tramp
      ;; override the buffer-file-name used by `lsp-bridge-sync-tramp-remote'
      ;; if buffer is created by lsp-bridge-open-remote-file then (buffer-file-name) returns nil
      (let ((buffer-file-name (or (buffer-file-name)
                                  (lsp-bridge-construct-tramp-file-name lsp-bridge-remote-file-tramp-method
                                                                        lsp-bridge-remote-file-user
                                                                        lsp-bridge-remote-file-host
                                                                        lsp-bridge-remote-file-port
                                                                        lsp-bridge-remote-file-path))))
        (lsp-bridge-sync-tramp-remote)))))

(defvar lsp-bridge-remote-file-window nil)
(defun lsp-bridge-open-remote-file--response(tramp-method user host port path content position)
  (if lsp-bridge-remote-file-window
      (progn
        (select-window lsp-bridge-remote-file-window)
        (setq lsp-bridge-remote-file-window nil)))
  (let ((buf-name (format "[LBR] %s" (file-name-nondirectory path))))
    (lsp-bridge--record-mark-ring)

    (with-current-buffer (get-buffer-create buf-name)
      (text-mode)

      (read-only-mode -1)
      (erase-buffer)
      (insert (lsp-bridge-decode-base64 content))
      (goto-char (point-min))

      (add-hook 'kill-buffer-hook 'lsp-bridge-remote-kill-buffer nil t)

      (let ((mode (lsp-bridge-get-mode-name-from-file-path path)))
        (when mode
          (let ((lsp-bridge-remote-file-flag t)
                (lsp-bridge-remote-file-tramp-method tramp-method)
                (lsp-bridge-remote-file-user user)
                (lsp-bridge-remote-file-host host)
                (lsp-bridge-remote-file-port port)
                (lsp-bridge-remote-file-path path))
            (funcall mode)))))

    (switch-to-buffer buf-name)

    (unless (equal position (list :line 0 :character 0))
      (lsp-bridge--set-mark-ring-in-new-buffer)

      (lsp-bridge-define--jump-flash position))

    (setq-local lsp-bridge-remote-file-flag t)
    (setq-local lsp-bridge-remote-file-tramp-method tramp-method)
    (setq-local lsp-bridge-remote-file-user user)
    (setq-local lsp-bridge-remote-file-host host)
    (setq-local lsp-bridge-remote-file-port port)
    (setq-local lsp-bridge-remote-file-path path)

    ;; Always enable lsp-bridge for remote file.
    ;; Remote file can always edit and update content even some file haven't corresponding lsp server, such as *.txt
    (lsp-bridge-mode 1))
  (when lsp-bridge-ref-open-remote-file-go-back-to-ref-window
    (lsp-bridge-switch-to-ref-window)
    (setq lsp-bridge-ref-open-remote-file-go-back-to-ref-window nil)))

(defun lsp-bridge-remote-kill-buffer ()
  (when lsp-bridge-remote-file-flag
    (lsp-bridge-call-async "close_remote_file" lsp-bridge-remote-file-host lsp-bridge-remote-file-path)
    ))

(defun lsp-bridge-decode-base64 (base64-string)
  (decode-coding-string (base64-decode-string base64-string) 'utf-8))

(defun lsp-bridge-remote-send-lsp-request (method &rest args)
  (lsp-bridge-deferred-chain
    (lsp-bridge-epc-call-deferred lsp-bridge-epc-process
                                  (read "lsp_request")
                                  (append
                                   (list lsp-bridge-remote-file-host lsp-bridge-remote-file-path method) args))))

(defun lsp-bridge-remote-send-func-request (method &rest args)
  (lsp-bridge-deferred-chain
    (lsp-bridge-epc-call-deferred lsp-bridge-epc-process
                                  (read "func_request")
                                  (append
                                   (list lsp-bridge-remote-file-host lsp-bridge-remote-file-path method) args))))

(defun lsp-bridge-remote-save-buffer ()
  (interactive)
  (if lsp-bridge-remote-file-flag
      (lsp-bridge-call-async "save_remote_file" lsp-bridge-remote-file-host lsp-bridge-remote-file-path)
    (message "lsp-bridge-remote-save-buffer only for lsp-bridge-remote file.")))

(defun lsp-bridge-indent-left (start end)
  (interactive "r")
  (let ((indent (symbol-value (lsp-bridge--get-indent-width major-mode))))
    (indent-rigidly start end (- indent))))

(defun lsp-bridge-indent-right (start end)
  (interactive "r")
  (let ((indent (symbol-value (lsp-bridge--get-indent-width major-mode))))
    (indent-rigidly start end indent)))

(provide 'lsp-bridge)

;;; lsp-bridge.el ends here
