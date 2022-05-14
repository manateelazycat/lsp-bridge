# lsp-bridge

lsp-bridge's goal is to become the fastest LSP client in the Emacs.

lsp-bridge use python threading technology build cache bridge between Emacs and LSP server, you will never feel stuck when you write the code.

<img src="./screenshot.png">

## Installation

1. Install Python dependences: [python-epc](https://github.com/tkf/python-epc)
2. Install Elisp dependences: [corfu](https://github.com/minad/corfu) ,[all-the-icons](https://github.com/domtronn/all-the-icons.el), [Orderless](https://github.com/oantolin/orderless), [posframe](https://github.com/tumashu/posframe)
3. Clone or download this repository (path of the folder is the `<path-to-lsp-bridge>` used below).
4. Add follow code in your ~/.emacs:

```
(add-to-list 'load-path "<path-to-lsp-bridge>")

(require 'lsp-bridge)             ;; load lsp-bridge
(global-corfu-mode)               ;; use corfu as completion ui

(require 'lsp-bridge-orderless)   ;; make lsp-bridge support fuzzy match, optional
(require 'lsp-bridge-icon)        ;; show icon for completion items, optional

;; Enable auto completion in elisp mode.
(dolist (hook (list
               'emacs-lisp-mode-hook
               ))
  (add-hook hook (lambda ()
                   (setq-local corfu-auto t)
                   )))

;; Enable lsp-bridge.
(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'python-mode-hook
               'ruby-mode-hook
               'rust-mode-hook
               'elixir-mode-hook
               'go-mode-hook
               'haskell-mode-hook
               'haskell-literate-mode-hook
               'dart-mode-hook
               'scala-mode-hook
               'typescript-mode-hook
               'typescript-tsx-mode-hook
               'js2-mode-hook
               'js-mode-hook
               'rjsx-mode-hook
               'tuareg-mode-hook
               'latex-mode-hook
               'Tex-latex-mode-hook
               'texmode-hook
               'context-mode-hook
               'texinfo-mode-hook
               'bibtex-mode-hook
               'clojure-mode-hook
               'clojurec-mode-hook
               'clojurescript-mode-hook
               'clojurex-mode-hook
               'sh-mode-hook
               'web-mode-hook
               ))
  (add-hook hook (lambda ()
                   (setq-local corfu-auto nil)  ;; let lsp-bridge control when popup completion frame
                   (lsp-bridge-mode 1)
                   )))
```

## Commands

* lsp-bridge-find-def: jump to the definition position
* lsp-bridge-return-from-def: return to location before lsp-bridge-find-def
* lsp-bridge-find-references: traversing across code references (fork from color-rg.el)
* lsp-bridge-lookup-documentation: lookup documentation of cursor 
* lsp-bridge-rename: rename the cursor content
* lsp-bridge-restart-process: restart lsp-bridge process (only used for development)

## Customize language server configuration
lsp-bridge default configuration for lang server store at [lsp-bridge/langserver](https://github.com/manateelazycat/lsp-bridge/tree/master/langserver).

Anyway you can customize server configuration with below priority:
1. ```lsp-bridge-get-lang-server-by-project```: write your own function to get server configuration base on project-path and file-path, default this function is nil
2. ```lsp-bridge-lang-server-extension-list```: get server configuration base on file extension, such as, we launch ```volar``` server instead ```javascript``` server when open *.vue file
3. ```lsp-bridge-lang-server-mode-list```: get server configuration base on major-mode

## Add support for new language?

1. Create settings file under lsp-bridge/langserver, such as `pyright.json` is use for pyright (windows platform need use `pyright_nt.json`).
2. Add `(mode . server_name)` in lsp-bridge-lang-server-list, such as `(python-mode . "pyright")`
3. Then add `(lsp-bridge-enable)` in mode-hook for test.

Welcome send PR to help us improve support for LSP servers, thank you!

## Supported language servers

1. clangd (c, c++)
2. pyright (python)
3. solargraph (ruby)
4. rust-analyzer (rust)
5. elixirLS (elixir) Note: please ensure export `elixir-ls` release directory in your system PATH at first.
6. gopls (go)
7. hls (haskell)
8. dart_analysis_server (dart)
9. metals (scala)
10. typescript (typescript, javascript)
11. ocamllsp (ocaml)
12. erlang_ls (erlang)
13. texlab (latex)
14. eclipse.jdt.ls (java) Note: please ensure export `org.eclipse.jdt.ls.product/target/repository/bin` in your system PATH at first.
15. clojure-lsp (clojure)
16. bash-language-server (bash)
17. volar (vue)

## Report bug

Please use `emacs -q` and load a minimal setup with only lsp-bridge to verify that the bug is reproducible. If `emacs -q` works fine, probably something is wrong with your Emacs config.

If the problem persists, please report it [here](https://github.com/manateelazycat/lsp-bridge/issues/new) with `*lsp-bridge*` buffer content, it contains many clues that can help us locate the problem faster.

If you get a segfault error, please use the following way to collect crash information:

1. Install gdb and turn on option `lsp-bridge-enable-debug`
2. Use the command `lsp-bridge-stop-process` to stop the current process
3. Restart lsp-bridge, send issue with `*lsp-bridge*` buffer content when next crash

## Contributor

<a href = "https://github.com/manateelazycat/lsp-bridge/graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=manateelazycat/lsp-bridge"/>
</a>
