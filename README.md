English | [简体中文](./README.zh-CN.md)

# lsp-bridge

lsp-bridge's goal is to become the fastest LSP client in the Emacs.

lsp-bridge use python threading technology build cache bridge between Emacs and LSP server, you will never feel stuck when you write the code.

<img src="./screenshot.png">

## Installation

1. Install Python dependences: [python-epc](https://github.com/tkf/python-epc)
2. Install Elisp dependences: [corfu](https://github.com/minad/corfu) ,[all-the-icons](https://github.com/domtronn/all-the-icons.el), [Orderless](https://github.com/oantolin/orderless), [posframe](https://github.com/tumashu/posframe), [markdown-mode](https://github.com/jrblevin/markdown-mode), [yasnippet](https://github.com/joaotavora/yasnippet)
3. Clone or download this repository (path of the folder is the `<path-to-lsp-bridge>` used below).
4. Add follow code in your ~/.emacs:

```elisp
(add-to-list 'load-path "<path-to-lsp-bridge>")

(require 'yasnippet)
(require 'lsp-bridge)             ;; load lsp-bridge
(require 'lsp-bridge-orderless)   ;; make lsp-bridge support fuzzy match, optional
(require 'lsp-bridge-icon)        ;; show icon for completion items, optional

(global-corfu-mode)               ;; use corfu as completion ui
(global-lsp-bridge-mode)
(yas-global-mode 1)
```

## Commands

* lsp-bridge-find-def: jump to the definition position
* lsp-bridge-find-def-other-window: jump to the definition position in other-window
* lsp-bridge-find-impl: jump to the implementation position
* lsp-bridge-find-impl-other-window: jump to the implementation position in other-window
* lsp-bridge-return-from-def: return to location before lsp-bridge-find-def
* lsp-bridge-find-references: traversing across code references (fork from color-rg.el)
* lsp-bridge-lookup-documentation: lookup documentation of cursor 
* lsp-bridge-rename: rename the cursor content
* lsp-bridge-show-signature-help-in-minibuffer: show signature help in minibuffer manually (move cursor to parameters area will show signature help automatically)
* lsp-bridge-restart-process: restart lsp-bridge process (only used for development)

## Customize language server configuration
lsp-bridge default configuration for lang server store at [lsp-bridge/langserver](https://github.com/manateelazycat/lsp-bridge/tree/master/langserver).

Anyway you can customize server configuration with below priority:
1. ```lsp-bridge-get-lang-server-by-project```: write your own function to get server configuration base on project-path and file-path, default this function is nil
2. ```lsp-bridge-lang-server-extension-list```: get server configuration base on file extension, such as, we launch ```volar``` server instead ```javascript``` server when open *.vue file
3. ```lsp-bridge-lang-server-mode-list```: get server configuration base on major-mode

## Add support for new language?

1. Create settings file under lsp-bridge/langserver, such as `pyright.json` is use for pyright (windows use `pyright_windows.json`, macOS use `pyright_darwin.json`).
2. Add `(mode . server_name)` in ```lsp-bridge-lang-server-list```, such as `(python-mode . "pyright")`
3. Add new mode-hook to `lsp-bridge-default-mode-hooks`

Welcome send PR to help us improve support for LSP servers, thank you!

## Supported language servers

1. [clangd](https://github.com/clangd/clangd) (c, c++)
2. [pyright](https://github.com/microsoft/pyright) (python)
3. [solargraph](https://github.com/castwide/solargraph) (ruby)
4. [rust-analyzer](https://github.com/rust-lang/rust-analyzer) (rust)
5. [elixirLS](https://github.com/elixir-lsp/elixir-ls) (elixir) Note: please ensure export `elixir-ls` release directory in your system PATH at first.
6. [gopls](https://github.com/golang/tools/tree/master/gopls) (go)
7. [hls](https://github.com/haskell/haskell-language-server) (haskell)
8. [dart-analysis-server](https://github.com/dart-lang/sdk/tree/master/pkg/analysis_server) (dart)
9. [metals](https://scalameta.org/metals/) (scala)
10. [typescript](https://www.npmjs.com/package/typescript) (typescript, javascript)
11. [ocamllsp](https://github.com/ocaml/ocaml-lsp) (ocaml)
12. [erlang-ls](https://github.com/erlang-ls/erlang_ls) (erlang)
13. [texlab](https://github.com/latex-lsp/texlab) (latex)
14. [eclipse.jdt.ls](https://projects.eclipse.org/projects/eclipse.jdt.ls) (java) Note: please ensure export `org.eclipse.jdt.ls.product/target/repository/bin` in your system PATH at first.
15. [clojure-lsp](https://github.com/clojure-lsp/clojure-lsp) (clojure)
16. [bash-language-server](https://github.com/bash-lsp/bash-language-server) (bash)
17. [volar](https://github.com/johnsoncodehk/volar) (vue)
18. [sumneko](https://github.com/sumneko/lua-language-server) (lua) Note: please ensure export `bin` under sumneko installation in your system PATH at first.
19. [wxml-language-server](https://github.com/chemzqm/wxml-languageserver) (wxml)
20. [vscode-html-language-server](https://github.com/hrsh7th/vscode-langservers-extracted) (html)
21. [vscode-css-language-server](https://github.com/hrsh7th/vscode-langservers-extracted) (css)
22. [elm-language-server](https://github.com/elm-tooling/elm-language-server) (elm)

### Features need to finish:

- [ ] Show signature help with eldoc 
- [ ] Code action
- [ ] Inline Value
- [ ] One file open multi-server, and mixed multi-result to corfu menu

### Features that won't support
lsp-bridge goal is become the fastest LSP client in the Emacs, not the complete implementation of LSP protocol.

The following function Emacs can do better, we will not repeat in lsp-bridge:
1. Code format: each LSP server has its own formatting specification, with the formatting function of Emacs, we will get more detail control
2. Diagnostics: [Flycheck](https://www.flycheck.org/en/latest/) or [Flymake](https://www.gnu.org/software/emacs/manual/html_node/flymake/Using-Flymake.html) is better chooise
3. Syntax highlight: [Tree-sitter](https://tree-sitter.github.io/tree-sitter/) is a wonderful incremental parsing library to implement highlight code

## Join development

Below is framework of lsp-bridge:

<img src="./framework.png">

Please read [LSP Specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/) and [The Design of lsp-bridge](https://manateelazycat.github.io/emacs/2022/05/12/lsp-bridge.html) first.

Then turn on option ```lsp-bridge-enable-log``` and happy hacking! ;)

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
