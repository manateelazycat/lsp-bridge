English | [简体中文](./README.zh-CN.md)

# lsp-bridge

Lsp-bridge's goal is to become the fastest LSP client in Emacs.

Lsp-bridge uses python's threading technology to build caches that bridge Emacs and LSP server. Lsp-bridge will provide smooth completion experience without compromise to slow down emacs' performance.

<img src="./screenshot.png">

## Installation

1. Install Python dependencies: [python-epc](https://github.com/tkf/python-epc)
2. Install Elisp dependencies:
+ [corfu](https://github.com/minad/corfu)
+ [orderless](https://github.com/oantolin/orderless) 
+ [all-the-icons](https://github.com/domtronn/all-the-icons.el) (need execute command `all-the-icons-install-fonts` to install all-the-icons fonts)
+ [posframe](https://github.com/tumashu/posframe)
+ [markdown-mode](https://github.com/jrblevin/markdown-mode)
+ [yasnippet](https://github.com/joaotavora/yasnippet)
3. Clone or download this repository (path of the folder is the `<path-to-lsp-bridge>` used below).
4. Add following code in your ~/.emacs:

```elisp
(add-to-list 'load-path "<path-to-lsp-bridge>")

(require 'yasnippet)
(require 'lsp-bridge)
(require 'lsp-bridge-icon)        ;; show icons for completion items, optional
(require 'lsp-bridge-jdtls)       ;; provide Java third-party library jump and -data directory support, optional
(yas-global-mode 1)

(require 'corfu)
(require 'corfu-info)
(require 'corfu-history)
(require 'lsp-bridge-orderless)   ;; make lsp-bridge support fuzzy match, optional
(corfu-history-mode t)
(global-lsp-bridge-mode)
(when (> (frame-pixel-width) 3000) (custom-set-faces '(corfu-default ((t (:height 1.3))))))  ;; adjust default font height when running in HiDPI screen.
```

## Commands

* lsp-bridge-find-def: jump to the definition
* lsp-bridge-find-def-other-window: jump to the definition in other-window
* lsp-bridge-find-impl: jump to the implementation
* lsp-bridge-find-impl-other-window: jump to the implementation in other-window
* lsp-bridge-return-from-def: return to the location before calling `lsp-bridge-find-def`
* lsp-bridge-find-references: traverse across code references (forked from color-rg.el)
* lsp-bridge-lookup-documentation: lookup documentation of symbol under the cursor
* lsp-bridge-popup-documentation-scroll-up: scroll up popup document.
* lsp-bridge-popup-documentation-scroll-down: scroll down popup document.
* lsp-bridge-rename: rename symbol under the cursor
* lsp-bridge-jump-to-next-diagnostic: Jump to the next diagnostic position
* lsp-bridge-jump-to-prev-diagnostic: Jump to the previous diagnostic position
* lsp-bridge-show-signature-help-in-minibuffer: show signature help in minibuffer manually (move cursor to parameters area will show signature help automatically)
* lsp-bridge-insert-common-prefix: insert common prefix of candidates
* lsp-bridge-restart-process: restart lsp-bridge process (only used for development)

## Customize lsp-bridge keymap
customize keymap for lsp-bridge commands.

eg:
```elisp
(defvar lsp-bridge-mode-map
    (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-j") 'lsp-bridge-popup-documentation-scroll-up)
    (define-key keymap (kbd "C-k") 'lsp-bridge-popup-documentation-scroll-down)
    keymap))
```

## Customize language server configuration

The default configuration of for each language server is stored at [lsp-bridge/langserver](https://github.com/manateelazycat/lsp-bridge/tree/master/langserver).

Anyway you can customize server configuration with the following priority:
1. ```lsp-bridge-get-lang-server-by-project```: write your own function to get server configuration based on project-path and file-path, this function is nil by default
2. ```lsp-bridge-lang-server-extension-list```: load server configuration based on file extension, such as, we launch ```volar``` server instead ```javascript``` server when open *.vue file
3. ```lsp-bridge-lang-server-mode-list```: load server configuration based on major-mode

## Add support for new language?

1. Create configuration file under lsp-bridge/langserver, such as `pyright.json` for pyright (windows user please uses `pyright_windows.json`, macOS user please uses `pyright_darwin.json`).
2. Add `(mode . server_name)` in ```lsp-bridge-lang-server-list```, such as `(python-mode . "pyright")`
3. Add new mode-hook to `lsp-bridge-default-mode-hooks`

Welcome to send PR to help us improve support for LSP servers, thanks for your contribution!

## Supported language servers

| Index | LSP Server              | Language | Note                                                                                                                                                               |
| :--- | :--- | :--- | :--- |
| 1 | [clangd](https://github.com/clangd/clangd) | c, c++ |  |
| 2 | [pyright](https://github.com/microsoft/pyright) | python | `pip install pyright`|
| 3 | [solargraph](https://github.com/castwide/solargraph) | ruby | | 
| 4 | [rust-analyzer](https://github.com/rust-lang/rust-analyzer) | rust | |
| 5 | [elixirLS](https://github.com/elixir-lsp/elixir-ls) | elixir | please ensure that the `elixir-ls` release directory is in your system PATH at first |
| 6 | [gopls](https://github.com/golang/tools/tree/master/gopls) | go | make sure gopls in PATH, please do `ln -s ~/go/bin/gopls ~/.local/bin` |
| 7 | [hls](https://github.com/haskell/haskell-language-server) | haskell | |
| 8 | [dart-analysis-server](https://github.com/dart-lang/sdk/tree/master/pkg/analysis_server) | dart | |
| 9 | [metals](https://scalameta.org/metals/) | scala | |
| 10 | [typescript](https://www.npmjs.com/package/typescript) | typescript, javascript | |
| 11 | [ocamllsp](https://github.com/ocaml/ocaml-lsp) | ocaml | |
| 12 | [erlang-ls](https://github.com/erlang-ls/erlang_ls) | erlang | |
| 13 | [texlab](https://github.com/latex-lsp/texlab) | latex | |
| 14 | [eclipse.jdt.ls](https://projects.eclipse.org/projects/eclipse.jdt.ls) | java | please ensure that `org.eclipse.jdt.ls.product/target/repository/bin` is in your system PATH at first |
| 15 | [clojure-lsp](https://github.com/clojure-lsp/clojure-lsp) | clojure | |
| 16 | [bash-language-server](https://github.com/bash-lsp/bash-language-server) | bash | |
| 17 | [volar](https://github.com/johnsoncodehk/volar) | vue | |
| 18 | [sumneko](https://github.com/sumneko/lua-language-server) | lua | please ensure `bin` under sumneko installation is in your system PATH at first |
| 19 | [wxml-language-server](https://github.com/chemzqm/wxml-languageserver) | wxml | |
| 20 | [vscode-html-language-server](https://github.com/hrsh7th/vscode-langservers-extracted) | html | |
| 21 | [vscode-css-language-server](https://github.com/hrsh7th/vscode-langservers-extracted) | css | |
| 22 | [elm-language-server](https://github.com/elm-tooling/elm-language-server) | elm | |

### TODO:

- [ ] Show signature help with eldoc
- [ ] Code action
- [ ] Inline Value
- [ ] JavaSctipt different code blocks use different language servers
- [ ] Support completionItem/resolve to implement auto-import for volar
- [ ] Cache candidate document at Python side, only fetch document information when switch candidate.

### Features that won't be supported

The goal of lsp-bridge is to become the fastest LSP client in Emacs, not a complete implementation of LSP protocol.

Emacs can do better for the following tasks, we will not reinvent the wheel in lsp-bridge:
1. Code formatting: each LSP server has its own formatting specification, we can gain finer control using Emacs' builtin formatting tool.
2. Syntax highlighting: [Tree-sitter](https://tree-sitter.github.io/tree-sitter/) is a wonderful incremental parsing library for syntax highlighting.
2. Xref: Xref's mechanism is simultaneously. lsp-bridge is completely asynchronous, recommended to use wrap function to uniformly key

## Join development

The following is the framework of lsp-bridge:

<img src="./framework.png">

The following is the directory structure of the lsp-bridge project:

| File              | Explanation                                                                                                                                                               |
| :----------------------- | :-------------------------------------------------------------------------------------------------------------------------------------------------- |
| lsp-bridge.el           | Elisp main logic part that provides custom options and elisp functions for python sub-process calls like code jumping, renaming, etc.                          |
| lsp-bridge-epc.el       | Communicating with lsp-bridge python sub-process, which mainly implements elisp IPC to connect to python EPC for data serialization, sending, receiving, and deserialization |
| lsp-bridge-ref.el       | Framework of code referencing, providing references viewing, batch renames, regex filtering of reference results, etc. The core code is forked from color-rg.el                                    |
| lsp-bridge-orderless.el | Fuzzy search for completion items, i.e. for long candidate you do not need to type the word in correct order to get the correct item                 |
| lsp-bridge-icon.el      | Rendering the completion menu icons, which is used to distinguish different types of completion options                                                                           |
 | lsp-bridge-jdtls.el      | Provide java language third-party library jumping function                                                           |
| lsp-bridge.py           | Python main logic part that provides event loop, message scheduling and status management                                                                                     |
| core/fileaction.py      | Tracking the status of each file, processing LSP response messages, calling Emacs elisp function                                                                                           |
| core/lspserver.py       | LSP message processing module, mainly to analyze, send and receive LSP messages, and ensure that the sequence of LSP requests conforms with the LSP protocol specification                 |
| core/utils.py           | Utility functions of convenience for each module call                                                                                                                                 |
| core/mergedeep.py           | JSON information merger is mainly used to send custom options to LSP server                                                                             |
| core/hanlder/           | The implementation of sending and receiving LSP message, where __init__.py is a base class                                                                                             |
| langserver              | The configurations of the LSP servers, each server corresponding to a JSON file that defines the name of the server, language ID, starting command, options, etc.                        |



Please read [LSP Specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/) and [The Design of lsp-bridge](https://manateelazycat.github.io/emacs/2022/05/12/lsp-bridge.html) first.

Then turn on option ```lsp-bridge-enable-log``` and happy hacking! ;)

## Report bug

Please use `emacs -q` and load a minimal setup with only lsp-bridge to verify that the bug is reproducible. If `emacs -q` works fine, probably something is wrong with your Emacs config.

If the problem still exists, please report it [here](https://github.com/manateelazycat/lsp-bridge/issues/new) with `*lsp-bridge*` buffer content, it contains many clues that can help us locate the problem faster.

If you get a segfault error, please use the following way to collect crash information:

1. Install gdb and turn on option `lsp-bridge-enable-debug`
2. Use the command `lsp-bridge-stop-process` to stop the current process
3. Restart lsp-bridge, send issue with `*lsp-bridge*` buffer content when next crash

## Contributor

<a href = "https://github.com/manateelazycat/lsp-bridge/graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=manateelazycat/lsp-bridge"/>
</a>
