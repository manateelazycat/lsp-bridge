English | [简体中文](./README.zh-CN.md)

# lsp-bridge

Lsp-bridge's goal is to become the fastest LSP client in Emacs.

Lsp-bridge uses python's threading technology to build caches that bridge Emacs and LSP server. Lsp-bridge will provide smooth completion experience without compromise to slow down emacs' performance.

<img src="./screenshot.png">

## Installation

1. Install Python dependencies: [python-epc](https://github.com/tkf/python-epc)
2. Install Elisp dependencies:
+ [posframe](https://github.com/tumashu/posframe)
+ [markdown-mode](https://github.com/jrblevin/markdown-mode)
+ [yasnippet](https://github.com/joaotavora/yasnippet)
3. Clone or download this repository (path of the folder is the `<path-to-lsp-bridge>` used below).
4. Add following code in your ~/.emacs:

```elisp
(add-to-list 'load-path "<path-to-lsp-bridge>")

(require 'yasnippet)
(require 'lsp-bridge)
(require 'lsp-bridge-jdtls)       ;; provide Java third-party library jump and -data directory support, optional

(yas-global-mode 1)
(global-lsp-bridge-mode)
```

## Usage
lsp-bridge is design for out the box. After installing the LSP server command corresponding to the open file, you can write the code directly without additional settings.

There are two modes in lsp-bridge:
1. When detecting the .git directory (to judge by command `git rev-parse-is-inside-work-tree`), lsp-bridge scan the entire directory files to provide completion
2. When the .git directory was not detected, lsp-bridge only provided a single file complementary to the open file

If you expect lsp-bridge to automatically scan the files of the entire project, please execute the `git init` command in the project root directory.

## Commands

* `lsp-bridge-find-def`: jump to the definition
* `lsp-bridge-find-def-other-window`: jump to the definition in other-window
* `lsp-bridge-find-impl`: jump to the implementation
* `lsp-bridge-find-impl-other-window`: jump to the implementation in other-window
* `lsp-bridge-return-from-def`: return to the location before calling `lsp-bridge-find-def`
* `lsp-bridge-find-references`: traverse across code references (forked from color-rg.el)
* `lsp-bridge-lookup-documentation`: lookup documentation of symbol under the cursor
* `lsp-bridge-popup-documentation-scroll-up`: scroll up popup document.
* `lsp-bridge-popup-documentation-scroll-down`: scroll down popup document.
* `lsp-bridge-rename`: rename symbol under the cursor
* `lsp-bridge-jump-to-next-diagnostic`: Jump to the next diagnostic position
* `lsp-bridge-jump-to-prev-diagnostic`: Jump to the previous diagnostic position
* `lsp-bridge-list-diagnostics`: List all diagnostic information
* `lsp-bridge-ignore-current-diagnostic`: Insert comment to ignore the current diagnosis
* `lsp-bridge-signature-help-fetch`: show signature help in minibuffer manually (move cursor to parameters area will show signature help automatically)
* `lsp-bridge-insert-common-prefix`: insert common prefix of candidates
* `lsp-bridge-restart-process`: restart lsp-bridge process (only used for development)

## Options
* `lsp-bridge-completion-popup-predicates`: the predicate function for completion menu, completion menu popup after all the functions pass
* `lsp-bridge-completion-stop-commands`: completion menu will not popup if these commands are executed
* `lsp-bridge-completion-hide-characters`: completion menu will not popup when cursor after those characters
* `lsp-bridge-diagnostics-fetch-idle`: diagnostic delay, start pulling diagnostic information 1 second after stopping typing
* `lsp-bridge-enable-diagnostics`: code diagnostic, enable by default
* `lsp-bridge-enable-candidate-doc-preview`: preview of the candidate document, enable by default
* `lsp-bridge-enable-signature-help`: show function parameter in minibufer, disable by default
* `lsp-bridge-org-babel-lang-list`: list of language to support org-mode code block completion
* `lsp-bridge-disable-backup`: forbidden version manage of emacs, enable by default
* `lsp-bridge-enable-log`: enable the LSP message log, disable by default
* `lsp-bridge-enable-debug`: enable program debugging, disable by default
* `lsp-bridge-python-command`: The path of the python command, if you use `conda`, you may customize this option
* `acm-backend-lsp-enable-auto-import`: automatic insert import code, enable by default
* `acm-candidate-match-function`: The complete menu matching algorithm, the algorithm prefix of orderless-* needs to be installed additional [orderless](https://github.com/oantolin/orderless)
* `acm-enable-doc`: Whether the complete menu display the help document
* `acm-enable-icon`: Whether the complete menu shows the icon
* `acm-fetch-candidate-doc-delay`: The complete menu pops up the delay of the document. It is not recommended to set it to 0, which will reduce the menu selection performance
* `acm-snippet-insert-index`: The display position of snippet candidate in the complementary menu

## Customize language server configuration

The default configuration of for each language server is stored at [lsp-bridge/langserver](https://github.com/manateelazycat/lsp-bridge/tree/master/langserver).

Anyway you can customize server configuration with the following priority:
1. ```lsp-bridge-get-lang-server-by-project```: write your own function to get server configuration based on project-path and file-path, this function is nil by default
2. ```lsp-bridge-lang-server-extension-list```: load server configuration based on file extension, such as, we launch ```volar``` server instead ```javascript``` server when open *.vue file
3. ```lsp-bridge-lang-server-mode-list```: load server configuration based on major-mode

## Add support for new language?

1. Create configuration file under lsp-bridge/langserver, such as `pyright.json` for pyright (windows user please uses `pyright_windows.json`, macOS user please uses `pyright_darwin.json`).
2. Add `(mode . server_name)` to `lsp-bridge-lang-server-mode-list` in `lsp-bridge.el`, such as `(python-mode . "pyright")`.
3. Add new mode-hook to `lsp-bridge-default-mode-hooks` in `lsp-bridge.el`.

Welcome to send PR to help us improve support for LSP servers, thanks for your contribution!

## Supported language servers

| Index | LSP Server              | Language | Note                                                                                                                                                               |
| :--- | :--- | :--- | :--- |
| 1 | [clangd](https://github.com/clangd/clangd) | c, c++ |  |
| 2 | [pyright](https://github.com/microsoft/pyright) | python | `pip install pyright`|
| 3 | [solargraph](https://github.com/castwide/solargraph) | ruby | |
| 4 | [rust-analyzer](https://github.com/rust-lang/rust-analyzer) | rust | |
| 5 | [elixirLS](https://github.com/elixir-lsp/elixir-ls) | elixir | please ensure that the `elixir-ls` release directory is in your system PATH at first |
| 6 | [gopls](https://github.com/golang/tools/tree/master/gopls) | go | make sure gopls in PATH, please do `ln -s ~/go/bin/gopls ~/.local/bin`, and do `go mod init` first |
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
| 23 | [intelephense](https://github.com/bmewburn/vscode-intelephense) | php | |
| 24 | [yaml-language-server](https://github.com/redhat-developer/yaml-language-server) | yaml | `npm install -g yaml-language-server` |

### TODO:

- [ ] Code action

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
 | lsp-bridge-jdtls.el      | Provide java language third-party library jumping function                                                           |
| lsp-bridge.py           | Python main logic part that provides event loop, message scheduling and status management                                                                                     |
| acm/acm.el      | Asynchronous completion menu, specially designed for lsp-bridge backend, supports LSP, elisp, dabbrev and other backend                                                                                           |
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
