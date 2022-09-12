English | [简体中文](./README.zh-CN.md)

# lsp-bridge

Lsp-bridge's goal is to become the fastest LSP client in Emacs.

Lsp-bridge uses python's threading technology to build caches that bridge Emacs and LSP server. Lsp-bridge will provide smooth completion experience without compromise to slow down emacs' performance.

<img src="./screenshot.png">

## Installation

1. Install Emacs 28 and above versions
2. Install Python dependencies:
+ [python-epc](https://github.com/tkf/python-epc)
+ [orjson](https://github.com/ijl/orjson)
3. Install Elisp dependencies:
+ [posframe](https://github.com/tumashu/posframe)
+ [markdown-mode](https://github.com/jrblevin/markdown-mode)
+ [yasnippet](https://github.com/joaotavora/yasnippet)
4. Clone or download this repository (path of the folder is the `<path-to-lsp-bridge>` used below).
5. Add following code in your ~/.emacs:

```elisp
(add-to-list 'load-path "<path-to-lsp-bridge>")

(require 'yasnippet)
(yas-global-mode 1)

(require 'lsp-bridge)
(global-lsp-bridge-mode)
```

## Usage
lsp-bridge is design for out the box. After installing the [LSP server](https://github.com/manateelazycat/lsp-bridge#supported-language-servers) and mode plugin corresponding to the open file, you can write the code directly without additional settings.

It should be noted that there are three scan modes of lsp-bridge:
1. When detecting the `.git` directory (check by command `git rev-parse-is-inside-work-tree`), lsp-bridge scan the entire directory file to provide completion
2. When the `.git` directory was not detected, lsp-bridge only scan opened file to provide completion
3. Custom `lsp-bridge-get-project-path-by-filepath` function, the input parameter is the path string of opened file, the output parameter is the project directory path, lsp-bridge will scan project directory path to provide provide completion

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
* `lsp-bridge-popup-complete`: Manually popup the completion menu, you only need this command when turn on option `lsp-bride-complete-manually`
* `acm-doc-scroll-up`: API document window scroll up
* `acm-doc-scroll-down`: API document window scroll down

## Options
* `lsp-bridge-completion-popup-predicates`: the predicate function for completion menu, completion menu popup after all the functions pass
* `lsp-bridge-completion-stop-commands`: completion menu will not popup if these commands are executed
* `lsp-bridge-completion-hide-characters`: completion menu will not popup when cursor after those characters
* `lsp-bridge-diagnostics-fetch-idle`: diagnostic delay, start pulling diagnostic information 1 second after stopping typing
* `lsp-bridge-enable-diagnostics`: code diagnostic, enable by default
* `lsp-bridge-enable-signature-help`: show function parameter in minibufer, enable by default
* `lsp-bridge-enable-search-words`: index the word of the file, enable by default
* `lsp-bridge-enable-auto-format-code`: automatic format code, disable by default
* `lsp-bridge-org-babel-lang-list`: list of language to support org-mode code block completion
* `lsp-bridge-disable-backup`: forbidden version manage of emacs, enable by default
* `lsp-bridge-enable-log`: enable the LSP message log, disable by default
* `lsp-bridge-enable-debug`: enable program debugging, disable by default
* `lsp-bridge-python-command`: The path of the python command, if you use `conda`, you may customize this option
* `lsp-bridge-signature-function`: The function used for displaying signature info
* `lsp-bridge-c-lsp-server`: C language server, you can choose `clangd` or` ccls`
* `lsp-bridge-python-lsp-server`: Python language server, you can choose `pyright` or` jedi`
* `lsp-bridge-tex-lsp-server`: LaTeX language server, you can choose `taxlab` or` digestif`
* `lsp-bridge-complete-manually`: Only popup completion menu when user call `lsp-bridge-popup-complete` command, default is nil
* `lsp-bridge-get-workspace-folder`: You need to put multiple project in a `workspace` directory in Java before you can jump function defintion normally. This function can be customized, the function input is the project path and returns the `workspace` directory corresponding
* `acm-backend-lsp-enable-auto-import`: automatic insert import code, enable by default
* `acm-candidate-match-function`: The complete menu matching algorithm, the algorithm prefix of orderless-* needs to be installed additional [orderless](https://github.com/oantolin/orderless)
* `acm-enable-search-words`: Whether the complete menu display the word of the file, enable by default
* `acm-enable-doc`: Whether the complete menu display the help document
* `acm-enable-icon`: Whether the complete menu shows the icon, macOS users need to add option `--with-rsvg` to the brew command to install emacs to display SVG icon
* `acm-enable-quick-access`: Whether the index is displayed behind the icon, you can quickly select the candidate through Alt + Number, disable by default
* `acm-snippet-insert-index`: The display position of snippet candidate in the complementary menu
* `acm-doc-frame-max-lines`: Max line number of help documentation, default is 20
* `acm-enable-tabnine-helper`: Enable tabnine support，disable by default，when enable call `tabnine-bridge-install-binary` command, and it can be used. TabNine will consume huge CPUs, causing your entire computer to be slow. If the computer performance is not good, it is not recommended to enable this option
* `acm-enable-yas`: yasnippet completion, enable by default
* `acm-backend-yas-candidates-number`: yasnippet display number，2 by default
* `acm-enable-citre`: citre completion, disable by default
* `acm-backend-citre-keyword-complete`: Completion is performed according to the keywords of each mode defined by `acm-backend-citre-keywords-alist`, which takes effect only after citre is enabled.

## Customize language server configuration

The default configuration of for each language server is stored at [lsp-bridge/langserver](https://github.com/manateelazycat/lsp-bridge/tree/master/langserver).

In most cases, you can customize server configuration with the following priority:
1. ```lsp-bridge-get-single-lang-server-by-project```: The user custom function, the input parameter is `project-path` and `file-path`, return the corresponding LSP server string, you can query all LSP servers in the list of `lsp-bridge-single-lang-server-mode-list` option, this function return nil default
2. ```lsp-bridge-single-lang-server-extension-list```: load server configuration based on file extension, such as, we launch ```wxml``` server when open *.wxml file
3. ```lsp-bridge-single-lang-server-mode-list```: load server configuration based on major-mode

If you are writing JavaScript code, you may need to customize multi-server configuration:
1. ```lsp-bridge-get-multi-lang-server-by-project```: The user custom function, the input parameter is `project-path` and `file-path`, return the multi-server configuration name, you can find configuration name in the subdirectory
[lsp-bridge/multiserver](https://github.com/manateelazycat/lsp-bridge/tree/master/multiserver)
2. ```lsp-bridge-multi-lang-server-extension-list```: Return multi-server configuration name according to the expansion of the file, for example, when opening the *.vue file, we will use the `volar` and `emmet-ls` to provide completion service
3. ```lsp-bridge-multi-lang-server-mode-list```: Return the corresponding multi-server configuration name according to Emacs's major-mode

## Add support for new language?

1. Create configuration file under lsp-bridge/langserver, such as `pyright.json` for pyright (windows user please uses `pyright_windows.json`, macOS user please uses `pyright_darwin.json`).
2. Add `(mode . server_name)` to `lsp-bridge-single-lang-server-mode-list` in `lsp-bridge.el`, such as `(python-mode . "pyright")`.
3. Add new mode-hook to `lsp-bridge-default-mode-hooks` in `lsp-bridge.el`.
4. Add new mode indent to `lsp-bridge-formatting-indent-alist` in `lsp-bridge.el`.

Welcome to send PR to help us improve support for LSP servers, thanks for your contribution!

## Supported language servers

You need to install the LSP server corresponding to each programming language, then lsp-bridge can provide code completion service.

| Index | LSP Server              | Language | Note                                                                                                                                                               |
| :--- | :--- | :--- | :--- |
| 1 | [clangd](https://github.com/clangd/clangd) | c, c++, object-c |  |
| 2 | [pyright](https://github.com/microsoft/pyright) | python | `pip install pyright`|
| 3 | [solargraph](https://github.com/castwide/solargraph) | ruby | |
| 4 | [rust-analyzer](https://github.com/rust-lang/rust-analyzer) | rust | |
| 5 | [elixirLS](https://github.com/elixir-lsp/elixir-ls) | elixir | please ensure that the `elixir-ls` release directory is in your system PATH at first |
| 6 | [gopls](https://github.com/golang/tools/tree/master/gopls) | go | make sure install [go-mode](https://github.com/dominikh/go-mode.el) and gopls in PATH, please do `ln -s ~/go/bin/gopls ~/.local/bin`, and do `go mod init` first |
| 7 | [hls](https://github.com/haskell/haskell-language-server) | haskell | |
| 8 | [dart-analysis-server](https://github.com/dart-lang/sdk/tree/master/pkg/analysis_server) | dart | |
| 9 | [metals](https://scalameta.org/metals/) | scala | |
| 10 | [typescript](https://github.com/typescript-language-server/typescript-language-server) | typescript, javascript | |
| 11 | [ocamllsp](https://github.com/ocaml/ocaml-lsp) | ocaml | |
| 12 | [erlang-ls](https://github.com/erlang-ls/erlang_ls) | erlang | |
| 13 | [texlab](https://github.com/latex-lsp/texlab) | latex | |
| 14 | [eclipse.jdt.ls](https://projects.eclipse.org/projects/eclipse.jdt.ls) | java | please ensure that `org.eclipse.jdt.ls.product/target/repository/bin` is in your system PATH at first |
| 15 | [clojure-lsp](https://github.com/clojure-lsp/clojure-lsp) | clojure | if you use `homebrew` , please ensure install `clojure-lsp/brew/clojure-lsp-native` [clojure-lsp-native](https://clojure-lsp.io/installation/#homebrew-macos-and-linux) |
| 16 | [bash-language-server](https://github.com/bash-lsp/bash-language-server) | bash | |
| 17 | [volar](https://github.com/johnsoncodehk/volar) | vue | npm install typescript volar -g |
| 18 | [sumneko](https://github.com/sumneko/lua-language-server) | lua | please ensure `bin` under sumneko installation is in your system PATH at first |
| 19 | [wxml-language-server](https://github.com/chemzqm/wxml-languageserver) | wxml | |
| 20 | [vscode-html-language-server](https://github.com/hrsh7th/vscode-langservers-extracted) | html | |
| 21 | [vscode-css-language-server](https://github.com/hrsh7th/vscode-langservers-extracted) | css | |
| 22 | [elm-language-server](https://github.com/elm-tooling/elm-language-server) | elm | |
| 23 | [intelephense](https://github.com/bmewburn/vscode-intelephense) | php | |
| 24 | [yaml-language-server](https://github.com/redhat-developer/yaml-language-server) | yaml | `npm install -g yaml-language-server` |
| 25 | [zls](https://github.com/zigtools/zls) | zig | execute `zls config` to generate configuration for zls. see [Configuration Options](https://github.com/zigtools/zls#configuration-options) |
| 26 | [groovy-language-server](https://github.com/GroovyLanguageServer/groovy-language-server) | groovy | Create a script "groovy-language-server" in PATH, with `$JAVA_HOME/bin/java -jar <path>/groovy-language-server-all.jar` |
| 27 | [docker-language-server](https://github.com/rcjsuen/dockerfile-language-server-nodejs) | Dockerfiles | |
| 28 | [serve-d](https://github.com/Pure-D/serve-d) | d | serve-d does not support single file mode, please init .git repository under project root at first or custom `lsp-bridge-get-project-path-by-filepath` function |
| 29 | [fortls](https://github.com/gnikit/fortls) | Fortran | |
| 30 | [ccls](https://github.com/MaskRay/ccls) | c, c++, object-c | `lsp-bridge-c-lsp-server` set to` ccls` |
| 31 | [jedi](https://github.com/pappasam/jedi-language-server) | python | `lsp-bridge-python-lsp-server` set to `jedi` |
| 32 | [emmet-ls](https://github.com/aca/emmet-ls) | html, js, css, sass, scss, less | |
| 33 | [rnix-lsp](https://github.com/nix-community/rnix-lsp) | nix | |
| 34 | [digestif](https://github.com/astoff/digestif) | latex | `lsp-bridge-tex-lsp-server` set to `digestif` |
| 35 | [rlanguageserver](https://github.com/REditorSupport/languageserver)       | R   | |
| 36 | [graphql-lsp](https://github.com/graphql/graphiql/tree/main/packages/graphql-language-service-cli) | GraphQL | |
| 37 | [microsoft-python-language-server](https://github.com/microsoft/python-language-server) | Python | legacy language server for Python2 |
| 38 | [cmake-language-server](https://github.com/regen100/cmake-language-server) | cmake | `pip install cmake-language-server` |
| 39 | [Wen](https://github.com/metaescape/Wen) | org-mode | `pip install pygls pypinyin` |

### Features that won't be supported

The goal of lsp-bridge is to become the fastest LSP client in Emacs, not a complete implementation of LSP protocol.

Emacs can do better for the following tasks, we will not reinvent the wheel in lsp-bridge:
1. Syntax highlighting: [Tree-sitter](https://tree-sitter.github.io/tree-sitter/) is a wonderful incremental parsing library for syntax highlighting.
2. Xref: Xref's mechanism is synchronous, but lsp-bridge is completely asynchronous. I recommended binding your xref key to a wrapper function that combines xref and lsp-bridge together.

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
| acm/acm.el      | Asynchronous completion menu, specially designed for lsp-bridge backend, supports LSP, elisp, words, TabNine and other backend                                                                                           |
| core/fileaction.py      | Tracking the status of each file, processing LSP response messages, calling Emacs elisp function                                                                                           |
| core/lspserver.py       | LSP message processing module, mainly to analyze, send and receive LSP messages, and ensure that the sequence of LSP requests conforms with the LSP protocol specification                 |
| core/utils.py           | Utility functions of convenience for each module call                                                                                                                                 |
| core/mergedeep.py           | JSON information merger is mainly used to send custom options to LSP server                                                                             |
| core/hanlder/           | The implementation of sending and receiving LSP message, where __init__.py is a base class                                                                                             |
| langserver              | The configurations of the LSP servers, each server corresponding to a JSON file that defines the name of the server, language ID, starting command, options, etc.                        |

Please read below articles first:
* [LSP Specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)
* [The Design of lsp-bridge](https://manateelazycat.github.io/emacs/2022/05/12/lsp-bridge.html)
* [Why lsp-bridge not use capf](https://manateelazycat.github.io/emacs/2022/06/26/why-lsp-bridge-not-use-capf.html)

Then turn on develop option ```lsp-bridge-enable-log``` and happy hacking! ;)

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
