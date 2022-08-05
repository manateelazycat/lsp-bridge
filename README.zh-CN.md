[English](./README.md) | 简体中文

# lsp-bridge

lsp-bridge的目标是实现Emacs生态中性能最快的LSP客户端。

lsp-bridge使用Python多线程技术在Emacs和LSP服务器之间构建高速缓存，在提供行云流水的代码补全体验的前提下，保证永远不会卡住Emacs。

<img src="./screenshot.png">

## 安装

1. 安装Python依赖: [python-epc](https://github.com/tkf/python-epc)
2. 安装[orjson](https://github.com/ijl/orjson) (可选， 依赖Rust)， JSON解析性能会提升150%~200%
3. 安装Elisp依赖:
+ [posframe](https://github.com/tumashu/posframe)
+ [markdown-mode](https://github.com/jrblevin/markdown-mode)
+ [yasnippet](https://github.com/joaotavora/yasnippet)
4. 用 `git clone` 下载此仓库，并替换下面配置中的 load-path 路径
5. 把下面代码加入到你的配置文件 ~/.emacs 中：

```elisp
(add-to-list 'load-path "<path-to-lsp-bridge>")

(require 'yasnippet)
(yas-global-mode 1)

(require 'lsp-bridge)
(global-lsp-bridge-mode)
```

## 使用
lsp-bridge开箱即用， 安装好语言对应的[LSP服务器](https://github.com/manateelazycat/lsp-bridge/blob/master/README.zh-CN.md#%E5%B7%B2%E7%BB%8F%E6%94%AF%E6%8C%81%E7%9A%84%E8%AF%AD%E8%A8%80%E6%9C%8D%E5%8A%A1%E5%99%A8)和模式插件以后， 直接写代码即可， 不需要额外的设置。

需要注意的是 lsp-bridge 有三种扫描模式：
1. 检测到 `.git` 目录时(通过命令 `git rev-parse --is-inside-work-tree` 来判断)， lsp-bridge会扫描整个目录文件来提供补全
2. 没有检测到 `.git` 目录时， lsp-bridge只会对打开的文件提供单文件补全
3. 自定义 `lsp-bridge-get-project-path-by-filepath` 函数， 输入参数是打开文件的路径字符串， 输出参数是项目目录路径， lsp-bridge会根据输出目录路径来提供补全

## 命令列表

* `lsp-bridge-find-def`: 跳转到定义位置
* `lsp-bridge-find-def-other-window`: 在其他窗口跳转到定义位置
* `lsp-bridge-find-impl`: 跳转到接口实现位置
* `lsp-bridge-find-impl-other-window`: 在其他窗口跳转到接口实现位置
* `lsp-bridge-return-from-def`: 返回跳转之前的位置
* `lsp-bridge-find-references`: 查看代码引用
* `lsp-bridge-lookup-documentation`: 查看光标处的文档
* `lsp-bridge-popup-documentation-scroll-up`: 文档窗口向上滚动
* `lsp-bridge-popup-documentation-scroll-down`: 文档窗口向下滚动
* `lsp-bridge-rename`: 重命名
* `lsp-bridge-jump-to-next-diagnostic`: 跳转到下一个诊断位置
* `lsp-bridge-jump-to-prev-diagnostic`: 跳转到上一个诊断位置
* `lsp-bridge-list-diagnostics`: 列出所有诊断信息
* `lsp-bridge-ignore-current-diagnostic`: 插入注视忽略当前诊断
* `lsp-bridge-signature-help-fetch`: 在minibuffer显示参数信息
* `lsp-bridge-insert-common-prefix`: 插入补全后选词的公共前缀
* `lsp-bridge-restart-process`: 重启lsp-bridge进程 (一般只有开发者才需要这个功能)
* `lsp-bridge-popup-complete`: 手动弹出补全菜单， 只有当打开 `lsp-bridge-complete-manually` 选项才需要使用这个命令
* `acm-doc-scroll-up`: API文档窗口向上滚动
* `acm-doc-scroll-down`: API文档窗口向下滚动

## 选项
* `lsp-bridge-completion-popup-predicates`: 补全菜单显示的检查函数， 这个选项包括的所有函数都检查过以后， 补全菜单才能显示
* `lsp-bridge-completion-stop-commands`: 这些命令执行以后，不再弹出补全菜单
* `lsp-bridge-completion-hide-characters`: 这些字符的后面不再弹出补全菜单
* `lsp-bridge-diagnostics-fetch-idle`： 诊断延迟，默认是停止敲键盘后1秒开始拉取诊断信息
* `lsp-bridge-enable-diagnostics`: 代码诊断， 默认打开
* `lsp-bridge-enable-signature-help`: 支持函数参数显示， 默认打开
* `lsp-bridge-enable-search-words`: 索引打开文件的单词， 默认打开
* `lsp-bridge-enable-auto-format-code`: 自动格式化代码, 默认关闭
* `lsp-bridge-org-babel-lang-list`: 支持org-mode代码块补全的语言列表
* `lsp-bridge-disable-backup`: 禁止emacs对文件做版本管理， 默认打开
* `lsp-bridge-enable-log`: 启用LSP消息日志， 默认关闭
* `lsp-bridge-enable-debug`: 启用程序调试， 默认关闭
* `lsp-bridge-python-command`: Python命令的路径, 如果你用 `conda`， 你也许会定制这个选项
* `lsp-bridge-signature-function`: 用于显示签名信息的函数
* `lsp-bridge-c-lsp-server`: C语言的服务器，可以选择`clangd`或者`ccls`
* `lsp-bridge-python-lsp-server`: Python语言的服务器，可以选择`pyright`或者`jedi`
* `lsp-bridge-tex-lsp-server`: LaTeX语言的服务器，可以选择`texlab`或者`digestif`
* `lsp-bridge-complete-manually`: 只有当用户手动调用 `lsp-bridge-popup-complete` 命令的时候才弹出补全菜单， 默认关闭
* `acm-backend-lsp-enable-auto-import`: 支持自动导入， 默认打开
* `acm-candidate-match-function`: 补全菜单匹配算法， orderless-* 开头的算法需要额外安装 [orderless](https://github.com/oantolin/orderless)
* `acm-enable-doc`: 补全菜单是否显示帮助文档
* `acm-enable-icon`: 补全菜单是否显示图标, macOS用户需要给 brew 命令增加选项 `--with-rsvg` 来安装Emacs才能显示SVG图片
* `acm-enable-quick-access`: 是否在图标后面显示索引， 可以通过 Alt + Number 来快速选择后选词， 默认关闭
* `acm-snippet-insert-index`: 代码模板后选词在补全菜单中的显示位置
* `acm-doc-frame-max-lines`: 帮助窗口的最大行数， 默认是20

## 自定义语言服务器配置
lsp-bridge每种语言的服务器配置存储在[lsp-bridge/langserver](https://github.com/manateelazycat/lsp-bridge/tree/master/langserver).


大多数情况， 你可以根据以下优先级顺序来自定义服务器配置：
1. ```lsp-bridge-get-single-lang-server-by-project```: 用户自定义函数， 输入参数是 `project-path` 和 `file-path`, 返回对应的LSP服务器字符串， 可以在 `lsp-bridge-single-lang-server-mode-list` 列表中查询所有LSP服务器的名称， 默认这个函数返回 nil
2. ```lsp-bridge-single-lang-server-extension-list```: 根据文件的扩展名来返回服务器，比如打开*.wxml文件时，我们会使用 ```wxml``` LSP服务器提供补全
3. ```lsp-bridge-single-lang-server-mode-list```: 根据Emacs的major-mode来返回对应的服务器

如果你在编写JavaScript代码， 你可能需要自定义多服务器配置：
1. ```lsp-bridge-get-multi-lang-server-by-project```: 用户自定义函数， 输入参数是 `project-path` 和 `file-path`, 返回多服务器配置名， 可以在子目录 [lsp-bridge/multiserver](https://github.com/manateelazycat/lsp-bridge/tree/master/multiserver) 中查找
2. ```lsp-bridge-multi-lang-server-extension-list```: 根据文件的扩展名来返回多服务器配置名， 比如打开*.vue文件时，我们会使用 ```volar_emmet``` 来同时利用 `volar` 和 `emmet-ls` 两种LSP服务器提供补全
3. ```lsp-bridge-multi-lang-server-mode-list```: 根据Emacs的major-mode来返回对应的多服务器配置名

## 添加新的编程语言支持?

1. 在 lsp-bridge/langserver 目录下创建配置文件， 比如`pyright.json`就是 pyright 服务器的配置文件 (windows 平台用`pyright_windows.json`, macOS 平台用`pyright_darwin.json`)。
2. 添加 `(mode . server_name)` 到 `lsp-bridge.el` 文件中的 `lsp-bridge-single-lang-server-mode-list` 选项中, 比如 `(python-mode . "pyright")`。
3. 添加新的 mode-hook 到 `lsp-bridge.el` 文件中的 `lsp-bridge-default-mode-hooks` 选项中。
4. 添加新的缩进变量到 `lsp-bridge.el` 文件中的 `lsp-bridge-formatting-indent-alist` 选项中。

欢迎发送补丁帮助我们支持更多的LSP服务器，感谢你的帮助！

## 已经支持的语言服务器

你需要安装每个编程语言对应的LSP服务器， lsp-bridge才能提供代码补全服务。

| 序号 | LSP服务器              | 语言 | 备注                                                                                                                                                               |
| :--- | :--- | :--- | :--- |
| 1 | [clangd](https://github.com/clangd/clangd) | c, c++, object-c |  |
| 2 | [pyright](https://github.com/microsoft/pyright) | python | `pip install pyright`|
| 3 | [solargraph](https://github.com/castwide/solargraph) | ruby | |
| 4 | [rust-analyzer](https://github.com/rust-lang/rust-analyzer) | rust | |
| 5 | [elixirLS](https://github.com/elixir-lsp/elixir-ls) | elixir | 请确保导出 `elixir-ls` 目录到你系统的PATH路径 |
| 6 | [gopls](https://github.com/golang/tools/tree/master/gopls) | go | 确保安装 [go-mode](https://github.com/dominikh/go-mode.el)， 同时确保 gopls 在 PATH 环境变量中, 执行命令 `ln -s ~/go/bin/gopls ~/.local/bin`, 还要在补全之前执行 `go mod init` 命令 |
| 7 | [hls](https://github.com/haskell/haskell-language-server) | haskell | |
| 8 | [dart-analysis-server](https://github.com/dart-lang/sdk/tree/master/pkg/analysis_server) | dart | |
| 9 | [metals](https://scalameta.org/metals/) | scala | |
| 10 | [typescript](https://www.npmjs.com/package/typescript) | typescript, javascript | |
| 11 | [ocamllsp](https://github.com/ocaml/ocaml-lsp) | ocaml | |
| 12 | [erlang-ls](https://github.com/erlang-ls/erlang_ls) | erlang | |
| 13 | [texlab](https://github.com/latex-lsp/texlab) | latex | |
| 14 | [eclipse.jdt.ls](https://projects.eclipse.org/projects/eclipse.jdt.ls) | java | 请确保导出 `org.eclipse.jdt.ls.product/target/repository/bin` 到你系统的PATH路径 |
| 15 | [clojure-lsp](https://github.com/clojure-lsp/clojure-lsp) | clojure | |
| 16 | [bash-language-server](https://github.com/bash-lsp/bash-language-server) | bash | |
| 17 | [volar](https://github.com/johnsoncodehk/volar) | vue | |
| 18 | [sumneko](https://github.com/sumneko/lua-language-server) | lua | 请确保导出sumneko的 `bin` 目录到你系统的PATH路径 |
| 19 | [wxml-language-server](https://github.com/chemzqm/wxml-languageserver) | wxml | |
| 20 | [vscode-html-language-server](https://github.com/hrsh7th/vscode-langservers-extracted) | html | |
| 21 | [vscode-css-language-server](https://github.com/hrsh7th/vscode-langservers-extracted) | css | |
| 22 | [elm-language-server](https://github.com/elm-tooling/elm-language-server) | elm | |
| 23 | [intelephense](https://github.com/bmewburn/vscode-intelephense) | php | |
| 24 | [yaml-language-server](https://github.com/redhat-developer/yaml-language-server) | yaml | `npm install -g yaml-language-server` |
| 25 | [zls](https://github.com/zigtools/zls) | zig | 运行 `zls config` 来生成 zls 的配置。参考 [Configuration Options](https://github.com/zigtools/zls#configuration-options) |
| 26 | [groovy-language-server](https://github.com/GroovyLanguageServer/groovy-language-server) | groovy | 在 PATH 中创建一个名为 "groovy-language-server" 的脚本, 内容为 `$JAVA_HOME/bin/java -jar <path>/groovy-language-server-all.jar` |
| 27 | [docker-language-server](https://github.com/rcjsuen/dockerfile-language-server-nodejs) | Dockerfiles | |
| 28 | [serve-d](https://github.com/Pure-D/serve-d) | d | serve-d 不支持单文件模式, 使用前请先在项目目录下初始 git 仓库或者自定义 `lsp-bridge-get-project-path-by-filepath` 返回项目目录 |
| 29 | [fortls](https://github.com/gnikit/fortls) | Fortran | |
| 30 | [ccls](https://github.com/MaskRay/ccls) | c, c++, object-c | `lsp-bridge-c-lsp-server` 设置成 `ccls` |
| 31 | [jedi](https://github.com/pappasam/jedi-language-server) | python | `lsp-bridge-python-lsp-server` 设置成 `jedi` |
| 32 | [emmet-ls](https://github.com/aca/emmet-ls) | html, js, css, sass, scss, less | |
| 33 | [rnix-lsp](https://github.com/nix-community/rnix-lsp) | nix | |
| 34 | [digestif](https://github.com/astoff/digestif) | latex | `lsp-bridge-tex-lsp-server` 设置成 `digestif` |
| 35 | [rlanguageserver](https://github.com/REditorSupport/languageserver)       | R   | |

### 不会支持的特性：
lsp-bridge的目标是实现Emacs生态中性能最快的LSP客户端, 但不是实现LSP协议最全的LSP客户端。

下面的功能用Emacs现有生态做更好：
1. 语法高亮: [Tree-sitter](https://tree-sitter.github.io/tree-sitter/) 是一个静态高性能的语法分析库，比LSP更适合完成语法高亮
2. Xref: Xref的机制是同步等待， lsp-bridge是完全异步的， 两个机制无法融合， 建议自己编写包装函数来统一按键

## 加入开发

下图是lsp-bridge的架构设计:

<img src="./framework.png">

下面是 lsp-bridge 项目的目录结构：

| 文件名                  | 作用                                                                                                         |
| :--------------------- | :-------------------                                                                                        |
| lsp-bridge.el           | lsp-bridge的Elisp主逻辑部分，提供自定义选项和Elisp函数供python子进程调用，比如代码跳转、重命名等             |
| lsp-bridge-epc.el       | 和lsp-bridge python子进程通讯的代码，主要实现Elisp IPC来对接Python EPC, 实现数据序列化、发送、接收和反序列化 |
| lsp-bridge-ref.el       | 代码引用查看框架，提供引用查看、批量重命名、引用结果正则过滤等，核心代码 fork 自color-rg.el                  |
| lsp-bridge-jdtls.el      | 提供Java语言第三方库跳转功能                                                           |
| lsp-bridge.py           | lsp-bridge的Python主逻辑部分，提供事件循环、消息调度和状态管理                                               |
| acm/acm.el      | 异步补全菜单， 专门为 lsp-bridge 后端而设计， 支持lsp, elisp, words等后端                                                                                           |
| core/fileaction.py      | 主要记录每个文件状态，处理LSP响应消息，调用Emacs Elisp函数                                                   |
| core/lspserver.py       | LSP消息处理模块，主要是解析、发送和接受LSP消息，并保证LSP请求顺序符合LSP协议规范                             |
| core/utils.py           | 一些全局工具函数，方便各模块调用                                                                             |
| core/mergedeep.py           | JSON信息合并， 主要用于发送自定义选项给LSP服务器                                                                             |
| core/hanlder/           | LSP消息发送和接受的实现，其中 __init__.py 是基类                                                             |
| langserver              | 主要放置LSP服务器的配置，每一个服务器一个 json 文件，分别定义服务器的名称、语言ID、启动命令和设置选项等      |


请先阅读下面的文章:
* [LSP协议规范](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)
* [lsp-bridge架构设计](https://manateelazycat.github.io/emacs/2022/05/12/lsp-bridge.html)
* [为什么lsp-bridge不用capf](https://manateelazycat.github.io/emacs/2022/06/26/why-lsp-bridge-not-use-capf.html)

接着打开开发选项 ```lsp-bridge-enable-log``` ， happy hacking! ;)

## 反馈问题

请用命令 `emacs -q` 并只添加lsp-bridge配置做一个对比测试，如果 `emacs -q` 可以正常工作，请检查你个人的配置文件。

如果`emacs -q`环境下问题依旧，请到[这里](https://github.com/manateelazycat/lsp-bridge/issues/new)反馈, 并附带 `*lsp-bridge*` 窗口的内容给我们提交issue，那里面有很多线索可以帮助我们排查问题。。

如果你遇到崩溃的问题, 请用下面的方式来收集崩溃信息:
1. 先安装gdb并打开选项 `(setq lsp-bridge-enable-debug t)`
2. 使用命令 `lsp-bridge-stop-process` 停止LSP-BRIDGE进程
3. 重新打开lsp-bridge, 并在下次崩溃时发送 `*lsp-bridge*` 的内容

## 贡献者

<a href = "https://github.com/manateelazycat/lsp-bridge/graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=manateelazycat/lsp-bridge"/>
</a>
