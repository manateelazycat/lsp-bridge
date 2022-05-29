[English](./README.md) | 简体中文

# lsp-bridge

lsp-bridge的目标是实现Emacs生态中性能最快的LSP客户端。

lsp-bridge使用Python多线程技术在Emacs和LSP服务器之间构建高速缓存，在提供行云流水的代码补全体验的前提下，保证永远不会卡住Emacs。

<img src="./screenshot.png">

## 安装

1. 安装Python依赖: [python-epc](https://github.com/tkf/python-epc)
2. 安装Elisp依赖:
+ [corfu](https://github.com/minad/corfu)
+ [orderless](https://github.com/oantolin/orderless)
+ [all-the-icons](https://github.com/domtronn/all-the-icons.el) (需要在安装all-the-icons后执行命令`all-the-icons-install-fonts`安装图标字体)
+ [posframe](https://github.com/tumashu/posframe)
+ [markdown-mode](https://github.com/jrblevin/markdown-mode)
+ [yasnippet](https://github.com/joaotavora/yasnippet)
3. 用 `git clone` 下载此仓库，并替换下面配置中的 load-path 路径
4. 把下面代码加入到你的配置文件 ~/.emacs 中：

```elisp
(add-to-list 'load-path "<path-to-lsp-bridge>")

(require 'yasnippet)
(require 'lsp-bridge)
(require 'lsp-bridge-icon)        ;; 显示图标在补全菜单中，可选
(require 'lsp-bridge-jdtls)       ;; 提供Java第三方库跳转和-data目录支持， Java用户必选
(yas-global-mode 1)

(require 'corfu)
(require 'corfu-info)
(require 'corfu-history)
(require 'lsp-bridge-orderless)   ;; 支持模糊搜索，可选
(corfu-history-mode t)
(global-lsp-bridge-mode)
(when (> (frame-pixel-width) 3000) (custom-set-faces '(corfu-default ((t (:height 1.3))))))  ;; 让corfu适应高分屏
```

## 命令列表

* lsp-bridge-find-def: 跳转到定义位置
* lsp-bridge-find-def-other-window: 在其他窗口跳转到定义位置
* lsp-bridge-find-impl: 跳转到接口实现位置
* lsp-bridge-find-impl-other-window: 在其他窗口跳转到接口实现位置
* lsp-bridge-return-from-def: 返回跳转之前的位置
* lsp-bridge-find-references: 查看代码引用
* lsp-bridge-lookup-documentation: 查看光标处的文档
* lsp-bridge-popup-documentation-scroll-up: 文档窗口向上滚动
* lsp-bridge-popup-documentation-scroll-down: 文档窗口向下滚动
* lsp-bridge-rename: 重命名
* lsp-bridge-jump-to-next-diagnostic: 跳转到下一个诊断位置
* lsp-bridge-jump-to-prev-diagnostic: 跳转到上一个诊断位置
* lsp-bridge-show-signature-help-in-minibuffer: 在minibuffer显示参数信息
* lsp-bridge-insert-common-prefix: 插入补全后选词的公共前缀
* lsp-bridge-restart-process: 重启lsp-bridge进程 (一般只有开发者才需要这个功能)

## 自定义lsp-bridge按键
可以为 lsp-bridge 命令列表自定义对应的按键

eg:
```elisp
(defvar lsp-bridge-mode-map
    (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-j") 'lsp-bridge-popup-documentation-scroll-up)
    (define-key keymap (kbd "C-k") 'lsp-bridge-popup-documentation-scroll-down)
    keymap))
```

## 自定义语言服务器配置
lsp-bridge每种语言的服务器配置存储在[lsp-bridge/langserver](https://github.com/manateelazycat/lsp-bridge/tree/master/langserver).

你可以根据以下优先级顺序来自定义服务器配置：
1. ```lsp-bridge-get-lang-server-by-project```: 编写自己的自定义函数，根据工程目录和文件路径来返回自定义配置的路径, 默认这个函数为nil
2. ```lsp-bridge-lang-server-extension-list```: 根据文件的扩展名来返回服务器，比如打开*.vue文件时，我们会使用 ```volar``` 服务器替代 javascript-mode 匹配的 ```javascript``` 服务器
3. ```lsp-bridge-lang-server-mode-list```: 根据Emacs的major-mode来返回对应的服务器

## 添加新的编程语言支持?

1. 在lsp-bridge/langserver目录下创建配置文件， 比如`pyright.json`就是pyright服务器的配置文件 (windows平台用`pyright_windows.json`, macOS平台用`pyright_darwin.json`).
2. 添加 `(mode . server_name)` 到选项 ```lsp-bridge-lang-server-list``` 中, 比如 `(python-mode . "pyright")`
3. 添加新的 mode-hook 到 `lsp-bridge-default-mode-hooks`

欢迎发送补丁帮助我们支持更多的LSP服务器，感谢你的帮助！

## 已经支持的语言服务器

| 序号 | LSP服务器              | 语言 | 备注                                                                                                                                                               |
| :--- | :--- | :--- | :--- |
| 1 | [clangd](https://github.com/clangd/clangd) | c, c++ |  |
| 2 | [pyright](https://github.com/microsoft/pyright) | python | `pip install pyright`|
| 3 | [solargraph](https://github.com/castwide/solargraph) | ruby | | 
| 4 | [rust-analyzer](https://github.com/rust-lang/rust-analyzer) | rust | |
| 5 | [elixirLS](https://github.com/elixir-lsp/elixir-ls) | elixir | 请确保导出 `elixir-ls` 目录到你系统的PATH路径 |
| 6 | [gopls](https://github.com/golang/tools/tree/master/gopls) | go | make sure gopls in PATH, please do `ln -s ~/go/bin/gopls ~/.local/bin` |
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


### 需要完成的功能：

- [ ] 用eldoc来显示参数信息
- [ ] Code Action: 代码动作， 比如自动修复代码
- [ ] Inline Value: 行类值显示
- [ ] JavaSctipt不同的代码块使用不同的语言服务器
- [ ] 支持completionItem/resolve消息以实现volar的自动导入功能
- [ ] 缓存后选词文档，只有用户切换后选词时才获取新的文档

### 不会支持的特性：
lsp-bridge的目标是实现Emacs生态中性能最快的LSP客户端, 但不是实现LSP协议最全的LSP客户端。

下面的功能用Emacs现有生态做更好：
1. 代码格式化: 每个LSP服务器都有自己的格式配置，使用Emacs内置的格式化工具，我们可以获得更细腻一致的格式化风格
2. 语法高亮: [Tree-sitter](https://tree-sitter.github.io/tree-sitter/) 是一个静态高性能的语法分析库，比LSP更适合完成语法高亮
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
| lsp-bridge-orderless.el | 提供模糊查找的功能，就是代码补全的时候不需要按照单词顺序敲就可以快速补全较长的后选项                         |
| lsp-bridge-icon.el      | 提供补全菜单Icon渲染，用于区分不同类型的补全后选项                                                           |
| lsp-bridge-jdtls.el      | 提供Java语言第三方库跳转功能                                                           |
| lsp-bridge.py           | lsp-bridge的Python主逻辑部分，提供事件循环、消息调度和状态管理                                               |
| core/fileaction.py      | 主要记录每个文件状态，处理LSP响应消息，调用Emacs Elisp函数                                                   |
| core/lspserver.py       | LSP消息处理模块，主要是解析、发送和接受LSP消息，并保证LSP请求顺序符合LSP协议规范                             |
| core/utils.py           | 一些全局工具函数，方便各模块调用                                                                             |
| core/mergedeep.py           | JSON信息合并， 主要用于发送自定义选项给LSP服务器                                                                             |
| core/hanlder/           | LSP消息发送和接受的实现，其中 __init__.py 是基类                                                             |
| langserver              | 主要放置LSP服务器的配置，每一个服务器一个 json 文件，分别定义服务器的名称、语言ID、启动命令和设置选项等      |


请先阅读[LSP协议规范](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/) 和 [lsp-bridge架构设计](https://manateelazycat.github.io/emacs/2022/05/12/lsp-bridge.html)。

接着打开选项 ```lsp-bridge-enable-log``` ， happy hacking! ;)

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
