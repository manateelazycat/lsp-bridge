# lsp-bridge
lsp-bridge's goal is to become the fastest LSP client in the Emacs.

Its design concept is:
1. High-Performance LSP client: use python threading technology build cache bridge between Emacs and LSP server, you will never feel stuck when you write the code
2. Integration Emacs ecological: focus on code-completion, find-define, find-references and rename-refacotry, focus on keyboard operation, not copy user experience of VSCode
3. Provide modern UI: we use Qt technology build completion UI, to provide fantastic icon, both-end text alignment, web document preview, etc.

## Installation
1. Install PyQt6: ```pip install PyQt6 PyQt6-Qt6 PyQt6-sip PyQt6-WebEngine PyQt6-WebEngine-Qt6```
2. Install [python-epc](https://github.com/tkf/python-epc): ```pip install epc```
3. Clone or download this repository (path of the folder is the `<path-to-lsp-bridge>` used below).
4. Add follow code in your ~/.emacs: ```(add-to-list 'load-path "<path-to-lsp-bridge>")```

### Proxy
If you need to use a proxy to access the internet, one can configure the proxy settings.

```Elisp
(setq lsp-bridge-proxy-type "http")
(setq lsp-bridge-proxy-host "127.0.0.1")
(setq lsp-bridge-proxy-port "1080")
```

If you use Socks5 as a local proxy, one can set proxy type with:

```Elisp
(setq lsp-bridge-proxy-type "socks5")
```

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
