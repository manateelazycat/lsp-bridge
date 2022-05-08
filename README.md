# lsp-bridge
lsp-bridge's goal is to become the fastest LSP client in the Emacs.

lsp-bridge use python threading technology build cache bridge between Emacs and LSP server, you will never feel stuck when you write the code.

<img src="./screenshot.png">

## Installation
1. Install [python-epc](https://github.com/tkf/python-epc): ```pip install epc```
2. Install [company-mode](https://github.com/company-mode/company-mode)
3. Clone or download this repository (path of the folder is the `<path-to-lsp-bridge>` used below).
4. Add follow code in your ~/.emacs: 

```
(add-to-list 'load-path "<path-to-lsp-bridge>")

(require 'company)
(require 'lsp-bridge)

(setq company-minimum-prefix-length 1)
(setq company-require-match nil)
(setq company-idle-delay 0.2)
(setq company-backends '((company-lsp-bridge)))

(global-company-mode)

;; For python and pyright
(dolist (hook (list
               'python-mode-hook
               'ruby-mode-hook
               ))
  (add-hook hook (lambda ()
                   (lsp-bridge-enable)
                   )))

```

## Customize language server configuration
lsp-bridge load lang server configuration from directory lsp-bridge/langserver. 

But default configuration maybe not works with your environment, you can change ```lsp-bridge-lang-server-list``` to customize language server configuration.

Example, we can change ```(python-mode . "pyright")``` to ```(python-mode . "/my_directory/pyright.json")``` then lsp-bridge will load configuration from ```/my_directory/pyright.json``` instead load from ```lsp-bridge/langserver/pyright.json```.

## Add support for new language?
1. Create settings file under lsp-bridge/langserver, such as ```pyright.json``` is use for pyright.
2. Add ```(mode . server_name)``` in lsp-bridge-lang-server-list, such as ```(python-mode . "pyright")```
3. Then add ```(lsp-bridge-enable)``` in mode-hook for test.

Welcome send PR to help us improve support for LSP servers, thank you!

## Todo
- [ ] Find references UI: use [color-rg](https://github.com/manateelazycat/color-rg) or xref
- [ ] Use capf implement completion-at-point: to support other completion UI, such as [Corfu](https://github.com/minad/corfu)
- [ ] Popup web document window by [Popweb](https://github.com/manateelazycat/popweb)
- [ ] To support other LSP server, only support pyright, solargraph now

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
