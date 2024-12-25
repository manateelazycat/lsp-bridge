;;; acm-backend-codeium.el --- acm codeium support -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(require 'array)

(defgroup acm-backend-codeium nil
  "ACM codeium support."
  :group 'acm)

(defcustom acm-enable-codeium nil
  "Enable codeium support."
  :type 'boolean
  :group 'acm-backend-codeium)

(defcustom acm-backend-codeium-candidate-min-length 0
  "Minimal length of candidate."
  :type 'integer
  :group 'acm-backend-codeium)

(defcustom acm-backend-codeium-candidate-max-length 50
  "Maximal length of candidate."
  :type 'integer
  :group 'acm-backend-lsp)

(defcustom acm-backend-codeium-candidates-number 10
  "Maximal number of codeium candidate of menu."
  :type 'integer
  :group 'acm-backend-codeium)

(defcustom acm-backend-codeium-api-server-host "server.codeium.com"
  "Codeium api server host."
  :type 'string
  :group 'acm-backend-codeium)

(defcustom acm-backend-codeium-api-server-port 443
  "Codeium api server port."
  :type 'integer
  :group 'acm-backend-codeium)

(defcustom acm-backend-codeium-api-key-path (expand-file-name (concat user-emacs-directory (file-name-as-directory "lsp-bridge") "codeium_api_key.txt"))
  "The path to store Codeium API Key."
  :type 'string
  :group 'acm-backend-codeium)

(defcustom acm-backend-codeium-accept nil
  "Send accept request."
  :type 'boolean
  :group 'acm-backend-codeium)

(defvar-local acm-backend-codeium-items nil)

;; https://github.com/Exafunction/codeium.el/blob/0240805690c685de9b75c953af2867b6fcc61208/codeium.el#L208
(defvar acm-backend-codeium-language-alist
  '(
	(nil . 0)
	(c-mode . 1)
	(c-ts-mode . 1)
	(clojure-mode . 2)
	(clojurec-mode . 2)
	(clojurescript-mode . 2)
	(coffee-mode . 3)
	(cc-mode . 4)
	(c++-mode . 4)
	(c++-ts-mode . 4)
	(csharp-mode . 5)
	(csharp-ts-mode . 5)
	(css-mode . 6)
	(css-ts-mode . 6)
	(cuda-mode . 7)
	(dockerfile-mode . 8)
	(dockerfile-ts-mode . 8)
	(go-dot-mod-mode . 9)
	(go-mod-ts-mode . 9)
	(go-mode . 9)
	(go-ts-mode . 9)
	(groovy-mode . 10)
	(haskell-mode . 12)
	(terraform-mode . 13)
	(html-mode . 14)
	(sgml-mode . 14)
	(mhtml-mode . 14)
	(java-mode . 16)
	(java-ts-mode . 16)
	(jdee-mode . 16)
	(ecmascript-mode . 17)
	(javascript-mode . 17)
	(js-mode . 17)
	(js2-mode . 17)
	(js-ts-mode . 17)
	(rjsx-mode . 17)
	(json-mode . 18)
	(json-ts-mode . 18)
	(julia-mode . 19)
	(ess-julia-mode . 19)
	(kotlin-mode . 20)
	(kotlin-ts-mode . 20)
	(latex-mode . 21)
	(less-mode . 22)
	(less-css-mode . 22)
	(lua-mode . 23)
	(lsp--render-markdown . 25)
	(markdown-mode . 25)
	(gfm-mode . 25)
	(objc-mode . 26)
	(perl-mode . 28)
	(cperl-mode . 28)
	(php-mode . 29)
	(php-ts-mode . 29)
	(text-mode . 30)
	(python-mode . 33)
	(python-ts-mode . 33)
	(cython-mode . 33)
	(ess-r-mode . 34)
	(ruby-mode . 35)
	(enh-ruby-mode . 35)
	(ruby-ts-mode . 35)
	(rust-mode . 36)
	(rust-ts-mode . 36)
	(rustic-mode . 36)
	(sass-mode . 37)
	(ssass-mode . 37)
	(scala-mode . 38)
	(scss-mode . 39)
	(sh-mode . 40)
	(ebuild-mode . 40)
	(pkgbuild-mode . 40)
	(sql-mode . 41)
	(swift-mode . 43)
	(ts-mode . 45)
	(typescript-mode . 45)
	(typescript-ts-mode . 45)
	(nxml-mode . 48)
	(xml-mode . 48)
	(yaml-mode . 50)
	(yaml-ts-mode . 50)
	(conf-toml-mode . 52)
	(toml-ts-mode . 52)
	(dart-mode . 53)
	(caml-mode . 55)
	(tuareg-mode . 55)
	(cmake-mode . 56)
	(cmake-ts-mode . 56)
	(pascal-mode . 57)
	(elixir-mode . 58)
	(elixir-ts-mode . 58)
	(heex-ts-mode . 58)
	(fsharp-mode . 59)
	(lisp-data-mode . 60)))

(defun acm-backend-codeium-candidates (keyword)
  (acm-with-cache-candidates
   acm-backend-codeium-cache-candiates
   (when (and acm-backend-codeium-items
              (>= (length keyword) acm-backend-codeium-candidate-min-length))
     acm-backend-codeium-items)))

(defun acm-backend-codeium-candidate-expand (candidate-info bound-start &optional preview)
  ;; We need replace whole area with codeium label.
  (let ((end-position (line-end-position)))
    (forward-line (- (plist-get candidate-info :line) (count-lines (point-min) (line-beginning-position))))
    (if preview
        (acm-preview-create-overlay (point) end-position (plist-get candidate-info :label))
      (delete-region (point) end-position)
      (insert (plist-get candidate-info :label))
      (when acm-backend-codeium-accept
        (lsp-bridge-call-async
         "codeium_completion_accept" (plist-get candidate-info :id))))))


(defun acm-backend-codeium-candidate-doc (candidate)
  (plist-get candidate :documentation))

(defun acm-backend-codeium-clean ()
  (setq-local acm-backend-codeium-items nil)
  (setq-local acm-backend-codeium-cache-candiates nil))

(provide 'acm-backend-codeium)
;;; acm-backend-codeium.el ends here
