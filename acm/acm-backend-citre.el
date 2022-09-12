;;; acm-backend-citre.el --- Citre backend for acm

;; Filename: acm-backend-citre.el
;; Description: Citre backend for acm
;; Author: skfwe <wcq-062821@163.com>
;; Copyright (C) 2022, skfwe, all rights reserved.
;; Compatibility: GNU Emacs 28.1
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Citre backend for acm
;;

;;; Installation:
;;
;; Put acm-backend-citre.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'acm-backend-citre)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET acm-backend-citre RET
;;

;;; Change log:
;;
;; 2022/08/31
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:

(defcustom acm-enable-citre nil
  "Popup citre completions when this option is turn on."
  :type 'boolean)

(defcustom acm-backend-citre-keyword-complete t
  "push mode keyword to candidate"
  :type 'boolean)

(defvar-local acm-backend-citre-search-keyword "")

(defvar-local acm-backend-citre-prefix-keyword ""
  "when type . keyword is \"\" so we use symbol befoe . as keyword prefix")

(defvar acm-backend-citre-keywords-alist
  `((verilog-mode
     ;; compiler directives, from IEEE 1800-2012 section 22.1
     "`__FILE__" "`__LINE" "`begin_keywords" "`celldefine" "`default_nettype"
     "`define" "`else" "`elsif" "`end_keywords" "`endcelldefine" "`endif"
     "`ifdef" "`ifndef" "`include" "`line" "`nounconnected_drive" "`pragma"
     "`resetall" "`timescale" "`unconnected_drive" "`undef" "`undefineall"
     ;; compiler directives not covered by IEEE 1800
     "`case" "`default" "`endfor" "`endprotect" "`endswitch" "`endwhile" "`for"
     "`format" "`if" "`let" "`protect" "`switch" "`time_scale" "`uselib"
     "`while" "after" "alias" "always" "always_comb" "always_ff" "always_latch" "analog" "and"
     "assert" "assign" "assume" "automatic" "before" "begin" "bind"
     "bins" "binsof" "bit" "break" "buf" "bufif0" "bufif1" "byte"
     "case" "casex" "casez" "cell" "chandle" "class" "clocking" "cmos"
     "config" "const" "constraint" "context" "continue" "cover"
     "covergroup" "coverpoint" "cross" "deassign" "default" "defparam"
     "design" "disable" "dist" "do" "edge" "else" "end" "endcase"
     "endclass" "endclocking" "endconfig" "endfunction" "endgenerate"
     "endgroup" "endinterface" "endmodule" "endpackage" "endprimitive"
     "endprogram" "endproperty" "endspecify" "endsequence" "endtable"
     "endtask" "enum" "event" "expect" "export" "extends" "extern"
     "final" "first_match" "for" "force" "foreach" "forever" "fork"
     "forkjoin" "function" "generate" "genvar" "highz0" "highz1" "if"
     "iff" "ifnone" "ignore_bins" "illegal_bins" "import" "incdir"
     "include" "initial" "inout" "input" "inside" "instance" "int"
     "integer" "interface" "intersect" "join" "join_any" "join_none"
     "large" "liblist" "library" "local" "localparam" "logic"
     "longint" "macromodule" "mailbox" "matches" "medium" "modport" "module"
     "nand" "negedge" "new" "nmos" "nor" "noshowcancelled" "not"
     "notif0" "notif1" "null" "or" "output" "package" "packed"
     "parameter" "pmos" "posedge" "primitive" "priority" "program"
     "property" "protected" "pull0" "pull1" "pulldown" "pullup"
     "pulsestyle_onevent" "pulsestyle_ondetect" "pure" "rand" "randc"
     "randcase" "randsequence" "rcmos" "real" "realtime" "ref" "reg"
     "release" "repeat" "return" "rnmos" "rpmos" "rtran" "rtranif0"
     "rtranif1" "scalared" "semaphore" "sequence" "shortint" "shortreal"
     "showcancelled" "signed" "small" "solve" "specify" "specparam"
     "static" "string" "strong0" "strong1" "struct" "super" "supply0"
     "supply1" "table" "tagged" "task" "this" "throughout" "time"
     "timeprecision" "timeunit" "tran" "tranif0" "tranif1" "tri"
     "tri0" "tri1" "triand" "trior" "trireg" "type" "typedef" "union"
     "unique" "unsigned" "use" "uwire" "var" "vectored" "virtual" "void"
     "wait" "wait_order" "wand" "weak0" "weak1" "while" "wildcard"
     "wire" "with" "within" "wor" "xnor" "xor"
     ;; 1800-2009
     "accept_on" "checker" "endchecker" "eventually" "global" "implies"
     "let" "nexttime" "reject_on" "restrict" "s_always" "s_eventually"
     "s_nexttime" "s_until" "s_until_with" "strong" "sync_accept_on"
     "sync_reject_on" "unique0" "until" "until_with" "untyped" "weak"
     ;; 1800-2012
     "implements" "interconnect" "nettype" "soft"
     ;; AMS
     "connectmodule" "endconnectmodule"
     )
    ))

(defun acm-backend-citre-clean ()
  (setq-local acm-backend-citre-prefix-keyword ""))

(defun acm-backend-citre-get-mode-keyword ()
  (let* ((mode-keyword (list)))
    (catch 'break
      (dolist (item acm-backend-citre-keywords-alist)
        (when (eq major-mode (car item))
          (setq mode-keyword (seq-subseq item 1))
          (throw 'break t))))
      (when mode-keyword
        (dolist (mk mode-keyword)
          (setq mk (citre-put-property mk 'annotation "keyword"))))
      mode-keyword))

(defun acm-backend-citre-candidates (keyword)
  (when acm-enable-citre
    (let* ((candidates (list)))
      (if (string-empty-p keyword)
          (progn
            (setq-local acm-backend-citre-prefix-keyword 
                        (save-excursion
                          (backward-char)
                          (if (string= (char-to-string (following-char)) ".")
                              (thing-at-point 'symbol)
                            "")))
            ;; When there is nothing in front of the . , it will get nil
            (unless acm-backend-citre-prefix-keyword
              (setq-local acm-backend-citre-prefix-keyword ""))
            (setq-local acm-backend-citre-search-keyword acm-backend-citre-prefix-keyword))
        (if (string-suffix-p (substring keyword 0 (1- (length keyword))) acm-backend-citre-search-keyword)
            (setq-local acm-backend-citre-search-keyword (concat acm-backend-citre-search-keyword (substring (string-reverse keyword) 0 1)))
          (setq-local acm-backend-citre-search-keyword keyword)))
      (let* ((collection (unless (string-empty-p acm-backend-citre-search-keyword)
                           (delete-dups (citre-capf--get-collection acm-backend-citre-search-keyword)))))
        (when acm-backend-citre-keyword-complete
          (setq collection (append (acm-backend-citre-get-mode-keyword) collection)))
        (when collection
          (dolist (candidate collection)
            (when (acm-candidate-fuzzy-search acm-backend-citre-search-keyword candidate)
              (let* ((annotation (replace-regexp-in-string "[() ]" "" (replace-regexp-in-string ")?@.*" "" (citre-get-property 'annotation candidate))))
                     (candidate-fix (replace-regexp-in-string (concat "^" acm-backend-citre-prefix-keyword "\\.?") "" candidate)))
                (add-to-list 'candidates (list :key candidate-fix
                                               :icon (downcase annotation)
                                               :label candidate-fix
                                               :display-label candidate-fix
                                               :annotation annotation 
                                               :backend "citre")
                             t))))
          (acm-candidate-sort-by-prefix acm-backend-citre-search-keyword candidates))))))

(defun acm-backend-citre-candidate-expand (candidate-info bound-start)
  (delete-region bound-start (point))
  (insert (plist-get candidate-info :label)))

(provide 'acm-backend-citre)

;;; acm-backend-citre.el ends here
