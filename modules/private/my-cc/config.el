;;; private/my-cc/config.el -*- lexical-binding: t; -*-

;; https://github.com/MaskRay/Config/blob/master/home/.config/doom/modules/private/my-cc/packages.el

;; (after! ccls
;;   (setq
;;    ccls-initialization-options
;;    `(:clang
;;      :completion
;;      (:include
;;       (:blacklist
;;        ["^/usr/(local/)?include/c\\+\\+/[0-9\\.]+/(bits|tr1|tr2|profile|ext|debug)/"
;;         "^/usr/(local/)?include/c\\+\\+/v1/"
;;         ]))
;;      :index (:initialBlacklist ,+ccls-initial-blacklist :parametersInDeclarations :json-false :trackDependency 1)))
;;    )

(when IS-MAC
(after! ccls
  (setq ccls-initialization-options
        '(:clang (:extraArgs ["-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1"
                              "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
                              "-isystem/usr/local/include"
                              "-isystem/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/11.0.0/include"
                              "-isystem/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
                              "-isystem/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include"
                              "-isystem/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/System/Library/Frameworks"]
                  :resourceDir "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/11.0.0")))
  ))




(after! cc-mode
  (map!
   :map (c-mode-map c++-mode-map)
   :n "C-h" (λ! (ccls-navigate "U"))
   :n "C-j" (λ! (ccls-navigate "R"))
   :n "C-k" (λ! (ccls-navigate "L"))
   :n "C-l" (λ! (ccls-navigate "D"))
   (:leader
     :n "=" #'clang-format-region
     )
   (:localleader
     :n "a" #'ccls/references-address
     :n "f" #'ccls/references-not-call
     :n "lp" #'ccls-preprocess-file
     :n "lf" #'ccls-reload
     :n "m" #'ccls/references-macro
     :n "r" #'ccls/references-read
     :n "w" #'ccls/references-write
     :desc "breakpoint"
     :n "db" (lambda ()
               (interactive)
               (evil-open-above 1)
               (insert "volatile static int z=0;while(!z)asm(\"pause\");")
               (evil-normal-state))
     :n "dd" #'realgud:gdb
     ))

  ;; LLVM coding style guidelines in emacs
  ;; Maintainer: LLVM Team, http://llvm.org/
  ;; https://github.com/llvm-mirror/llvm/blob/master/utils/emacs/emacs.el
  ;; https://github.com/motine/cppstylelineup
  (defun llvm-lineup-statement (langelem)
    (let ((in-assign (c-lineup-assignments langelem)))
      (if (not in-assign)
          '++
        (aset in-assign 0
              (+ (aref in-assign 0)
                 (* 2 c-basic-offset)))
        in-assign)))

  ;; Add a cc-mode style for editing LLVM C and C++ code
  (c-add-style "llvm"
               '("gnu"
                 (fill-column . 80)
                 (c++-indent-level . 2)
                 (c-basic-offset . 2)
                 (indent-tabs-mode . nil)
                 (c-offsets-alist . ((arglist-intro . ++)
                                     (innamespace . 0)
                                     (member-init-intro . ++)
                                     (statement-cont . llvm-lineup-statement)))))
  (when (listp c-default-style)
    (setf (alist-get 'other c-default-style) "llvm"))

  (add-hook 'cc-mode-hook
          (lambda () ((set (make-local-variable 'comment-auto-fill-only-comments) t))))

  )

(use-package! clang-format
  :commands (clang-format-region)
  )

;; recommended helpers
;; https://github.com/MaskRay/ccls/wiki/lsp-mode#cross-reference-extensions
(defun ccls/callee ()
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))
(defun ccls/caller ()
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/call"))
(defun ccls/vars (kind)
  (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))
(defun ccls/base (levels)
  (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))
(defun ccls/derived (levels)
  (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))
(defun ccls/member (kind)
  (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))


;; The meaning of :role corresponds to https://github.com/maskray/ccls/blob/master/src/symbol.h

;; References w/ Role::Address bit (e.g. variables explicitly being taken addresses)
(defun ccls/references-address ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 128)))

;; References w/ Role::Dynamic bit (macro expansions)
(defun ccls/references-macro ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 64)))

;; References w/o Role::Call bit (e.g. where functions are taken addresses)
(defun ccls/references-not-call ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :excludeRole 32)))

;; References w/ Role::Read
(defun ccls/references-read ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 8)))

;; References w/ Role::Write
(defun ccls/references-write ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 16)))

;; ccls/vars ccls/base ccls/derived ccls/members have a parameter while others are interactive.
;; (ccls/base 1) direct bases
;; (ccls/derived 1) direct derived
;; (ccls/member 2) => 2 (Type) => nested classes / types in a namespace
;; (ccls/member 3) => 3 (Func) => member functions / functions in a namespace
;; (ccls/member 0) => member variables / variables in a namespace
;; (ccls/vars 1) => field
;; (ccls/vars 2) => local variable
;; (ccls/vars 3) => field or local variable. 3 = 1 | 2
;; (ccls/vars 4) => parameter


;; https://github.com/syohex/emacs-quickrun/tree/master/sample
;; (after! quickrun
;;   (quickrun-add-command "c++/c1z"
;;     '((:command . "clang++")
;;       (:exec    . ("%c -std=c++1z %o -o %e %s"
;;                    "%e %a"))
;;       (:remove  . ("%e")))
;;     :default "c++"))
