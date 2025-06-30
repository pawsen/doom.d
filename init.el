;;; init.el -*- lexical-binding: t; -*-

;;; This file controls what Doom modules are enabled and what order they load in.
;; Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find information about all of Doom's modules
;;      and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).

;; doom reload, SPC h r r

;; (setq doom-localleader-key ",")
;; (setq doom-localleader-alt-key ",")

(doom! :input
       :completion
       ;; orderless:
       ;; ressing C-SPC again while completing inserts a space as separator. This allows searching with space-separated terms; each piece will match individually and in any order, with smart casing. Pressing just SPC acts as normal and quits completion
       (corfu +dabbrev +icons +orderless)
       (vertico + icons)  ; the search engine of the future

       :ui
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       ;;doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;;fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;; hydra
       indent-guides     ; highlighted indent columns
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       ;;pretty-code       ; replace bits of code with pretty symbols
       ;;tabs              ; an tab bar for Emacs
       (treemacs +lsp)          ; a project drawer, like neotree but cooler
       (vc-gutter +pretty +diff-hl)  ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       ;; file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format) ; +onsave)  ; automated prettiness
       multiple-cursors  ; editing in many places at once
       ;;rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired
        +dirvish         ; making dired pretty [functional]
        +icons)
       electric          ; smarter, keyword-based electric-indent
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       eshell            ; a consistent, cross-platform shell (WIP)
       ;;shell             ; a terminal REPL for Emacs
       ;;term              ; terminals in Emacs
       vterm             ; another terminals in Emacs

       :checkers
       ;; grammar           ; install LT here: /usr/share/java/languagetool/
                            ; install java: sudo apt install default-jre
       (spell +flyspell +everywhere)
       (syntax +childframe)

       :tools
       ;; ansible
       (debugger +lsp)     ; FIXME stepping through code, to help you add bugs
       direnv              ; local env in .envrc file
       ;; (docker +lsp)
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
       (lookup           ; helps you navigate your code and documentation
        +docsets        ; ...or in Dash docsets locally
        +dictionary)
       ;; (lsp +eglot)
       (lsp +peek)
       (magit +childframe)             ; a git porcelain for Emacs
       ;; +forge)          ; porcelain for managing issues and PRs from within Emacs.
       make              ; run make tasks from Emacs
       ;; pdf               ; pdf enhancements
       tree-sitter       ; syntax highligtning
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)  ; MacOS-specific commands
       tty                 ; configures Emacs for terminal usage.

       :lang
       ;;agda              ; types of types of types of types...
       ;;assembly          ; assembly for fun or debugging
       (cc +lsp +tree-sitter)
       ;;(clojure +lsp)           ; java with a lisp
       common-lisp
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp           ; ML stands for Microsoft's Language
       ;;(go +lsp)           ; the hipster dialect
       ;;(haskell +intero) ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       json
       ;;(java +lsp) ; the poster child for carpal tunnel syndrome
       ;; (javascript +lsp +tree-sitter)       ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       (latex +cdlatex) 
       ;;lean
       ;;ledger            ; an accounting system in Emacs
       ;;lua
       markdown
        ; +grip)             ; live github-style previews of your markdown (or org) files.
                           ; <localleader> p
       ;;nim               ; python + lisp at the speed of c
       (nix +tree-sitter)  ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org
        +dragndrop       ; drag & drop files/images into org buffers
        ;; +journal
        +roam2
        +gnuplot         ; install gnuplot and gnuplot-mode, which enables rendering images from gnuplot src blocks or plotting tables with org-plot/gnuplot (bound to <localleader> b p, by default).
        +hugo            ; use Emacs for hugo blogging
        +jupyter        ; ipython/jupyter support for babel
        +pandoc          ; export-with-pandoc support
        ;;+pomodoro        ; be fruitful with the tomato technique
        ;;+present        ; using org-mode for presentations
        )
       ;;perl              ; write code no one else can comprehend
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python             ; beautiful is better than ugly
        ;; https://github.com/doomemacs/doomemacs/tree/master/modules/lang/python#module-flags
        +lsp
        ;conda
        ;cython
        ;+poetry
        ;+pyenv
        +pyright
        +tree-sitter)
       ;;qt
       ;;racket            ; a DSL for DSLs
       ;;rest              ; Emacs as a REST client
       rst               ; ReST in peace
       ;;ruby              ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust +lsp +tree-sitter)
       ;;scala             ; java, but good
       ;;scheme            ; a fully conniving family of lisps
       (sh
        +lsp
        +fish)           ; she sells {ba,z,fi}sh shells on the C xor
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       (web
        +lsp
        ;; +web-django-mode)
        )
       yaml                ; JSON, but readable

       :email
       ;; (:if (executable-find "mu") (mu4e +org +gmail))
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;irc               ; how neckbeards socialize
       ;; (rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought
       ;;everywhere          ; Spawn an emacsclient window for quick edits.
       ;; ereader

       :config
       ;;literate
       (default +bindings +smartparens)

       :private
       ;; my-python
)
