(setq tylers-package-list '
      (
       4clojure
       ac-emmet
       ac-etags
       ac-slime
       ace-jump-buffer
       ace-jump-mode
       ace-window
       aggressive-indent
       alert
       anything
       anything-exuberant-ctags
       anzu
       async
       auto-complete
       auto-complete-exuberant-ctags
       auto-highlight-symbol
       avy
       avy-zap
       back-button
       browse-kill-ring
       chronos
       cider
       circe
       clj-refactor
       clojure-mode
       clojure-snippets
       coffee-mode
       color-theme
       color-theme-sanityinc-tomorrow
       color-theme-solarized
       company
       ctags
       dash
       deferred
       diff-hl
       diminish
       dired+
       dired-details
       dired-details+
       dired-efap
       dirtree
       discover
       discover-clj-refactor
       discover-my-major
       easy-kill
       ecb
       edit-server
       edit-server-htmlize
       edn
       elisp-slime-nav
       elixir-mode
       emmet-mode
       emoji-display
       emoji-fontset
       emojify
       enh-ruby-mode
       epl
       eshell-did-you-mean
       eshell-prompt-extras
       exec-path-from-shell
       expand-region
       f
       fancy-battery
       flx
       flx-ido
       flycheck
       flycheck-pos-tip
       flycheck-status-emoji
       flycheck-tip
       flymake-coffee
       flymake-easy
       flymake-ruby
       font-utils
       geiser
       gh
       gist
       git-commit
       git-messenger
       git-timemachine
       gitconfig-mode
       gitignore-mode
       gntp
       god-mode
       grizzl
       guru-mode
       hackernews
       helm
       helm-ag
       helm-core
       helm-descbinds
       helm-emmet
       helm-git-grep
       helm-gtags
       helm-projectile
       helm-robe
       ht
       hungry-delete
       hyde
       hydra
       ido-completing-read+
       ido-ubiquitous
       inf-ruby
       inflections
       js2-highlight-vars
       js2-mode
       js2-refactor
       json-mode
       json-reformat
       json-snatcher
       jsx-mode
       key-chord
       kibit-helper
       let-alist
       leuven-theme
       list-utils
       log4e
       logito
       magit
       magit-popup
       makey
       markdown-mode
       markdown-mode+
       mmm-mode
       move-text
       multiple-cursors
       names
       nav-flash
       noctilux-theme
       noflet
       operate-on-number
       org
       org-gcal
       ov
       paredit
       pcache
       peg
       persistent-soft
       php-mode
       pivotal-tracker
       pkg-info
       polymode
       popup
       pos-tip
       powerline
       prodigy
       projectile
       projectile-rails
       projectile-speedbar
       queue
       rainbow-delimiters
       rainbow-mode
       rake
       request
       request-deferred
       rich-minority
       robe
       rspec-mode
       rubocop
       ruby-additional
       ruby-electric
       ruby-hash-syntax
       ruby-refactor
       ruby-tools
       s
       s-buffer
       scss-mode
       seq
       slime
       smart-mode-line
       smart-mode-line-powerline-theme
       smartparens
       smartrep
       smartscan
       smex
       solarized-theme
       spinner
       sr-speedbar
       string-inflection
       stylus-mode
       sublime-themes
       sws-mode
       tern
       tern-auto-complete
       thesaurus
       tree-mode
       ucs-utils
       undo-tree
       unicode-emoticons
       unicode-fonts
       vkill
       volatile-highlights
       w3
       web-mode
       which-key
       windata
       wisp-mode
       wispjs-mode
       with-editor
       yaml-mode
       yasnippet
       zenburn-theme
       zop-to-char
       ))

(mapc #'package-install tylers-package-list)
