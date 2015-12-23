(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
; (desktop-save-mode 1)

;; Emacs from command line
(x-focus-frame nil)

;; Starting emacs config lib
(load "~/.emacs.d/prelude.el")

(set-face-attribute 'default nil :family "Inconsolata")
(add-to-list 'default-frame-alist '(font . "Inconsolata-18"))

(setq scroll-margin 3
      scroll-preserve-screen-position 1
      indent-line-function 'insert-tab)

(setq ns-use-srgb-colorspace 't)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(defun td/web-mode-hook ()
  (setq web-mode-comment-style 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-attr-indent-offset 2))

(add-hook 'web-mode-hook  'td/web-mode-hook)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Helpful frame title
(setq frame-title-format '((buffer-file-name "%f" (dired-directory dired-directory "%b"))" - %m mode (emacs " emacs-version ")"))

;; Trim trailing spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; File mode aliases
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.cjsx$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Mac stuff
(setq ns-function-modifier 'hyper)

;; Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; String inflection bonuses
(require 'string-inflection)

;; Helm
(require 'helm)
(require 'helm-config)
(require 'prelude-helm-everywhere)
;; (setq projectile-enable-caching f)
(helm-mode 1)
(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(eval-after-load "helm-regexp"
  '(setq helm-source-moccur
         (helm-make-source "Moccur"
             'helm-source-multi-occur :follow 1)))

(with-eval-after-load 'helm-projectile
(defvar helm-source-file-not-found
  (helm-build-dummy-source
      "Create file"
    :action (lambda (cand) (find-file cand))))

(add-to-list 'helm-projectile-sources-list helm-source-file-not-found t))

(defun my-helm-multi-all ()
  "multi-occur in all buffers backed by files."
  (interactive)
  (helm-multi-occur
   (delq nil
         (mapcar (lambda (b)
                   (when (buffer-file-name b) (buffer-name b)))
                 (buffer-list)))))

;; C-SPC after C-U C-SPC to keep moving through mark ring
(setq set-mark-command-repeat-pop t)

;; Projectile!
(projectile-global-mode)

;; Remember where I'm at!
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Functions to modify text
(load "~/.emacs.d/config/text-manipulation-fn.el")

;; Functions to navigate code
(load "~/.emacs.d/config/code-navigation-fn.el")

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setq css-indent-offset 2)

;; Some coffeescript stuff
(setq whitespace-action '(auto-cleanup))
(custom-set-variables '(coffee-tab-width 2))

;; Facilitate variable renaming
(smartscan-mode 1)

;; Smart mode line
(setq sml/theme 'dark)

;; Terse yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; This is auto completion via tags.
(eval-after-load "etags" '(progn (ac-etags-setup)))

(add-hook 'c-mode-common-hook 'ac-etags-ac-setup)
(add-hook 'ruby-mode-common-hook 'ac-etags-ac-setup)

(normal-erase-is-backspace-mode 1)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Show how much battery percent I have left (I fullscreen often)
(add-hook 'after-init-hook #'fancy-battery-mode)

;; Speedbar config
(require 'speedbar)
(speedbar-add-supported-extension ".rb")
(speedbar-add-supported-extension ".js")
(speedbar-add-supported-extension ".coffee")
(speedbar-add-supported-extension ".cjsx")
(setq speedbar-use-images nil)

(global-auto-highlight-symbol-mode t)

;; Optimal Git client
(require 'magit)
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(setq magit-diff-refine-hunk 'all)
(setq magit-push-always-verify nil)
(setq magit-branch-arguments nil)
(setq magit-push-arguments '("--set-upstream"))

;; I tend to keep a magit window open somewhere and prefer to see it update live
(add-hook 'after-save-hook 'magit-after-save-refresh-status)

(add-to-list 'magit-no-confirm 'stage-all-changes)
(defun magit-key-mode--add-default-options (arguments)
  (if (eq (car arguments) 'pulling)
      (list 'pulling (list "--rebase"))
    arguments))

(advice-add 'magit-key-mode :filter-args #'magit-key-mode--add-default-options)

;; Quick and easy snippets
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(setq yas-global-mode t)
(setq yas-indent-mode 'auto)
(yas-reload-all) ;; always want up-to-date snippets

(add-hook 'web-mode-hook #'yas-minor-mode)
(add-hook 'coffee-mode-hook #'yas-minor-mode)
(add-hook 'ruby-mode-hook #'yas-minor-mode)

;; Prodigy proccesses
(load "~/.emacs.d/config/prodigy.el")

; Emmet stuff
(add-hook 'coffee-mode-hook  'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

(setq visible-bell nil)
(setq ring-bell-function `(lambda ()
                            (message "*ding*")))

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

;; Allow jsx to somewhat play with coffescript
(require 'mmm-mode)
(mmm-add-classes
 '((jsx
    :submode web-mode
    :front "\\((\\)[[:space:]\n]*<"
    :front-match 1
    :back ">[[:space:]\n]*\\()\\)"
    :back-match 1)))

(setq mmm-global-mode 'maybe)
(mmm-add-mode-ext-class 'coffee-mode "\\.cjsx\\'" 'jsx)

(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))

;; Web mode for editing PHP, HTML, CSS, some JS, ERB
(require 'web-mode)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-extra-auto-pairs
  '(("erb"  . (("beg" "end")))
  ("php"  . (("beg" "end")
  ("beg" "end")))
))

;; Checking stuff on-the-fly
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)
(setq flycheck-checkers '(javascript-eslint ruby-rubocop))

;; Add support for ascii emojis
(add-hook 'after-init-hook #'global-emojify-mode)
(emoji-fontset-enable "Symbola")

;; More serious editing of JS
(setq-default js2-basic-offset 2)
(setq js-indent-level 2)
(custom-set-variables
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p -1)
)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'js-mode)

;; use tern for JS auto completion
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

; Auto indentation
(add-hook 'ruby-mode-hook #'aggressive-indent-mode)

(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(add-hook 'ruby-mode-common-hook 'font-lock-comment-annotations)

(setq compilation-scroll-output t)

;; recompile init file after save
(byte-recompile-file "init.el")

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(load "~/.emacs.d/config/org.el")
(load "~/.emacs.d/config/keys.el")

;; start the emacsserver that listens to emacsclient
(server-start)

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(defun save-macro (name)
  "save a Macro. Take a name as argument
     and save the last defined macro under
     this name at the end of your .emacs"
  (interactive "SName of the macro :")  ; ask for the name of the macro
  (kmacro-name-last-macro name)         ; use this name for the macro
  (find-file user-init-file)            ; open ~/.emacs or other user init file
  (goto-char (point-max))               ; go to the end of the .emacs
  (newline)                             ; insert a newline
  (insert-kbd-macro name)               ; copy the macro
  (newline)                             ; insert a newline
  (switch-to-buffer nil))               ; return to the initial buffer

(fset 'td/scss-add-colon-spaces
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217765 58 return 58 32 return 33 134217788 134217765 58 32 32 return 58 32 return 33] 0 "%d")) arg)))

(fset 'td/convert-next-snake-instance-to-camel
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217788 19 95 return 134217848 108 111 119 101 114 45 99 97 109 101 108 return] 0 "%d")) arg)))

;;; init.el ends here

(fset 'td/countr/convert-prop-to-proxy
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 125 return left 41943072 8388707 18 114 101 110 100 101 114 return up] 0 "%d")) arg)))

(fset 'td/countr/convert_fn_to_lodash_partial
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([18 123 return right 95 46 112 97 114 116 105 97 108 40 kp-delete 19 40 return backspace 32 44 backspace backspace 44 32 19 41 return] 0 "%d")) arg)))
