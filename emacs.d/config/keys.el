;; Key chords!
(key-chord-mode 1)
(key-chord-define-global "jj" 'avy-goto-word-or-subword-1)
(key-chord-define-global "zz" 'yas-new-snippet)

(global-set-key "\C-m" 'newline-and-indent)
(global-set-key (kbd "s-.") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-`") 'ace-window)
(global-set-key (kbd "s-W") 'kill-some-buffers)
(global-set-key (kbd "s-w") 'kill-this-buffer)

(global-set-key (kbd "s-Z") 'undo-tree-redo)
(global-set-key (kbd "s-z") 'undo-tree-undo)

;; set keybindings
(global-set-key (kbd "C-M-s") 'instant-search-using-helm)
(global-set-key (kbd "C-M-S-s") 'helm-resume)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-c C-k") 'copy-line)
(global-set-key (kbd "C-c C-r") 'remember)
(global-set-key (kbd "C-c a") 'org-agenda)

(global-set-key (kbd "C-\'") 'sp-rewrap-sexp)

;; Move between buffers like Chrome tabs
(global-set-key (kbd "s-{") 'previous-buffer)
(global-set-key (kbd "s-}") 'next-buffer)

;; don't prompt when finding a tag
(global-set-key (kbd "M-.") 'sm-find-tag-other-window)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h"globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; Custom emacs shortcuts!
(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)
(global-set-key (kbd "C-x /") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "s-SPC") 'er/expand-region)
(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "C-d") 'duplicate-line)
(global-set-key [remap forward-line] 'goto-line-with-feedback)
(global-set-key "\C-m" 'newline-and-indent)

(defun td/insert-camel-buffer-name ()
  (interactive)
  (insert
    (string-inflection-camelcase-function
      (replace-regexp-in-string "-" "_"
        (substring (car (last (split-string buffer-file-name "/"))) 0 -4)))))

;; SUPPER COMMANDS
(global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen)

(global-set-key (kbd "s-b") 'helm-buffers-list)
(global-set-key (kbd "s-t") 'projectile-find-file)
(global-set-key (kbd "s-F") 'helm-git-grep-at-point)
