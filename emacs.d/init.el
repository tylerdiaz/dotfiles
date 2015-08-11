;; init.el --- Prelude's configuration entry point.
;;
;; Copyright (c) 2011 Bozhidar Batsov
;
; Author: Bozhidar Batsov <bozhidar@batsov.com>
; URL: http://batsov.com/prelude
; Version: 1.0.0
; Keywords: convenience

; This file is not part of GNU Emacs.

;; Commentary:

; This file simply sets up the default load path and requires
; the various modules defined within Emacs Prelude.

;; License:

; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 3
; of the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with GNU Emacs; see the file COPYING.  If not, write to the
; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
; Boston, MA 02110-1301, USA.

;;; Code:
(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Prelude is powering up... Be patient, Master %s!" current-user)

(when (version< emacs-version "24.1")
  (error "Prelude requires at least GNU Emacs 24.1, but you're running %s" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)

(defvar prelude-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Prelude distribution.")
(defvar prelude-core-dir (expand-file-name "core" prelude-dir)
  "The home of Prelude's core functionality.")
(defvar prelude-modules-dir (expand-file-name  "modules" prelude-dir)
  "This directory houses all of the built-in Prelude modules.")
(defvar prelude-personal-dir (expand-file-name "personal" prelude-dir)
  "This directory is for your personal configuration.

Users of Emacs Prelude are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by Prelude.")
(defvar prelude-personal-preload-dir (expand-file-name "preload" prelude-personal-dir)
  "This directory is for your personal configuration, that you want loaded before Prelude.")
(defvar prelude-vendor-dir (expand-file-name "vendor" prelude-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar prelude-savefile-dir (expand-file-name "savefile" prelude-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar prelude-modules-file (expand-file-name "prelude-modules.el" prelude-dir)
  "This files contains a list of modules that will be loaded by Prelude.")

(unless (file-exists-p prelude-savefile-dir)
  (make-directory prelude-savefile-dir))

(defun prelude-add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path name)
        (prelude-add-subfolders-to-load-path name)))))

;; add Prelude's directories to Emacs's `load-path'
(add-to-list 'load-path prelude-core-dir)
(add-to-list 'load-path prelude-modules-dir)
(add-to-list 'load-path prelude-vendor-dir)
(prelude-add-subfolders-to-load-path prelude-vendor-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; preload the personal settings from `prelude-personal-preload-dir'
(when (file-exists-p prelude-personal-preload-dir)
  (message "Loading personal configuration files in %s..." prelude-personal-preload-dir)
  (mapc 'load (directory-files prelude-personal-preload-dir 't "^[^#].*el$")))

(message "Loading Prelude's core...")

;; the core stuff
(require 'prelude-packages)
(require 'prelude-custom)  ;; Needs to be loaded before core, editor and ui
(require 'prelude-ui)
(require 'prelude-core)
(require 'prelude-mode)
(require 'prelude-editor)
                                        ;(require 'preludeglobal-keybindings)

;; OSX specific settings
(when (eq system-type 'darwin)
  (require 'prelude-osx))

(message "Loading Prelude's modules...")

;; the modules
(if (file-exists-p prelude-modules-file)
    (load prelude-modules-file)
  (message "Missing modules file %s" prelude-modules-file)
  (message "You can get started by copying the bundled example file"))

;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" prelude-personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p prelude-personal-dir)
  (message "Loading personal configuration files in %s..." prelude-personal-dir)
  (mapc 'load (directory-files prelude-personal-dir 't "^[^#].*el$")))

(message "Prelude is ready to do thy bidding, Master %s!" current-user)

(prelude-eval-after-init
 ;; greet the use with some useful tip
 (run-at-time 5 nil 'prelude-tip-of-the-day))

(set-default-font "Inconsolata-24")
(set-face-attribute 'default nil :family "Inconsolata")
(add-to-list 'default-frame-alist '(font . "Inconsolata-24"))

;;; init.el ends here
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; This is for Melpa, a "better" package managment archive
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize) ;; You might already have this

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.cjsx$" . coffee-mode))


(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

;; Helm
(require 'helm)
(require 'helm-config)
(require 'prelude-helm-everywhere)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

;; Projectile!
(projectile-global-mode)

;; Remember where I'm at!
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

;; magit bliss
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Mac stuff
(setq ns-function-modifier 'hyper)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Line shuffling!
(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun lunaryorn-new-buffer-frame ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(setq css-indent-offset 2)

;; Some coffeescript stuff
(setq whitespace-action '(auto-cleanup))
(custom-set-variables '(coffee-tab-width 2))

(sml/setup)
(fset 'yes-or-no-p 'y-or-n-p)

;; Emacs from command line
(x-focus-frame nil)

;; This is auto completion via tags.
(eval-after-load "etags"
  '(progn
     (ac-etags-setup)))

(add-hook 'c-mode-common-hook 'ac-etags-ac-setup)
(add-hook 'ruby-mode-common-hook 'ac-etags-ac-setup)

;; instant search across all buffers with helm
(defun instant-search-using-helm ()
  "Multi-occur in all buffers backed by files."
  (interactive)
  (let ((helm-after-initialize-hook #'helm-follow-mode))
    (helm-multi-occur
     (delq nil
           (mapcar (lambda (b)
                     (when (buffer-file-name b) (buffer-name b)))
                   (buffer-list))))))
(key-chord-mode 1)

;; Key chords!
(key-chord-define-global "jj" 'ace-jump-mode)

;; set keybindings
(global-set-key (kbd "C-M-s") 'instant-search-using-helm)
(global-set-key (kbd "C-M-S-s") 'helm-resume)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-c C-k") 'copy-line)

(defun sm-find-tag-other-window ()
  (interactive)
  (let ((tag (funcall (or find-tag-default-function
                          (get major-mode 'find-tag-default-function)
                          'find-tag-default))))
    (if tag
        (find-tag-other-window
         (replace-regexp-in-string "[<>]" "" (regexp-quote tag))))))

(defun find-tag-no-prompt ()
  "Jump to the tag at point without prompting"
  (interactive)
  (find-tag (find-tag-default)))

(defun bh/org-todo (arg)
    (interactive "p")
    (if (equal arg 4)
        (save-restriction
          (bh/narrow-to-org-subtree)
          (org-show-todo-tree nil))
      (bh/narrow-to-org-subtree)
      (org-show-todo-tree nil)))

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

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
(global-set-key (kbd "C-x /") 'toggle-comment-on-line)
(global-set-key (kbd "s-SPC") 'er/expand-region)
(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "C-d") 'duplicate-line)
(global-set-key (kbd "C-c n") #'lunaryorn-new-buffer-frame)
(global-set-key [remap goto-line] 'goto-line-with-feedback)
(global-set-key (kbd "<f5>") 'bh/org-todo)

;; SUPPER COMMANDS
(global-set-key (kbd "s-b") 'helm-buffers-list)
(global-set-key (kbd "s-t") 'projectile-find-file)
;(global-set-key (kbd "s-t") 'imenu)
(global-set-key (kbd "s-\\") 'xah-change-bracket-pairs)
(global-set-key (kbd "s-F") 'helm-git-grep-at-point)

;; Local key modes
; (define-key god-local-mode-map (kbd ".") 'repeat)

(normal-erase-is-backspace-mode 1)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-hook 'after-init-hook #'fancy-battery-mode)

(eval-after-load "helm-regexp"
  '(setq helm-source-moccur
         (helm-make-source "Moccur"
             'helm-source-multi-occur :follow 1)))

(defun my-helm-multi-all ()
  "multi-occur in all buffers backed by files."
  (interactive)
  (helm-multi-occur
   (delq nil
         (mapcar (lambda (b)
                   (when (buffer-file-name b) (buffer-name b)))
                 (buffer-list)))))

(require 'speedbar)
(speedbar-add-supported-extension ".rb")
(speedbar-add-supported-extension ".js")
(speedbar-add-supported-extension ".coffee")
(speedbar-add-supported-extension ".cjsx")
(setq speedbar-use-images nil)
                                        ; (scroll-bar-mode -1)
(global-auto-highlight-symbol-mode t)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(defun xah-change-bracket-pairs (φp1 φp2 φfromType φtoType)
  "Change bracket pairs from one type to another on current line or
   selection. For example, change all parenthesis () to square brackets [].
   When called in lisp program, φp1 φp2 are region begin/end position, φfromType or
   φtoType is a string of a bracket pair. ⁖ \"()\",  \"[]\", etc.

   URL `http://ergoemacs.org/emacs/elisp_change_brackets.html'
   Version 2015-04-12"
  (interactive
   (let ((brackets
          '("()" "{}" "[]" "<>" "\"\"")))
     (if (use-region-p)
         (progn (list
                 (region-beginning)
                 (region-end)
                 (ido-completing-read "Replace this:" brackets )
                 (ido-completing-read "To:" brackets )))
       (progn
         (list
          (line-beginning-position)
          (line-end-position)
          (ido-completing-read "Replace this:" brackets )
          (ido-completing-read "To:" brackets ))))))
  (let* (
         (ξfindReplaceMap
          (vector
           (vector (char-to-string (elt φfromType 0)) (char-to-string (elt φtoType 0)))
           (vector (char-to-string (elt φfromType 1)) (char-to-string (elt φtoType 1))))))
    (save-excursion
      (save-restriction
        (narrow-to-region φp1 φp2)
        (let ( (case-fold-search nil))
          (mapc
           (lambda (ξx)
             (goto-char (point-min))
             (while (search-forward (elt ξx 0) nil t)
               (replace-match (elt ξx 1) 'FIXEDCASE 'LITERAL)))
           ξfindReplaceMap))))))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(setq magit-diff-refine-hunk 'all)
(desktop-save-mode 1)

(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(setq yas-global-mode 't)
(setq yas-indent-mode 'fixed)

(prodigy-define-service
  :name "Government/Web Server"
  :command "gulp"
  :args '("webserver")
  :tags '(government gulp)
  :cwd "/Users/tyler/Desktop/Government"
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Government/Gulp"
  :command "gulp"
  :tags '(government gulp)
  :cwd "/Users/tyler/Desktop/Government"
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Government/Server processing"
  :command "nodemon"
  :args '("--watch" "server/app.js" "server/app.js")
  :tags '(government node)
  :cwd "/Users/tyler/Desktop/Government"
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t
)

(prodigy-define-service
  :name "Government/Firebase stub"
  :command "nodemon"
  :args '("--watch" "firebase-app.js" "firebase-app.js")
  :tags '(government node)
  :cwd "/Users/tyler/Desktop/Government"
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Personal Blog/webserver"
  :command "bundle"
  :args '("exec" "jekyll" "serve" "--watch" "--drafts")
  :tags '(blog jekyll)
  :cwd "/Users/tyler/Desktop/tylerdiaz.github.io"
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t)

(setq visible-bell nil)
  (setq ring-bell-function `(lambda ()
                            (set-face-background 'default "#111111")
                            (set-face-background 'default "black")))

;; Time to be productive!!
(setq org-remember-templates
      '(("Tasks" ?t "* TODO %?\n  %i\n  %a" "~/notebooks/personal/organizer.org")                      ;; (2)
        ("Appointments" ?a "* Appointment: %?\n%^T\n%i\n  %a" "~/notebooks/personal/organizer.org")))
   (setq remember-annotation-functions '(org-remember-annotation))
   (setq remember-handler-functions '(org-remember-handler))
   (eval-after-load 'remember
     '(add-hook 'remember-mode-hook 'org-remember-apply-template))
   (global-set-key (kbd "C-c r") 'remember)                                         ;; (3)
   (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))                           ;; (4)
   (global-set-key (kbd "C-c a") 'org-agenda)                                       ;; (5)
   (setq org-todo-keywords '("CONSIDERING" "TODO" "STARTED" "WAITING" "DONE"))                    ;; (6)
   (setq org-agenda-include-diary t)                                                ;; (7)
   (setq org-agenda-include-all-todo t)
   (setq org-startup-indented t)

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
    Ease of use features:
    - Move to start of next line.
    - Appends the copy on sequential calls.
    - Use newline as last char even on the last line of the buffer.
    - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(setq bitly-access-token "5690a3863775485d27819880a329e793d6685f3c")

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

(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-enable-current-element-highlight t)

(setq-default js2-basic-offset 2)
(custom-set-variables
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p -1)
)

(defun apply-function-to-region-lines (fn)
  (interactive "function to apply to lines in region: ")
  (save-excursion
    (goto-char (region-end))
    (let ((end-marker (copy-marker (point-marker)))
          next-line-marker)
      (goto-char (region-beginning))
      (if (not (bolp))
          (forward-line 1))
      (setq next-line-marker (point-marker))
      (while (< next-line-marker end-marker)
        (let ((start nil)
              (end nil))
          (goto-char next-line-marker)
          (save-excursion
            (setq start (point))
            (forward-line 1)
            (set-marker next-line-marker (point))
            (setq end (point)))
          (save-excursion
            (let ((mark-active nil))
              (narrow-to-region start end)
              (funcall fn)
              (widen)))))
      (set-marker end-marker nil)
      (set-marker next-line-marker nil))))

(global-set-key "\C-m" 'newline-and-indent)

(require 'hyde)

(defun find-file-at-point-with-line()
  "if file has an attached line num goto that line, ie boom.rb:12"
  (interactive)
  (setq line-num 0)
  (save-excursion
    (search-forward-regexp "[^ ]:" (point-max) t)
    (if (looking-at "[0-9]+")
        (setq line-num (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
  (find-file-at-point)
  (if (not (equal line-num 0))
      (goto-line line-num)))
