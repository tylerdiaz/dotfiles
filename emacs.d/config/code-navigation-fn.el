(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (forward-line (read-number "Goto line: ")))
    (linum-mode -1)))

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
      (forward-line line-num)))

(defun td/current-buffer-extension()
  (when (buffer-file-name)
    (file-name-extension (buffer-file-name))))
