A place to keep my important files.

There's a separate [snippets repository](https://github.com/tylerdiaz/snippets) for my emacs. I use [mkrc](https://thoughtbot.github.io/rcm/mkrc.1.html) to facilitate symlinking.

To setup: 
```bash
cd ~
git clone https://github.com/tylerdiaz/dotfiles
rcup -d dotfiles -x README.md
cd dotfiles
source ./osx_setup
```

Some handy snippets:
* To get a list of activated packages on Emacs run `M-:` and paste:
```elisp
(insert (format "%s" (delq nil (delete-dups package-activated-list))))
```
