export EDITOR=emacsclient
export PATH=/usr/local/bin:$PATH
export HISTSIZE="" #infinite history!

# Marking directories for quick navigation
export MARKPATH=$HOME/.marks
function jump {
    cd -P $MARKPATH/$1 2>/dev/null || echo "No such mark: $1"
}
function mark {
    mkdir -p $MARKPATH; ln -s $(pwd) $MARKPATH/$1
}
function unmark {
    rm -i $MARKPATH/$1
}
function marks {
    \ls -l "$MARKPATH" | tail -n +2 | sed 's/  / /g' | cut -d' ' -f9- | awk -F ' -> ' '{printf "%-10s -> %s\n", $1, $2}'
}

# ========
#   GIT
# ========
parse_git_branch() {
  if [ "$1" = "true" ]; then
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/' -e 's/.*(\(.*\)).*/\1/'
  else
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
  fi
}

# Git magic
alias gir="git rebase -i origin/develop"
alias gcm="git checkout develop"
alias grom="git rebase origin/develop"

# Git auto completion
if [ -f `brew --prefix`/etc/bash_completion ]; then
    . `brew --prefix`/etc/bash_completion
fi

# GitHUB stuff
eval "$(hub alias -s)"

# The prompt. Show the Git branch, too.
export PS1="\W\[\033[32m\]\$(parse_git_branch)\[\033[00m\] $ "

if [ -f "$HOME/.bashrc" ]; then
  source $HOME/.bashrc
fi

case $- in
   *i*) source ~/.bashrc
esac

source ~/.profile

if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
