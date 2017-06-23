export EDITOR=emacsclient
alias e="emacsclient -t"
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

function auto_complete_jump {
    local cur=${COMP_WORDS[COMP_CWORD]}
    local marks=$(find $MARKPATH -type l | awk -F '/' '{print $NF}')
    COMPREPLY=($(compgen -W '${marks[@]}' -- "$cur"))
    return 0
}
complete -o default -o nospace -F auto_complete_jump jump

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

# Git shortcuts
alias gir="git rebase -i origin/master"
alias gcm="git checkout master"
alias grom="git fetch && git rebase origin/master"
alias g-="git checkout @{-1}"
alias g--="git checkout @{-2}"

# Git auto completion
if [ -f ~/.git-completion.bash ]; then
  . ~/.git-completion.bash
fi

# The prompt. Show the Git branch, too.
export PS1="\W\[\033[32m\]\$(parse_git_branch)\[\033[00m\] $ "

if [ -f "$HOME/.bashrc" ]; then
  source $HOME/.bashrc
fi

case $- in
   *i*) source ~/.bashrc
esac

source ~/.profile
