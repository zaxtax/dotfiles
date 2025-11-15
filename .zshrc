# The following lines were added by compinstall
zstyle :compinstall filename '/home/z/.zshrc'

zstyle ':completion:*' completer _complete
zstyle ':completion:*' menu select=1
# End of lines added by compinstall

fpath+=/home/z/.zsh_completions/conda-zsh-completion

autoload -Uz compinit
compinit

zstyle ":conda_zsh_completion:*" use-groups true

# source ~/upstream/fzf-tab/fzf-tab.plugin.zsh
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${GUROBI_HOME}/lib"
export PATH=/sbin:/usr/sbin:$PATH

# clojure, local, android, haskell
export PATH=/home/z/.lein/bin:/home/z/.local/bin:/home/z/upstream/android-sdk-linux/tools:/home/z/.cabal/bin:/home/z/bin:$PATH

# go
export GOROOT=$HOME/upstream/go
export GOPATH=$HOME/upstream/go-work
export PATH=$GOROOT/bin:$GOPATH/bin:$PATH

# rust
export PATH=$HOME/.cargo/bin:$PATH
export RUST_SRC_PATH=$HOME/upstream/rust/src

# elm
# export PATH=$HOME/upstream/elm-platform/installers/Elm-Platform/0.15.1/.cabal-sandbox/bin:$PATH

# pyenv
# export PATH="/home/z/.pyenv/bin:$PATH"
# eval "$(pyenv init -)"
# eval "$(pyenv virtualenv-init -)"

# make the prompt look like [user@host /dir] $
RCMD_CMD=ssh;   export RCMD_CMD
export PAGER=most
export VISUALWORKS=/home/z/vw7.6nc
export BROWSER=firefox

if [ -z "$GPG_AGENT_INFO" ] ; then
     if [ -f $HOME/.gpg-agent-info ] ; then
         export GPG_AGENT_INFO=$(cat $HOME/.gpg-agent-info)
     fi
fi
export GPG_TTY=$(tty)

# wmname LG3D

alias mathematica='mathematica -defaultvisual'
alias ls='ls --color'
alias todo="emacs -batch -l ~/.emacs -eval '(org-batch-agenda \"t\")' 2> /dev/null "
alias today="emacs -batch -l ~/.emacs -eval '(org-batch-agenda \"a\")' 2> /dev/null "
alias cclip='xclip -selection clipboard'

alias icat='kitty +kitten icat'
alias idot='dot -Tpng -Nfontcolor=white -Ncolor=white -Nbgcolor=green -Gbgcolor=transparent -Ecolor=white | icat'


. /usr/lib/git-core/git-sh-prompt
GIT_PS1_SHOWCOLORHINTS=1
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1

function git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo " (${ref#refs/heads/})"
}

function pixi_activate() {
    # default to current directory if no path is given
    local manifest_path="${1:-.}"
    eval "$(pixi shell-hook --manifest-path $manifest_path)"
}

export MARKPATH=$HOME/.marks
function jump { 
    cd -P $MARKPATH/$1 2>/dev/null || echo "No such mark: $1"
}
function mark { 
    mkdir -p $MARKPATH; ln -s "$(pwd)" $MARKPATH/$1
}
function unmark { 
    rm -i $MARKPATH/$1 
}
function marks {
    ls -l $MARKPATH | sed 's/  / /g' | cut -d' ' -f9- | sed 's/ -/\t-/g' && echo
}

function _marks {
      reply=($(ls $MARKPATH))
}

compctl -K _marks jump
compctl -K _marks unmark

function _mview {
      reply=($(ls ~/.mutt/accounts | sed 's/\.rc//g'))
}

function mview {
    mutt -F ~/.mutt/accounts/"$1".rc
}

compctl -K _mview mview
#compctl -k "(usc iu rzgmail cfgmail)" mview

 
# if we're in an xterm, then we can actually set the xterm titlebar
# to have this info also. except we use the full directory path in
# the titlebar, since we have lots of room
case $TERM in
   xterm*)
       # chpwd executes a command every time the directory changes
       # print is like echo, but with access to prompt expansion variables
       chpwd () {print -Pn "\e]0;%n@%m %d\a"}
       ;;
esac

precmd () {
  PS1='[$PIXI_PROMPT%F{green}%n@%m%f %F{blue}%1d%f$(__git_ps1 " (%s)")]%# ';
  [[ -t 1 ]] || return
  case $TERM in
    *xterm*|rxvt|urxvt|rxvt-unicode|(dt|k|E|a)term) print -Pn "\e]2;%n@%m:%1d\a"
    ;;
    screen*) print -Pn "\e\"%n@%m:%1d\e\134"
  esac
}

setopt prompt_subst
setopt interactivecomments
autoload colors zsh/terminfo
colors

function cabal_sandbox_info() {
    cabal_files=(*.cabal(N))
    if [ $#cabal_files -gt 0 ]; then
        if [ -f cabal.sandbox.config ]; then
            echo "%{$fg[green]%}sandboxed%{$reset_color%}"
        fi
    fi
}
 
RPROMPT="\$(cabal_sandbox_info) $RPROMPT"

export HISTFILE=~/.zsh_history
export HISTSIZE=5000000
export SAVEHIST=5000000

export R_HISTFILE=~/.Rhistory

export IPP=/home/z/intel/ipp/6.1.2.051/ia32
export EASYVISION=/home/z/upstream/easyVision
export LD_LIBRARY_PATH=$IPP/sharedlib

bindkey '^[[A' history-search-backward
bindkey '^[[B' history-search-forward
bindkey '^[[C' forward-char 
bindkey '^[[D' backward-char

setopt correct \
    appendhistory \
    incappendhistory \
    histverify \
    auto_pushd \
    pushd_silent \
    pushdminus \
    pushd_ignore_dups \
    share_history \
    complete_aliases
# OPAM configuration
. /home/z/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# n and node
export N_PREFIX=/home/z/upstream/n
export PATH=$N_PREFIX/bin:$PATH

alias wi=". ~/.local/bin/workonwrapper.sh"
alias wo="exit"

compdef _doctl doctl

# >>> juliaup initialize >>>

# !! Contents within this block are managed by juliaup !!

path=('/home/z/.juliaup/bin' $path)
export PATH

# <<< juliaup initialize <<<


export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
export PATH="/home/z/.pixi/bin:$PATH"

if [[ -v PIXI_WI_ENV ]]; then
    pixi_activate $PIXI_WI_ENV
fi

source /home/z/.config/broot/launcher/bash/br

[ -f "/home/z/.ghcup/env" ] && . "/home/z/.ghcup/env" # ghcup-env
