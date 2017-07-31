# The following lines were added by compinstall
zstyle :compinstall filename '/home/zv/.zshrc'

zstyle ':completion:*' completer _complete
zstyle ':completion:*' menu select=1
autoload -Uz compinit
compinit
# End of lines added by compinstall
export GUROBI_HOME="/home/zv/upstream/gurobi550/linux64"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${GUROBI_HOME}/lib"
export PATH=/sbin:/usr/sbin:$PATH

# clojure, local, android, maple, haskell
export PATH=/home/zv/.lein/bin:/home/zv/.local/bin:/home/zv/upstream/android-sdk-linux/tools:/home/zv/Desktop/maple2016/bin:/home/zv/.cabal/bin:/opt/ghc/bin:$PATH

# perl 6
export PATH=/home/zv/.rakudobrew/bin:$PATH

export GRB_LICENSE_FILE=/home/zv/upstream/gurobi500/gurobi.lic
# export MATLAB_JAVA=/usr/lib/jvm/java-1.6.0-openjdk-1.6.0.0/jre

# go
export GOROOT=$HOME/upstream/go
export GOPATH=$HOME/upstream/go-work
export PATH=$GOROOT/bin:$GOPATH/bin:$PATH

# rust
export PATH=$HOME/.cargo/bin:$PATH
export RUST_SRC_PATH=$HOME/upstream/rust/src

# elm
export PATH=$HOME/upstream/elm-platform/installers/Elm-Platform/0.15.1/.cabal-sandbox/bin:$PATH

# hakaru
export LOCAL_MAPLE="`which maple`"

# make the prompt look like [user@host /dir] $
RCMD_CMD=ssh;   export RCMD_CMD
export PAGER=most
export VISUALWORKS=/home/zv/vw7.6nc
export BROWSER=google-chrome

if [ -z "$GPG_AGENT_INFO" ] ; then
     if [ -f $HOME/.gpg-agent-info ] ; then
         export GPG_AGENT_INFO=$(cat $HOME/.gpg-agent-info)
     fi
fi
export GPG_TTY=$(tty)

alias mathematica='mathematica -defaultvisual'
alias ls='ls --color'
alias todo="emacs -batch -l ~/.emacs -eval '(org-batch-agenda \"t\")' 2> /dev/null "
alias today="emacs -batch -l ~/.emacs -eval '(org-batch-agenda \"a\")' 2> /dev/null "

function git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo " (${ref#refs/heads/})"
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
  PS1='[%n@%m %1d$(git_prompt_info)]%# ';
  [[ -t 1 ]] || return
  case $TERM in
    *xterm*|rxvt|urxvt|rxvt-unicode|(dt|k|E|a)term) print -Pn "\e]2;%n@%m:%~\a"
    ;;
    screen*) print -Pn "\e\"%n@%m:%~\e\134"
  esac
}

[[ -s "/home/zv/.rvm/scripts/rvm" ]] && source "/home/zv/.rvm/scripts/rvm"

setopt prompt_subst
setopt interactivecomments
autoload colors zsh/terminfo
colors

# export RPS1='$(__git_prompt)'

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
export HISTSIZE=500000
export SAVEHIST=500000

export R_HISTFILE=~/.Rhistory

export IPP=/home/zv/intel/ipp/6.1.2.051/ia32
export EASYVISION=/home/zv/upstream/easyVision
export LD_LIBRARY_PATH=$IPP/sharedlib
export CUDA=/home/zv/upstream/CUDA

bindkey '^[[A' history-search-backward
bindkey '^[[B' history-search-forward
bindkey '^[[C' forward-char 
bindkey '^[[D' backward-char

setopt correct \
    appendhistory \
    histverify \
    auto_pushd \
    pushd_silent \
    pushd_ignore_dups \
    share_history

# OPAM configuration
. /home/zv/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/zv/.sdkman"
[[ -s "/home/zv/.sdkman/bin/sdkman-init.sh" ]] && source "/home/zv/.sdkman/bin/sdkman-init.sh"
