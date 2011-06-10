# The following lines were added by compinstall
zstyle :compinstall filename '~/.zshrc'

zstyle ':completion:*' completer _complete
zstyle ':completion:*' menu select=1
autoload -Uz compinit
compinit
# End of lines added by compinstall

# make the prompt look like [user@host /dir] $
RCMD_CMD=ssh;   export RCMD_CMD
export PAGER=most

alias mathematica='mathematica -defaultvisual'
alias ls='ls --color'

git_prompt_info() {
  ref=$(git-symbolic-ref HEAD 2> /dev/null) || return
  echo " (${ref#refs/heads/})"
}

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
  PS1="[%n@%m %1d$(git_prompt_info)]%# ";
  [[ -t 1 ]] || return
  case $TERM in
    *xterm*|rxvt|urxvt|rxvt-unicode|(dt|k|E|a)term) print -Pn "\e]2;%n@%m:%~\a"
    ;;
    screen*) print -Pn "\e\"%n@%m:%~\e\134"
  esac
}

# This sets the window title to the last run command.
[[ -t 1 ]] || return
case $TERM in
  *xterm*|rxvt|(dt|k|E|a)term)
    preexec () {
      print -Pn "\e]2;$1\a"
    }
  ;;
  screen*)
    preexec () {
      print -Pn "\e\"$1\e\134"
    }
  ;;
esac

export HISTFILE=~/.zsh_history
export HISTSIZE=50000
export SAVEHIST=50000

export IPP=/home/zv/intel/ipp/6.1.2.051/ia32
export EASYVISION=/home/zv/custom_builds/easyVision
export LD_LIBRARY_PATH=$IPP/sharedlib
export CUDA=/home/zv/custom_builds/CUDA

bindkey '^[[A' history-search-backward
bindkey '^[[B' history-search-forward
bindkey '^[[C' forward-char 
bindkey '^[[D' backward-char

setopt correct \
    appendhistory \
    histverify \
    auto_pushd \
    pushd_silent \
    pushd_ignore_dups