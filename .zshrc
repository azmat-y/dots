# lines configured by zsh-newuser-install
bindkey -e
# end of lines configured by zsh-newuser-install
# the following lines were added by compinstall
zstyle :compinstall filename '/home/azmat/.zshrc'
 
autoload -Uz compinit
compinit
# end of lines added by compinstall
 
# case insensistive tab completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' menu select 

# user prefrences
HISTSIZE=5000
HISTFILE=~/.zsh_history
SAVEHIST=5000
HISTDUP=erase
setopt appendhistory
setopt sharehistory
setopt incappendhistory
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups
 
# aliases
alias egrep='grep -E --color=auto'
alias fgrep='grep -F --color=auto'
alias grep='grep --color=auto'
alias ll='ls -l --color=auto'
alias ls='ls --color=auto'
alias which='alias | /usr/bin/which --tty-only --read-alias --show-tilde --show-dot'
alias xzegrep='xzegrep --color=auto'
alias xzfgrep='xzfgrep --color=auto'
alias xzgrep='xzgrep --color=auto'
alias zegrep='zegrep --color=auto'
alias zfgrep='zfgrep --color=auto'
alias zgrep='zgrep --color=auto'
alias off='systemctl poweroff'
alias bat0='upower --enumerate | rg battery | xargs upower -i' #Battery report command

# syntax highlighting
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
# auto suggestion
source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh
 
# dotfile bare repo
alias dotgit='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

# install zoxide;
eval "$(zoxide init --cmd cd zsh)"
# show execution time on right
function preexec() {
  timer=${timer:-$SECONDS}
}
 
function precmd() {
  if [ $timer ]; then
    timer_show=$(($SECONDS - $timer))
    export RPROMPT="%F{green}${timer_show}s %{$reset_color%}"
    unset timer
  fi
}

 
# recongnising ctrl + arrow keys
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word

# configuring the Delete key
bindkey "^[[3~" delete-char

# install direnv
eval "$(direnv hook zsh)"
eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
# init ohmyposh
# install homebrew from there install ohmyposh
eval "$(
oh-my-posh init zsh --config \
	  /home/azmat/.config/ohmyposh/zen.toml 
)"

# man setting
export MAN_POSIXLY_CORRECT=1

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/azmat/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/azmat/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/azmat/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/azmat/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<


# poetry auto completion
fpath+=~/.zfunc
autoload -Uz compinit && compinit

# when in distrobox
if (env | grep -Fqi 'DISTROBOX'); then
        PS1='\u@\h:\w\$'
fi

# gitignore command line tool
function gi() { curl -sLw "\n" https://www.toptal.com/developers/gitignore/api/$@ ;}
source <(fzf --zsh)

# subword mode for navigating line
WORDCHARS=''

# For some reason dnd remains on by default, so disable it
swaync-client -df
