source "$HOME/.homesick/repos/homeshick/homeshick.sh"
fpath=($HOME/.homesick/repos/homeshick/completions $fpath)
fpath=(/usr/local/share/zsh/site-functions $fpath)
fpath=($HOME/.config/zsh $fpath)

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
export ZSH_CUSTOM_PLUGINS=$ZSH/custom/plugins

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="gnzh"
#ZSH_THEME="agnoster"
DEFAULT_USER="jonathan"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

plugins=(git git-extras fzf httpie gh starship direnv uv pyenv nvm rvm rust)

if [[ -d $ZSH_CUSTOM_PLUGINS/zsh-autosuggestions ]]; then
	zmodload zsh/zpty
	ZSH_AUTOSUGGEST_STRATEGY=(history completion)
	ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=white'
	plugins+=(zsh-autosuggestions)
fi

if [[ -d $ZSH_CUSTOM_PLUGINS/zsh-history-substring-search ]]; then
	plugins+=(zsh-history-substring-search)
fi

if [[ -d $ZSH_CUSTOM_PLUGINS/zsh-syntax-highlighting ]]; then
	plugins+=(zsh-syntax-highlighting)
fi

if [[ $(uname -s) == "Darwin" ]]; then
    plugins+=(brew)
fi

# User configuration

source $ZSH/oh-my-zsh.sh

export EDITOR='nvim'

if [[ $(uname -s) == "Linux" ]]; then
	alias open='xdg-open'
fi

export LESS='-XFR'

case "$TERM" in
	"dumb")
		PS1="> "
		unset zle_bracketed_paste
		;;
esac

if [[ -n "$TMUX" ]]; then
	function refresh_ssh {
		auth_sock=$(tmux show-environment | grep "^SSH_AUTH_SOCK")
		if [[ -n "${auth_sock}" ]]; then
			export ${auth_sock}
		fi

		display=$(tmux show-environment | grep "^DISPLAY")
		if [[ -n "${display}" ]]; then
			export ${display}
		fi
	}
else
  function refresh_ssh { }
fi

function preexec {
	refresh_ssh
}

type exa >/dev/null 2>&1
if [[ $? -eq 0 ]]; then
	alias ls=exa
fi

type bat >/dev/null 2>&1
if [[ $? -eq 0 ]]; then
	alias cat=bat
fi

type rg >/dev/null 2>&1
if [[ $? -ne 0 ]]; then
	function rg {
		grep -R "$@" *
	}
fi

if [[ -e ~/.zshrc.local ]]; then
	source ~/.zshrc.local
fi

git-review () {
    local repo
    repo=${1:=$(git_develop_branch)}

    git branch | sed 's/^[* ] //' | grep -q -E "^${repo}$"

    if [[ $? -ne 0 ]]; then
        repo=$(git_main_branch)
    fi

    command git --no-pager log --reverse --oneline ${repo}.. | nl
}

git-revshow () {
    commit=$1
    repo=${2:=$(git_develop_branch)}

    git branch | sed 's/^[* ] //' | grep -q -E "^${repo}$"

    if [[ $? -ne 0 ]]; then
        repo=$(git_main_branch)
    fi

    git-review ${repo} | head -n ${commit} | tail -n 1 | awk '{print $2}' | xargs git show
}

git-revshow-stat () {
    commit=$1
    repo=${2:=$(git_develop_branch)}

    git branch | sed 's/^[* ] //' | grep -q -E "^${repo}$"

    if [[ $? -ne 0 ]]; then
        repo=$(git_main_branch)
    fi

    git-review ${repo} | head -n ${commit} | tail -n 1 | awk '{print $2}' | xargs git show --stat
}

feature-branch () {
    name=${1:="issue$(gh issue list --author "@me" -L 1 --json number --jq '.[].number')"}
    base=${2:=$(git_develop_branch)}

    git branch | sed 's/^[* ] //' | grep -q -E "^${base}$"

    if [[ $? -ne 0 ]]; then
        base=$(git_main_branch)
    fi

    git checkout -b jcreekmore/${name} ${base}
}

git-switch-branch () {
    branch=${1:=$(git branch | grep -v "^*" | tr -d ' ' | fzf --cycle --ansi --preview "git show --color --pretty='format:%Cgreen%s%Creset%n%n%b' -s {}")}

    if [[ ! -z ${branch} ]]; then 
        git switch ${branch}
    fi
}

git-fixup () {
    commit=$1
    repo=${2:=$(git_develop_branch)}

    git branch | sed 's/^[* ] //' | grep -q -E "^${repo}$"

    if [[ $? -ne 0 ]]; then
        repo=$(git_main_branch)
    fi

    git-review ${repo} | head -n ${commit} | tail -n 1 | awk '{print $2}' | xargs git commit --fixup
}

git-autosquash () {
    repo=${1:=$(git_develop_branch)}

    git branch | sed 's/^[* ] //' | grep -q -E "^${repo}$"

    if [[ $? -ne 0 ]]; then
        repo=$(git_main_branch)
    fi

    git rebase --interactive --autosquash ${repo}
}

alias grv='git-review'
alias grs='git-revshow'
alias grst='git-revshow-stat'
alias gcfix='git-fixup'
alias grbia='git-autosquash'
alias gsw='git-switch-branch'

if [[ $(uname -s) == "Darwin" ]]; then
    export LIBTORCH=$(brew --prefix pytorch)
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[[ $TERM == "dumb" ]] && unsetopt zle && PS0='$ ' && return

# Enable keychain
type -p keychain 2>&1 > /dev/null
if [ $? -eq 0 ]; then
    # find keys that start with id but don't end in .pub
    local keyfiles=$(find $HOME/.ssh/ -name 'id*' -a ! -name '*.pub')
    eval $(keychain --eval --agents gpg,ssh --inherit any-once "${keyfiles}" -Q -q)
    unset keyfiles
fi

type -p brew 2>&1 > /dev/null
if [ $? -eq 0 ]; then
    export LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix)/lib"
fi

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
test -e "${HOME}/.config/op/plugins.sh" && source "${HOME}/.config/op/plugins.sh"
