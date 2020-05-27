
source "$HOME/.homesick/repos/homeshick/homeshick.sh"
fpath=($HOME/.homesick/repos/homeshick/completions $fpath)
fpath=(/usr/local/share/zsh/site-functions $fpath)
fpath=(~/.config/taskquor $fpath)

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

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git git-extras fzf)

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

# User configuration

# export PATH="$HOME/.cabal/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi
export EDITOR='vim'

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
#

# added by travis gem
[ -f $HOME/.travis/travis.sh ] && source $HOME/.travis/travis.sh

alias grv='git review'
alias grs='git revshow'

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

function remove-kernel {
	if [[ -z "$1" ]]; then
		echo "Missing kernel version number"
		return 1
	fi

	VERS=$1
	sudo dpkg --purge linux-headers-${VERS} linux-headers-${VERS}-generic linux-image-${VERS}-generic linux-image-extra-${VERS}-generic
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

alias vi=vim

if [[ $(uname -s) == "Darwin" ]]; then
	OPENSSL_PREFIX=$(brew --prefix openssl)
	export OPENSSL_INCLUDE_DIR=${OPENSSL_PREFIX}/include
	export OPENSSL_LIB_DIR=${OPENSSL_PREFIX}/lib
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

type starship >/dev/null 2>&1
if [[ $? -eq 0 ]]; then
	eval $(starship init zsh)
fi

# Enable keychain
type -p keychain 2>&1 > /dev/null
if [ $? -eq 0 ]; then
    # find keys that start with id but don't end in .pub
    local keyfiles=$(find $HOME/.ssh/ -name 'id*' -a ! -name '*.pub')
    eval $(keychain --eval --agents gpg,ssh --inherit any-once "${keyfiles}")
    unset keyfiles
fi

function hist {
	num=${1:-20}
	history | tail -n $num
}
