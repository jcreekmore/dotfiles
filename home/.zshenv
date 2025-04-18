if [[ -x /usr/libexec/path_helper ]]; then
	eval `/usr/libexec/path_helper -s`
fi

export PATH="/opt/homebrew/bin:$PATH"
PATH="$HOME/.bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
PATH="${PATH}:${HOME}/.krew/bin"
PATH="$HOME/.rbenv/bin:$PATH"

export MANPATH="/usr/local/man:$MANPATH"
MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"

export ACK_PAGER_COLOR='less -R'

if [[ $(uname -s) == "Darwin" ]]; then
    PATH="/opt/homebrew/opt/gnu-sed/libexec/gnubin:$PATH"

	export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:/usr/local/opt/openssl@3/lib/pkgconfig"
	export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:/usr/local/opt/zlib/lib/pkgconfig"
	export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:/usr/local/opt/sqlite/lib/pkgconfig"
	export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:/usr/local/opt/libffi/lib/pkgconfig"
    export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:/opt/homebrew/opt/mysql-client/lib/pkgconfig"

	export LDFLAGS="-L/usr/local/opt/openssl@3/lib"
	export LDFLAGS="$LDFLAGS -L/usr/local/opt/zlib/lib"
	export LDFLAGS="$LDFLAGS -L/usr/local/opt/sqlite/lib"
	export LDFLAGS="$LDFLAGS -L/usr/local/opt/libffi/lib"
    export LDFLAGS="$LDFLAGS -L/opt/homebrew/opt/mysql-client/lib"

	export CPPFLAGS="-I/usr/local/opt/openssl@3/include"
	export CPPFLAGS="$CPPFLAGS -I/usr/local/opt/zlib/include"
	export CPPFLAGS="$CPPFLAGS -I/usr/local/opt/sqlite/include"
	export CPPFLAGS="$CPPFLAGS -I/usr/local/opt/libffi/include"
    export CPPFLAGS="$CPPFLAGS -I/opt/homebrew/opt/mysql-client/include"

	export LIBRARY_PATH="$LIBRARY_PATH:/usr/local/lib"
fi

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/shims:$PATH"
    eval "$(pyenv init -)"
    export PYENV_GLOBAL_PYTHON3="$(pyenv prefix $(pyenv global))/bin/python3"
fi

if [[ -f ${HOME}/.zshenv.local ]]; then
	  source ${HOME}/.zshenv.local
fi

export BAT_THEME="Solarized (dark)"
export BAT_STYLE="numbers"

alias vi="nvim"
alias vim="nvim"
. "$HOME/.cargo/env"

alias files-to-prompt-multi="fd | fzf --highlight-line --multi --cycle --preview='bat --color=always {}' --track | xargs uvx files-to-prompt"
alias ftpm='files-to-prompt-multi | pbcopy'
