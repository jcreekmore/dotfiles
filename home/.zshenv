export PATH="/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin"
PATH="$HOME/.bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
PATH="$HOME/.cargo/bin:$PATH"
PATH="${PATH}:${HOME}/.krew/bin"
PATH="$HOME/.rbenv/bin:$PATH"

export MANPATH="/usr/local/man:$MANPATH"
MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"

if [[ -x /usr/libexec/path_helper ]]; then
	eval `/usr/libexec/path_helper -s`
fi

export ACK_PAGER_COLOR='less -R'

if [[ $(uname -s) == "Darwin" ]]; then
	PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

	export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:/usr/local/opt/openssl@3/lib/pkgconfig"
	export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:/usr/local/opt/zlib/lib/pkgconfig"
	export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:/usr/local/opt/sqlite/lib/pkgconfig"
	export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:/usr/local/opt/libffi/lib/pkgconfig"
	#export LEDGER_FILE="$HOME/Documents/ledger/journal.ledger"

	export LDFLAGS="-L/usr/local/opt/openssl@3/lib"
	export LDFLAGS="$LDFLAGS -L/usr/local/opt/zlib/lib"
	export LDFLAGS="$LDFLAGS -L/usr/local/opt/sqlite/lib"
	export LDFLAGS="$LDFLAGS -L/usr/local/opt/libffi/lib"

	export CPPFLAGS="-I/usr/local/opt/openssl@3/include"
	export CPPFLAGS="$CPPFLAGS -I/usr/local/opt/zlib/include"
	export CPPFLAGS="$CPPFLAGS -I/usr/local/opt/sqlite/include"
	export CPPFLAGS="$CPPFLAGS -I/usr/local/opt/libffi/include"

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

if [[ -e ~/.zshenv.local ]]; then
	  source ~/.zshenv.local
fi

export BAT_THEME="Solarized (dark)"
export BAT_STYLE="plain"

export PROJECT_PATHS=(~/src ~/work)

fif() {
  if [ ! "$#" -gt 0 ]; then echo "Need a string to search for!"; return 1; fi
  rg --files-with-matches --no-messages "$1" | fzf --preview "highlight -O ansi -l {} 2> /dev/null | rg --colors 'match:bg:yellow' --ignore-case --pretty --context 10 '$1' || rg --ignore-case --pretty --context 10 '$1' {}"
}

vfif() {
	filename=$(fif "$@")
	if [[ $filename != "" ]]; then
		vim -o "$filename"
	fi
}

alias gsledger="hledger -f $HOME/Documents/Girl\ Scouts/financial-report.ledger"
alias vi="nvim"
alias vim="nvim"
