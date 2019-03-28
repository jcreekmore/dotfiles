export PATH="$HOME/.cabal/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin"
PATH="$HOME/.bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
PATH="$HOME/.cargo/bin:$PATH"

export MANPATH="/usr/local/man:$MANPATH"
MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"

if [[ -x /usr/libexec/path_helper ]]; then
	eval `/usr/libexec/path_helper -s`
fi

export ACK_PAGER_COLOR='less -R'

if [[ $(uname -s) == "Darwin" ]]; then
	PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

	export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:/usr/local/opt/openssl/lib/pkgconfig"
	export LEDGER_FILE="$HOME/Documents/Financial/master.ledger"

	export OPENSSL_INCLUDE_DIR=`brew --prefix openssl`/include
	export OPENSSL_LIB_DIR=`brew --prefix openssl`/lib

	export LDFLAGS=-L/usr/local/opt/openssl/lib
	export CPPFLAGS=-I/usr/local/opt/openssl/include

	export LIBRARY_PATH="$LIBRARY_PATH:/usr/local/lib"
	export PYENV_ROOT="$HOME/.pyenv"
	export PATH="$PYENV_ROOT/bin:$PATH"
	if command -v pyenv 1>/dev/null 2>&1; then
		eval "$(pyenv init -)"
	fi
fi

if [[ $(uname -s) == "Linux" ]]; then
	alias x="sudo xl"
	alias xls="x list"
	alias xi="x info"
	alias xdm="x dmesg"

	PATH="$HOME/hypervisors/scripts/:$PATH"
fi

if [[ -e ~/.zshenv.local ]]; then
	  source ~/.zshenv.local
fi

export BAT_THEME="Solarized (dark)"
export BAT_STYLE="plain"
