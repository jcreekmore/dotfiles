export PATH="$HOME/.cabal/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
PATH="$HOME/.bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
PATH="$HOME/.cargo/bin:$PATH"

export MANPATH="/usr/local/man:$MANPATH"
MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"

if [[ -x /usr/libexec/path_helper ]]; then
	eval `/usr/libexec/path_helper -s`
fi

export ACK_PAGER_COLOR='less -R'
