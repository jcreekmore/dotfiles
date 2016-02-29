if !isdirectory(expand('~/.vim/bundle/neobundle.vim'))
	call system('git clone https://github.com/Shougo/neobundle.vim.git
		\ ~/.vim/bundle/neobundle.vim')
endif

if has('vim_starting')
	if &compatible
		set nocompatible
	endif

	set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

let hostname = substitute(system("hostname -s"), '\n', '', '')

" Turn on NeoBundle
call neobundle#begin(expand('~/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
NeoBundleFetch 'Shougo/neobundle.vim'

" My Bundles here:
" Refer to |:NeoBundle-examples|.
NeoBundle 'rust-lang/rust.vim'
NeoBundle 'kergoth/vim-bitbake'
NeoBundle 'rking/ag.vim'
NeoBundle 'chun-yang/vim-action-ag'
if hostname == "Nebula"
	NeoBundle 'ledger/vim-ledger'
endif

call neobundle#end()

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

set nobackup		" do not keep a backup file, use versions instead
set history=50		" keep 50 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching

" For Win32 GUI: remove 't' flag from 'guioptions': no tearoff menu entries
" let &guioptions = substitute(&guioptions, "t", "", "g")

" Don't use Ex mode, use Q for formatting
map Q gq

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
  set mouse=a
endif

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif

" Install missing bundles
NeoBundleCheck

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  " Also don't do it when the mark is in the first line, that is the default
  " position when opening a file.
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  augroup END

  " Python files should get PEP8 white space settings
  autocmd BufNewFile,BufEnter,BufRead *.py set filetype=python
  autocmd FileType python set tabstop=4|set shiftwidth=4|set expandtab

  " C files should get our white space settings
  autocmd BufNewFile,BufEnter,BufRead *.c set filetype=c
  autocmd BufNewFile,BufEnter,BufRead *.h set filetype=c
  autocmd FileType c set tabstop=4|set shiftwidth=4|set expandtab

  " Markdown not Modula-2...
  autocmd BufNewFile,BufReadPost *.md set filetype=markdown

  " Highlight trailing whitespace
  highlight ExtraWhitespace ctermbg=red guibg=red
  autocmd InsertEnter * match ExtraWhitespace /\s\+%#\@<!$/
  autocmd InsertLeave,BufWinEnter * match ExtraWhitespace /\s\+$/

else

  set autoindent		" always set autoindenting on

endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif

" Terminals should always be dark. The light is blinding!
set bg=dark

" 4 space tabs FTW
set sw=4
set ts=4

set modeline
set modelines=5

let g:ledger_maxwidth=80
let g:ledger_fold_blanks=1

" Use relative line numbering
set number
set relativenumber

set exrc   " enables per-directory .vimrc files
set secure " disables unsafe commands in local .vimrc files

NeoBundleSource
