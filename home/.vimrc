if !isdirectory(expand('~/.cache/dein/repos/github.com/Shougo/dein.vim'))
	call system('git clone https://github.com/Shougo/dein.vim.git
		\ ~/.cache/dein/repos/github.com/Shougo/dein.vim')
endif

if has('vim_starting')
	if &compatible
		set nocompatible
	endif
endif

"dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=~/.cache/dein/repos/github.com/Shougo/dein.vim
set runtimepath+=/usr/local/opt/fzf

" Required:
if dein#load_state('~/.cache/dein')
  call dein#begin('~/jonathan/.cache/dein')

  " Let dein manage dein
  " Required:
  call dein#add('~/.cache/dein/repos/github.com/Shougo/dein.vim')

  " call dein#add('vim-syntastic/syntastic')
  " call dein#add('preservim/nerdtree')
  call dein#add('rust-lang/rust.vim')
  call dein#add('ledger/vim-ledger')
  call dein#add('pangloss/vim-javascript')
  call dein#add('mxw/vim-jsx')
  " call dein#add('ctrlpvim/ctrlp.vim')

  " Required:
  call dein#end()
  call dein#save_state()
endif

" Required:
filetype plugin indent on
syntax enable

" If you want to install not installed plugins on startup.
if dein#check_install()
  call dein#install()
endif

"End dein Scripts-------------------------

let hostname = substitute(system("hostname -s"), '\n', '', '')

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
" if has('mouse')
"  set mouse=a
" endif

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
  set guifont=Hack-Regular:h14
endif
colorscheme solarized

set colorcolumn=+0

" Install missing bundles
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

  " For all git commit messages, set 'textwidth' to 72 characters.
  autocmd FileType gitcommit setlocal textwidth=72

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
  autocmd FileType python set tabstop=4|set shiftwidth=4|set expandtab|setlocal textwidth=79

  " C files should get our white space settings
  autocmd BufNewFile,BufEnter,BufRead *.c set filetype=c
  autocmd BufNewFile,BufEnter,BufRead *.h set filetype=c
  autocmd FileType c set tabstop=4|set shiftwidth=4|set expandtab

  " Markdown not Modula-2...
  autocmd BufNewFile,BufReadPost *.md set filetype=markdown
  autocmd FileType markdown set tabstop=4|set shiftwidth=4|set expandtab|setlocal textwidth=79

  " Html tag files
  autocmd FileType html set tabstop=4|set shiftwidth=4|set expandtab
  autocmd FileType htmldjango set tabstop=4|set shiftwidth=4|set expandtab

  " Highlight trailing whitespace
  highlight ExtraWhitespace ctermbg=red guibg=red
  autocmd InsertEnter * match ExtraWhitespace /\s\+%#\@<!$/
  autocmd InsertLeave,BufWinEnter * match ExtraWhitespace /\s\+$/

  " Cucumber (behave) files should use spaces
  autocmd FileType cucumber set tabstop=4|set shiftwidth=4|set expandtab

  " Json tab-twiddler
  autocmd FileType json set tabstop=2|set shiftwidth=2|set expandtab

  " Javascript tab-twiddler
  autocmd FileType javascript set tabstop=2|set shiftwidth=2|set expandtab

  " Rust files
  autocmd FileType rust setlocal makeprg=cargo|setlocal errorformat=%f:%l:%c:%m

  " Haskell files
  autocmd FileType haskell set tabstop=4|set shiftwidth=4|set expandtab

  au BufRead,BufNewFile *.tag :set filetype=html
  autocmd FileType html set tabstop=2|set shiftwidth=2|set expandtab
  au BufRead,BufNewFile *.json :set filetype=javascript
  autocmd FileType javascript set tabstop=2|set shiftwidth=2|set expandtab
  autocmd FileType javascript.jsx set tabstop=2|set shiftwidth=2|set expandtab

  autocmd FileType sh set tabstop=4|set shiftwidth=4|set expandtab

  autocmd FileType ledger set tabstop=2|set shiftwidth=2|set expandtab

  " autocmd StdinReadPre * let s:std_in=1
  " autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
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
let g:ledger_bin="hledger"

" Use relative line numbering
set number
set relativenumber

set exrc   " enables per-directory .vimrc files
set secure " disables unsafe commands in local .vimrc files

let mapleader = ","

nnoremap <C-o> :call fzf#run(fzf#wrap({'source': 'git ls-files'}))<CR>
nnoremap <C-p> :FZF<CR>

"set clipboard=unnamed
set mouse=

set wildmode=longest,list,full
set wildmenu

if executable("rg")
    set grepprg=rg\ --vimgrep\ --no-heading
    set grepformat=%f:%l:%c:%m,%f:%l:%m
endif

set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

"let g:syntastic_always_populate_loc_list = 1
"let g:syntastic_auto_loc_list = 1
"let g:syntastic_check_on_open = 1
"let g:syntastic_check_on_wq = 0

"let g:syntastic_python_checkers = ['flake8']
