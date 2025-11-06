" ===== Neovim 0.11.5 clean config (init.vim) =====

" If you have lua/basic.lua, load it
lua require("basic")

" Use your pyenv Python host
let g:python3_host_prog="$PYENV_GLOBAL_PYTHON3"

" ---------------- Plugins (vim-plug) ----------------
call plug#begin("~/.vim/plugged")
  " UI / colors
  Plug 'dracula/vim'
  Plug 'arcticicestudio/nord-vim'
  Plug 'nvim-tree/nvim-web-devicons'      " replaces ryanoasis/vim-devicons for nvim

  " Startup, utils
  Plug 'mhinz/vim-startify'
  Plug 'tpope/vim-fugitive'

  " File finding
  Plug 'nvim-lua/plenary.nvim'
  Plug 'nvim-telescope/telescope.nvim'

  " FZF (optional, keep if you use it)
  Plug 'junegunn/fzf'
  Plug 'junegunn/fzf.vim'

  " Treesitter
  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

  " LSP & tooling (native)
  Plug 'neovim/nvim-lspconfig'
  Plug 'mason-org/mason.nvim'
  Plug 'mason-org/mason-lspconfig.nvim'

  " Completion (nvim-cmp stack)
  Plug 'hrsh7th/nvim-cmp'
  Plug 'hrsh7th/cmp-nvim-lsp'
  Plug 'hrsh7th/cmp-path'
  Plug 'hrsh7th/cmp-buffer'
  Plug 'hrsh7th/vim-vsnip'
  Plug 'hrsh7th/cmp-vsnip'

  " TypeScript helper
  Plug 'pmizio/typescript-tools.nvim'

  " Syntax extras
  Plug 'honza/vim-snippets'
  Plug 'ekalinin/Dockerfile.vim'
  Plug 'vim-python/python-syntax'
  Plug 'hashivim/vim-terraform'
  Plug 'NoahTheDuke/vim-just'
call plug#end()

" Theme
colorscheme dracula
let g:python_highlight_all = 1

" ---------------- Movement & splits ----------------
" Move lines/blocks with Alt+j/k
inoremap <A-j> <Esc>:m .+1<CR>==gi
inoremap <A-k> <Esc>:m .-2<CR>==gi
vnoremap <A-j> :m '>+1<CR>gv=gv
vnoremap <A-k> :m '<-2<CR>gv=gv

" Move splits with Alt+hjkl
nnoremap <A-h> <C-W>H
nnoremap <A-j> <C-W>J
nnoremap <A-k> <C-W>K
nnoremap <A-l> <C-W>L

" Navigate splits with Ctrl+hjkl
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

let mapleader = ","

" ---------------- Completion behavior ----------------
set completeopt=menuone,noinsert,noselect
set shortmess+=c

" ---------------- Filetype & formatting ----------------
if has("autocmd")
  filetype plugin indent on

  augroup vimrcEx
    au!
    autocmd BufNewFile,BufEnter,BufRead *.py set filetype=python
    autocmd FileType python set tabstop=4|set shiftwidth=4|set expandtab|setlocal textwidth=88

    autocmd FileType text setlocal textwidth=78
    autocmd FileType markdown setlocal textwidth=78
    autocmd FileType gitcommit setlocal textwidth=72

    autocmd FileType json set tabstop=2|set shiftwidth=2|set expandtab
    autocmd FileType javascript set tabstop=2|set shiftwidth=2|set expandtab
    autocmd FileType javascript.jsx set tabstop=2|set shiftwidth=2|set expandtab
    autocmd FileType yaml set tabstop=2|set shiftwidth=2|set expandtab

    au BufNewFile,BufRead */ansible/roles/*/files/*.{automount,mount,path,service,socket,swap,target,timer} setf systemd
  augroup END
else
  set autoindent
endif

" Markdown fenced language hints
let g:vim_markdown_fenced_languages = ['rust=rust', 'json=json', 'diff=diff']

" Link an LSP semantic token to rustKeyword if you like
hi! link @lsp.type.keyword.rust rustKeyword

" --------- Lua block: LSP, diagnostics, telescope, treesitter ---------
"
lua<<EOF

-- ===== NATIVE LSP (0.11+) =====
-- Capabilities for nvim-cmp
local capabilities = require('cmp_nvim_lsp').default_capabilities()

-- Define per-server configs first
vim.lsp.config('pyright', {
  capabilities = capabilities,
})

vim.lsp.config('rust_analyzer', {
  capabilities = capabilities,
  settings = {
    ['rust-analyzer'] = {
      checkOnSave = { command = 'clippy' },
      cargo = { buildScripts = { enable = false }, targetDir = 'target-analyzer' },
      diagnostics = { enable = true },
    },
  },
})

-- Optional: TypeScript via typescript-tools (keeps its own setup)
require('typescript-tools').setup({})

-- Enable inlay hints toggle
vim.keymap.set('n', '<Leader>ih', function()
  local enabled = vim.lsp.inlay_hint.is_enabled and vim.lsp.inlay_hint.is_enabled(0)
  vim.lsp.inlay_hint.enable(0, not enabled)
end, { desc = 'Toggle inlay hints' })

-- Format Rust on save
local format_sync_grp = vim.api.nvim_create_augroup("Format", {})
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = "*.rs",
  callback = function() vim.lsp.buf.format({ async = false, timeout_ms = 500 }) end,
  group = format_sync_grp,
})

-- ===== Mason (updated org) =====
require('mason').setup()
require('mason-lspconfig').setup({
  ensure_installed = { 'pyright', 'rust_analyzer' },
  -- Auto-enable any installed servers using the new API
  handlers = {
    function(server) vim.lsp.enable(server) end,
  },
})

-- ===== Telescope =====
require('telescope').setup({
  defaults = {
    mappings = {
      i = { ["<C-h>"] = "which_key" }
    }
  }
})
local builtin = require('telescope.builtin')
vim.keymap.set('n', '<Leader>ff', builtin.find_files, {})
vim.keymap.set('n', '<Leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<Leader>fb', function() builtin.buffers({ sort_lastused = true, ignore_current_buffer = true }) end, { desc = "Switch buffers" })
vim.keymap.set('n', '<Leader>fh', builtin.help_tags, {})
vim.keymap.set('n', '<Leader>fr', builtin.lsp_references, {})
vim.keymap.set('n', '<Leader>fi', builtin.lsp_implementations, {})
vim.keymap.set('n', '<Leader>fd', builtin.lsp_definitions, {})
vim.keymap.set('n', '<Leader>fs', function() builtin.lsp_document_symbols({symbol_width=50, symbols={"method","function"}}) end, {})
vim.keymap.set('n', '<C-p>', builtin.git_files, {})

-- ===== Auto-root (vim.fs) =====
local root_names = { '.git', 'Makefile' }
local root_cache = {}
local function set_root()
  local path = vim.api.nvim_buf_get_name(0)
  if path == '' then return end
  path = vim.fs.dirname(path)
  local root = root_cache[path]
  if root == nil then
    local root_file = vim.fs.find(root_names, { path = path, upward = true })[1]
    if root_file == nil then return end
    root = vim.fs.dirname(root_file)
    root_cache[path] = root
  end
  vim.fn.chdir(root)
end
local root_augroup = vim.api.nvim_create_augroup('MyAutoRoot', {})
vim.api.nvim_create_autocmd('BufEnter', { group = root_augroup, callback = set_root })

-- ===== Treesitter =====
require('nvim-treesitter.configs').setup {
  highlight = { enable = true, additional_vim_regex_highlighting = false },
}
EOF

