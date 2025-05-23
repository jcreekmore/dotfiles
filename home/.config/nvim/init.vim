lua require("basic")

let g:python3_host_prog="$PYENV_GLOBAL_PYTHON3"

call plug#begin("~/.vim/plugged")
 " Plugin Section
" Plug 'williamboman/mason.nvim'    
" Plug 'williamboman/mason-lspconfig.nvim'
 Plug 'dracula/vim'
 " Plug 'SirVer/ultisnips'
 Plug 'honza/vim-snippets'
 Plug 'mhinz/vim-startify'
 Plug 'neoclide/coc.nvim', {'branch': 'release'}
 Plug 'NoahTheDuke/vim-just'

 Plug 'nvim-treesitter/nvim-treesitter'

 " Collection of common configurations for the Nvim LSP client
 Plug 'neovim/nvim-lspconfig'

 " Completion framework
 Plug 'hrsh7th/nvim-cmp'

 " LSP completion source for nvim-cmp
 Plug 'hrsh7th/cmp-nvim-lsp'

 " Snippet completion source for nvim-cmp
 Plug 'hrsh7th/cmp-vsnip'

 " Other usefull completion sources
 Plug 'hrsh7th/cmp-path'
 Plug 'hrsh7th/cmp-buffer'

 " See hrsh7th's other plugins for more completion sources!

 " To enable more of the features of rust-analyzer, such as inlay hints and more!
 Plug 'simrat39/rust-tools.nvim'

 " Snippet engine
 Plug 'hrsh7th/vim-vsnip'

 " Fuzzy finder
 " Optional
 Plug 'nvim-lua/popup.nvim'
 Plug 'nvim-lua/plenary.nvim'
 Plug 'nvim-telescope/telescope.nvim'

 " Color scheme used in the GIFs!
 Plug 'arcticicestudio/nord-vim'
 Plug 'williamboman/nvim-lsp-installer'
 Plug 'ekalinin/Dockerfile.vim'
 Plug 'vim-python/python-syntax'
 Plug 'hashivim/vim-terraform'

 Plug 'ryanoasis/vim-devicons'
 Plug 'junegunn/fzf'
 Plug 'junegunn/fzf.vim'
 Plug 'tpope/vim-fugitive'

 Plug 'pmizio/typescript-tools.nvim'
call plug#end()

colorscheme dracula

let g:python_highlight_all = 1

" move line or visually selected block - alt+j/k
inoremap <A-j> <Esc>:m .+1<CR>==gi
inoremap <A-k> <Esc>:m .-2<CR>==gi
vnoremap <A-j> :m '>+1<CR>gv=gv
vnoremap <A-k> :m '<-2<CR>gv=gv
" move split panes to left/bottom/top/right
 nnoremap <A-h> <C-W>H
 nnoremap <A-j> <C-W>J
 nnoremap <A-k> <C-W>K
 nnoremap <A-l> <C-W>L
" move between panes to left/bottom/top/right
 nnoremap <C-h> <C-w>h
 nnoremap <C-j> <C-w>j
 nnoremap <C-k> <C-w>k
 nnoremap <C-l> <C-w>l

" Set completeopt to have a better completion experience
" :help completeopt
" menuone: popup even when there's only one match
" noinsert: Do not insert text until a selection is made
" noselect: Do not select, force user to select one from the menu
set completeopt=menuone,noinsert,noselect

" Avoid showing extra messages when using completion
set shortmess+=c

let mapleader = ","

" Configure LSP through rust-tools.nvim plugin.
" rust-tools will configure and enable certain LSP features for us.
" See https://github.com/simrat39/rust-tools.nvim#configuration
lua <<EOF
local nvim_lsp = require'lspconfig'

vim.api.nvim_create_autocmd("CursorHold", {
    callback = function()
        vim.diagnostic.open_float(nil, { focusable = false })
    end
})

local opts = {
    tools = { -- rust-tools options
        autoSetHints = true,
        inlay_hints = {
            show_parameter_hints = false,
            parameter_hints_prefix = "",
            other_hints_prefix = "",
        },
    },

    -- all the opts to send to nvim-lspconfig
    -- these override the defaults set by rust-tools.nvim
    -- see https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#rust_analyzer
    server = {
        -- on_attach is a callback called when the language server attachs to the buffer
        -- on_attach = on_attach,
        --on_attach = function(_, bufnr)
          --vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
          --vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
        --end,
        settings = {
            -- to enable rust-analyzer settings visit:
            -- https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/user/generated_config.adoc
            ["rust-analyzer"] = {
                -- enable clippy on save
                checkOnSave = {
                    command = "clippy"
                },
                cargo = {
                    buildScripts = {
                        enable = false
                    }
                }
            }
        }
    },
}

vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"

-- require('rust-tools').setup(opts)

local format_sync_grp = vim.api.nvim_create_augroup("Format", {})
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = "*.rs",
  callback = function()
    vim.lsp.buf.format({ timeout_ms = 200 })
  end,
  group = format_sync_grp,
})

require('telescope').setup{
  defaults = {
    -- Default configuration for telescope goes here:
    -- config_key = value,
    mappings = {
      i = {
        -- map actions.which_key to <C-h> (default: <C-/>)
        -- actions.which_key shows the mappings for your picker,
        -- e.g. git_{create, delete, ...}_branch for the git_branches picker
        ["<C-h>"] = "which_key"
      }
    }
  },
  pickers = {
    -- Default configuration for builtin pickers goes here:
    -- picker_name = {
    --   picker_config_key = value,
    --   ...
    -- }
    -- Now the picker_config_key will be applied every time you call this
    -- builtin picker
  },
  extensions = {
    -- Your extension configuration goes here:
    -- extension_name = {
    --   extension_config_key = value,
    -- }
    -- please take a look at the readme of the extension you want to configure
  }
}

local builtin = require('telescope.builtin')
vim.keymap.set('n', '<Leader>ff', builtin.find_files, {})
vim.keymap.set('n', '<Leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<Leader>fb', function()
builtin.buffers({ sort_lastused = true, ignore_current_buffer = true })
end, { desc = "Switch buffers" })
vim.keymap.set('n', '<Leader>fh', builtin.help_tags, {})
vim.keymap.set('n', '<Leader>fr', builtin.lsp_references, {})
vim.keymap.set('n', '<Leader>fi', builtin.lsp_implementations, {})
vim.keymap.set('n', '<Leader>fd', builtin.lsp_definitions, {})
vim.keymap.set('n', '<Leader>rn', vim.lsp.buf.rename, { desc = "Rename symbol" })
vim.keymap.set('n', '<Leader>fs', function()
builtin.lsp_document_symbols({symbol_width=50, symbols={"method", "function"}})
end, {})
vim.keymap.set('n', '<C-p>', builtin.git_files, {})

-- Setup Completion
-- See https://github.com/hrsh7th/nvim-cmp#basic-configuration
local cmp = require'cmp'
cmp.setup({
  -- Enable LSP snippets
  snippet = {
    expand = function(args)
        vim.fn["vsnip#anonymous"](args.body)
    end,
  },
  mapping = {
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    -- Add tab support
    ['<S-Tab>'] = cmp.mapping.select_prev_item(),
    ['<Tab>'] = cmp.mapping.select_next_item(),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Insert,
      select = true,
    })
  },

  -- Installed sources
  sources = {
    { name = 'nvim_lsp' },
    { name = 'vsnip' },
    { name = 'path' },
    { name = 'buffer' },
  },
})

nvim_lsp.pyright.setup{}
nvim_lsp.rust_analyzer.setup{
  settings = {
    ['rust-analyzer'] = {
      diagnostics = {
        enable = false;
      },
      cargo = {
        targetDir = 'target-analyzer';
      }
    }
  }
}

require("typescript-tools").setup {}

EOF

if has("autocmd")
    filetype plugin indent on

    augroup vimrcEx
    au!

    " Python files should get PEP8 white space settings
    autocmd BufNewFile,BufEnter,BufRead *.py set filetype=python
    autocmd FileType python set tabstop=4|set shiftwidth=4|set expandtab|setlocal textwidth=88


    autocmd FileType text setlocal textwidth=78
    autocmd FileType markdown setlocal textwidth=78
    autocmd FileType gitcommit setlocal textwidth=72

    " Json tab-twiddler
    autocmd FileType json set tabstop=2|set shiftwidth=2|set expandtab

    " Javascript tab-twiddler
    autocmd FileType javascript set tabstop=2|set shiftwidth=2|set expandtab
    autocmd FileType javascript.jsx set tabstop=2|set shiftwidth=2|set expandtab

    " Yaml 
    autocmd FileType yaml set tabstop=2|set shiftwidth=2|set expandtab

    au BufNewFile,BufRead */ansible/roles/*/files/*.{automount,mount,path,service,socket,swap,target,timer}  setf systemd

    augroup END
else
    set autoindent
endif


try
    nmap <silent> [c :call CocAction('diagnosticNext')<cr>
    nmap <silent> ]c :call CocAction('diagnosticPrevious')<cr>
    nmap <silent> gd :call CocAction('jumpDefinition')<cr>
endtry

let g:vim_markdown_fenced_languages = ['rust=rust', 'json=json', 'diff=diff']

lua <<EOF

-- Array of file names indicating root directory. Modify to your liking.
local root_names = { '.git', 'Makefile' }

-- Cache to use for speed up (at cost of possibly outdated results)
local root_cache = {}

local set_root = function()
  -- Get directory path to start search from
  local path = vim.api.nvim_buf_get_name(0)
  if path == '' then return end
  path = vim.fs.dirname(path)

  -- Try cache and resort to searching upward for root directory
  local root = root_cache[path]
  if root == nil then
    local root_file = vim.fs.find(root_names, { path = path, upward = true })[1]
    if root_file == nil then return end
    root = vim.fs.dirname(root_file)
    root_cache[path] = root
  end

  -- Set current directory
  vim.fn.chdir(root)
end

local root_augroup = vim.api.nvim_create_augroup('MyAutoRoot', {})
vim.api.nvim_create_autocmd('BufEnter', { group = root_augroup, callback = set_root })

require'nvim-treesitter.configs'.setup {
  highlight = {
    enable = true,
    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
}

EOF

hi! link @lsp.type.keyword.rust rustKeyword
