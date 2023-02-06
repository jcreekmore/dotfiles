local set = vim.opt

set.autoread = true
set.showmatch = true                  
set.ignorecase = true
set.mouse = "a"
set.hlsearch = true
set.incsearch = true
set.tabstop = 4
set.softtabstop = 4
set.expandtab = true
set.shiftwidth = 4
set.number = true
set.relativenumber = true
set.colorcolumn = "+1"
set.clipboard = "unnamedplus"
set.ttyfast = true
set.wildmode = {"longest", "list"}
set.splitright = true
set.splitbelow = true

vim.cmd [[
  filetype plugin indent on
  syntax enable
  colorscheme evening
]]
