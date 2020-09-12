set nocompatible
filetype plugin indent on
syntax on

au BufRead,BufNewFile *.yrl,rebar.* setlocal filetype=erlang
"au FileType c setlocal sw=8

set cino=(0,u0,U0,:0,l1,g0,t0 "see :help cinoptions-values

"set ts=2 sw=2 sts=2 et
set sw=4

inoremap \<tab> <tab>
inoremap <tab> <c-p>
imap jf <esc>
vmap jf <esc>
cmap jf <c-f>
nmap <space> <c-f>
nmap gh <c-t>
nmap gl <c-]>

set enc=utf-8
set modelines=5
set modeline

set smartindent
set title
set ruler
set incsearch
set ic

hi Error NONE
colorscheme delek
