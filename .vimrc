au BufRead,BufNewFile *.xrl,*.yrl,rebar.* setlocal filetype=erlang
au FileType erlang setlocal sw=4 ts=8 noet
au FileType make,asm setlocal sw=8 ts=8 noet

filetype plugin indent on
syntax on

set nocompatible
set cino=(0,u0,U0,:0,l1,g0,t0 "cinoptions-values

set enc=utf-8
set modelines=5
set modeline

set smartindent
set title
set ruler
set incsearch
set ic

hi Error NONE

inoremap \<tab> <tab>
inoremap <tab> <c-p>
imap jf <esc>
vmap jf <esc>
cmap jf <c-f>
nmap <space> <c-f>
nmap gh <c-t>
nmap gl <c-]>

colorscheme pablo

