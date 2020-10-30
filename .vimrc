au BufRead,BufNewFile *.xrl,*.yrl,rebar.* setlocal filetype=erlang
"au FileType elm setlocal sw=2 ts=2 sts=2 et noet

"" in the world of erlang, sw=4, ts=8
set sw=4

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

