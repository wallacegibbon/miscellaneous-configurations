set nocompatible
filetype plugin indent on
syntax on

"au BufRead,BufNewFile *.yrl,rebar.* setlocal filetype=erlang
"au FileType elm setlocal et sw=2

inoremap \<tab> <tab>
inoremap <tab> <c-p>
imap jf <esc>
vmap jf <esc>
cmap jf <c-f>
nmap <space> <c-f>
nmap gh <c-t>
nmap gl <c-]>

hi Error NONE

"set ts=2 sw=2 sts=2 et
set modelines=5
set modeline

set smartindent
set title
set ruler
set incsearch
set ic

set enc=utf-8

