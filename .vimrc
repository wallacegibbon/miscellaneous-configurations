set nocompatible
filetype plugin indent on
syntax on

au BufRead,BufNewFile *.yrl,*.app.src,rebar.* setlocal filetype=erlang
au FileType elm setlocal sw=2

inoremap \<tab> <tab>
inoremap <tab> <c-p>
imap jf <esc>
vmap jf <esc>
cmap jf <c-f>
nmap <space> <c-f>
nmap gh <c-t>
nmap gl <c-]>

hi Error NONE

colorscheme delek

set modelines=5
set modeline

set sw=4
set smartindent
set title
set ruler
set incsearch
set ic

