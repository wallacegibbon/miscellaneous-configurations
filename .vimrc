"au BufRead,BufNewFile *.xrl,*.yrl setlocal filetype=erlang
au FileType c,cpp setlocal cino=(s,:0,l1,g0,t0,N-s,E-s "cinoptions-values

filetype plugin indent on
syntax on

set nocompatible
set enc=utf-8
set modelines=5
set modeline
set ffs=unix
set ff=unix

"set smartindent
set title
set ruler
set incsearch
set ic

hi Error NONE

"inoremap \<tab> <tab>
"inoremap <tab> <c-p>
inoremap jf <esc>
vnoremap jf <esc>
cnoremap jf <c-f>
nnoremap <space> <c-f>
nnoremap gh <c-t>
nnoremap gl <c-]>

set lispwords-=if
set lispwords+=match

let maplocalleader = "\\"

colorscheme slate
