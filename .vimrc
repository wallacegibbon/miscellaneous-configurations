"au BufRead,BufNewFile *.xrl,*.yrl setlocal filetype=erlang
"au FileType c,cpp setlocal cino=(s,:0,l1,g0,t0,N-s,E-s

set nocompatible nosmartindent autoindent noincsearch title ruler nonu
set modeline modelines=6 ff=unix ffs=unix enc=utf-8

"set lispwords-=if
"set lispwords+=match

"let mapleader = "\<space>"
nnoremap <space>b :buffers<cr>:b<space>
nnoremap <space>e :b#<cr>
nnoremap <space>w :w<cr>
"nnoremap jf <esc>
"vnoremap jf <esc>
inoremap jf <esc>
cnoremap jf <c-c>

filetype plugin on
syntax on
"colorscheme slate
hi Error NONE
hi Statement cterm=bold
hi Comment cterm=bold
"hi String cterm=underline

