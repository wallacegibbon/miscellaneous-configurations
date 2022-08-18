"au BufRead,BufNewFile *.xrl,*.yrl setlocal filetype=erlang
"au FileType c,cpp setlocal cino=(s,:0,l1,g0,t0,N-s,E-s

"filetype plugin indent on
syntax on

set nocompatible nosmartindent autoindent noincsearch title ruler
set modeline modelines=6 ff=unix ffs=unix enc=utf-8

"set lispwords-=if
"set lispwords+=match

inoremap jf <esc>
vnoremap jf <esc>
cnoremap jf <c-f>
nnoremap gh <c-t>
nnoremap gl <c-]>

let mapleader = "\<space>"
nnoremap <leader>t :buffers<cr>:b<space>
nnoremap <leader><space> :b#<cr>

colorscheme slate
hi Error NONE
hi Statement cterm=bold
hi Comment cterm=bold
hi String cterm=underline

