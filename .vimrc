"au BufRead,BufNewFile *.xrl,*.yrl setlocal filetype=erlang
"au FileType c,cpp setlocal cino=(s,:0,l1,g0,t0,N-s,E-s

set nocompatible nosmartindent autoindent noincsearch title ruler nu relativenumber
set modeline modelines=6 ff=unix ffs=unix enc=utf-8 nuw=9

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

"set lispwords-=if
"set lispwords+=match

"" Copy `coc.vim` to `~/.vim/pack/my/start/`,
"" then `CocInstall coc-tsserver`, `CocInstall coc-clangd`
nmap <space>d <Plug>(coc-definition)
nmap <space>r <Plug>(coc-references)
nmap <space>t <Plug>(coc-type-definition)
nmap <space>i <Plug>(coc-implementation)
nmap <space>2 <Plug>(coc-rename)
nmap <space>h <Plug>(coc-diagnostic-prev)
nmap <space>l <Plug>(coc-diagnostic-next)

