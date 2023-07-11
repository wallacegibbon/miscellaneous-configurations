"autocmd BufRead,BufNewFile *.xrl,*.yrl setlocal filetype=erlang
"autocmd FileType c,cpp setlocal cinoptions=(s,:0,l1,g0,t0,N-s,E-s

set nocompatible nosmartindent autoindent noincsearch title ruler number numberwidth=9 relativenumber modeline modelines=6
set fileformat=unix fileformats=unix encoding=utf-8
"set lispwords-=if lispwords+=match

"let mapleader = "\<space>"

nnoremap <space>b :buffers<cr>:b<space>
nnoremap <space>e :b#<cr>
nnoremap <space>w :w<cr>
inoremap jf <esc>
cnoremap jf <c-c>

filetype plugin on

"" The "syntax on" command have to be before the "highlight ..." commands to make highlight working.
syntax on

highlight Error NONE
highlight Statement cterm=bold
highlight Comment cterm=bold
"highlight String cterm=underline

"colorscheme slate

"" Copy the coc package into "~/.vim/pack/my/start/", then run ":CocInstall coc-tsserver coc-clangd" in Vim.
nmap <space>d <Plug>(coc-definition)
nmap <space>r <Plug>(coc-references)
nmap <space>t <Plug>(coc-type-definition)
nmap <space>i <Plug>(coc-implementation)
nmap <space>2 <Plug>(coc-rename)
nmap <space>h <Plug>(coc-diagnostic-prev)
nmap <space>l <Plug>(coc-diagnostic-next)

