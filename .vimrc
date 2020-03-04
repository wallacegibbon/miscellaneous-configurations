set nocompatible

"au FileType python setlocal backspace=indent,start sw=4 et
au FileType erlang,c setlocal sw=4

au BufRead,BufNewFile *.app.src,rebar.* setlocal filetype=erlang
"au BufRead,BufNewFile *.lisp setlocal lisp ai

inoremap \<Tab> <Tab>
inoremap <Tab> <C-n>
imap jf <Esc>
vmap jf <Esc>
cmap jf <C-f>
nmap <Space> <C-f>
nmap <BS> <C-b>
nmap gh <C-t>
nmap gl <C-]>

"On MacOS, the default modelines=0
set modeline
set modelines=5

filetype plugin indent on
syntax on

set smartindent
set title
set ruler
set incsearch
set ic

hi Error NONE

