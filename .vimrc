set nocompatible
au FileType erlang,python setlocal backspace=indent,start ts=4 sw=4 sts=4 et
au BufRead,BufNewFile rebar.* setlocal filetype=erlang
"au BufRead,BufNewFile *.lisp setlocal lisp ai

inoremap \<Tab> <Tab>
inoremap <Tab> <C-n>
imap jf <Esc>
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

