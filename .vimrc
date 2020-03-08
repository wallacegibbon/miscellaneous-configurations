set nocompatible
filetype plugin indent on
syntax on

au BufRead,BufNewFile *.yrl,*.app.src,rebar.* setlocal filetype=erlang
"au FileType python setlocal backspace=indent,start sw=4 et
set sw=4

"On MacOS, the default modelines=0
set modeline
set modelines=5

set smartindent
set title
set ruler
set incsearch
set ic

inoremap \<Tab> <Tab>
inoremap <Tab> <C-p>
imap jf <Esc>
vmap jf <Esc>
cmap jf <C-f>
nmap <Space> <C-f>
nmap <BS> <C-b>
nmap gh <C-t>
nmap gl <C-]>

hi Error NONE

colorscheme delek

