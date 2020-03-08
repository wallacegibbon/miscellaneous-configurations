set nocompatible
filetype plugin indent on
syntax on

au BufRead,BufNewFile *.yrl,*.app.src,rebar.* setlocal filetype=erlang
"au FileType python setlocal backspace=indent,start sw=4 et
set sw=4

"On MacOS, the default modelines=0
set modelines=5
set modeline

set smartindent
set title
set ruler
set incsearch
set ic

inoremap \<tab> <tab>
inoremap <tab> <c-p>
imap jf <esc>
vmap jf <esc>
cmap jf <c-f>
nmap <space> <c-f>
nmap <bs> <c-b>
nmap gh <c-t>
nmap gl <c-]>

hi Error NONE

colorscheme delek

