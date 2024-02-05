"" Position of the initialization file of neovim: "~/.config/nvim/init.vim" (Linux) or "~/AppData/Local/nvim/init.vim" (Windows).

"autocmd BufRead,BufNewFile *.xrl,*.yrl setlocal filetype=erlang
"autocmd FileType c,cpp setlocal cinoptions=(s,:0,l1,g0,t0,N-s,E-s

set nocompatible nosmartindent autoindent noincsearch title ruler modeline modelines=6 laststatus=0 encoding=utf-8
"set fileformat=unix fileformats=unix
"set number numberwidth=9 relativenumber
"set expandtab tabstop=2 softtabstop=2 shiftwidth=2
"set lispwords-=if lispwords+=match

"" "jumpoptions=stack" is the reason why I choose neovim.
set jumpoptions=stack

"let mapleader = "\<space>"

nnoremap <space>b :buffers<cr>:b<space>
nnoremap <space>e :b#<cr>
nnoremap <space>w :w<cr>
nnoremap <space>q :qa<cr>
inoremap jf <esc>
cnoremap jf <c-c>

filetype plugin on

"" The "syntax on" command have to be before the "highlight ..." commands to make highlight working.
syntax on

highlight Error NONE
"highlight Statement cterm=bold
"highlight Comment cterm=bold
"highlight String cterm=underline

"colorscheme slate

"" Install "https://github.com/junegunn/vim-plug", then run ":PlugInstall" and ":CocInstall coc-tsserver coc-clangd".

call plug#begin()
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
call plug#end()

inoremap <expr> <cr> coc#pum#visible() ? coc#pum#confirm() : "\<cr>"
nnoremap <silent> <space>? :call CocActionAsync("doHover")<cr>

nmap <space>d <Plug>(coc-definition)
nmap <space>r <Plug>(coc-references)
nmap <space>t <Plug>(coc-type-definition)
nmap <space>i <Plug>(coc-implementation)
nmap <space>2 <Plug>(coc-rename)
nmap <space>h <Plug>(coc-diagnostic-prev)
nmap <space>l <Plug>(coc-diagnostic-next)
nmap <space>p <Plug>(coc-format)

nnoremap <space>f :FZF<cr>

"let g:rust_recommended_style = 0

"set guifont=Consolas:h18:i

