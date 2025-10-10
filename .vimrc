"" vim:ft=vim

autocmd BufRead,BufNewFile *.yrl,*.escript,*.es setlocal filetype=erlang
"autocmd BufRead,BufNewFile *.pl,*.pro setlocal filetype=prolog
autocmd BufRead,BufNewFile *.S,*.s,*.asm setlocal filetype=asm
autocmd BufRead,BufNewFile *.hex,*.ihx setlocal filetype=hex
autocmd BufRead,BufNewFile *.e setlocal filetype=elang
"autocmd BufRead,BufNewFile * colorscheme default
autocmd FileType c,cpp setlocal cinoptions=(0,:0,g0,t0,N-s,E-s

"" "jumpoptions=stack" is not supported in old Vim. (older than Vim 9.0.1921)
set jumpoptions=stack

set nocompatible autoindent smartindent noincsearch nostartofline scrolloff=0
set number numberwidth=9 relativenumber
set title ruler modeline modelines=6 laststatus=0 belloff=all
set fileencodings=utf-8,latin-1,chinese,gbk,gb2312,gb18030 encoding=utf-8 langmenu=none
set expandtab tabstop=2 softtabstop=2 shiftwidth=2
"set fileformat=unix fileformats=unix
"set lispwords-=if lispwords+=match

language C

"let mapleader = "\<space>"

filetype plugin on

"" "syntax on" have to be before "highlight ..." to make highlight command work.
syntax on

highlight Error NONE
"highlight Statement cterm=bold
"highlight Comment cterm=bold
"highlight String cterm=underline

"" Install vim-plug from "https://github.com/junegunn/vim-plug",
"" then run ":PlugInstall" to install configured Vim plugins.

call plug#begin()
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'junegunn/fzf', {'do': {-> fzf#install()}}
call plug#end()

"" Run ":CocInstall coc-clangd coc-tsserver" to install COC plugins.
"" COC plugins (npm packages) are in
"" - "~/AppData/Local/coc/extensions" on Windows.
"" - "~/.config/coc/extensions/" on Unix.

inoremap <expr> <cr> coc#pum#visible() ? coc#pum#confirm() : "\<cr>"
nnoremap <silent> <space>? :call CocActionAsync("doHover")<cr>

nnoremap <expr> <c-d> coc#float#has_scroll() ? coc#float#scroll(1, 1) : "\<c-d>"
nnoremap <expr> <c-u> coc#float#has_scroll() ? coc#float#scroll(0, 1) : "\<c-u>"

nmap <space>d <Plug>(coc-definition)
nmap <space>r <Plug>(coc-references)
nmap <space>t <Plug>(coc-type-definition)
nmap <space>i <Plug>(coc-implementation)
nmap <space>2 <Plug>(coc-rename)
nmap <space>h <Plug>(coc-diagnostic-prev)
nmap <space>l <Plug>(coc-diagnostic-next)
nmap <space>p <Plug>(coc-format)

nnoremap <space>f :FZF<cr>

"" Custom keymap
nnoremap <space>b :buffers<cr>:b<space>
nnoremap <space>e :b#<cr>
nnoremap <space>w :w<cr>
nnoremap <space>q :qa<cr>

"let g:rust_recommended_style = 0
let g:markdown_recommended_style = 0
"" Compound literals are not well supported by C syntax yet.
let g:c_no_curly_error = 1
