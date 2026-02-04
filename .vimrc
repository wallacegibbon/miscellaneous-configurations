"autocmd BufRead,BufNewFile *.yrl,*.escript,*.es setlocal filetype=erlang
autocmd FileType c,cpp setlocal cinoptions=:0,(0,l1,t0,W2s,N-s,E-s,g0

set nocompatible smartindent noincsearch nostartofline scrolloff=0 jumpoptions=stack
set fileencodings=utf-8,latin-1,gb18030,gbk,gb2312 encoding=utf-8 langmenu=none
set title ruler modeline modelines=6 laststatus=1 belloff=all
set number numberwidth=9 relativenumber signcolumn=yes
set completeopt=menuone,noinsert,preview
"set fileformat=unix fileformats=unix,dos

let g:asyncomplete_auto_completeopt = 0
let g:lsp_diagnostics_enabled = 1

filetype plugin on
"" `syntax on` have to be before `highlight ...` to make highlight command work.
syntax on
highlight SignColumn ctermbg=NONE ctermfg=NONE guibg=NONE guifg=NONE
highlight Error NONE
language C

"" Install <https://github.com/junegunn/vim-plug>, then run `:PlugInstall`.
call plug#begin()
Plug 'junegunn/fzf', {'do': {-> fzf#install()}}
Plug 'junegunn/fzf.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
call plug#end()

inoremap <expr> <cr> pumvisible() ? "\<c-y>" : "\<cr>"

nnoremap <space>f :Files<cr>
nnoremap <space>b :Buffers<cr>

nnoremap <space>d <plug>(lsp-definition)
nnoremap <space>t <plug>(lsp-type-definition)
nnoremap <space>r <plug>(lsp-references)
nnoremap <space>j <plug>(lsp-next-diagnostic)
nnoremap <space>k <plug>(lsp-previous-diagnostic)
nnoremap <space>2 <plug>(lsp-rename)
nnoremap <space>p <plug>(lsp-document-format)

nnoremap <space>e :b#<cr>
nnoremap <space>w :w<cr>
nnoremap <space>q :qa<cr>

cnoremap jf <c-c>
tnoremap jf <esc>
inoremap jf <esc>
vnoremap jf <esc>
onoremap jf <esc>

silent! source ~/.lsp.vim
