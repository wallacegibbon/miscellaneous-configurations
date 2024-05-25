set lines=999 co=999
set go-=m go-=T go-=r
"winpos 620 0

set guifont=Consolas:h18:i,Cascadia\ Code:h18:i

"" Delay running time of `colorscheme` to take the effect you want.
autocmd BufRead,BufNewFile * colorscheme default
"colorscheme desert
"colorscheme slate

