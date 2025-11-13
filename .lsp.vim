function! s:find_root(pathlist) abort
    let l:bufferpath = lsp#utils#get_buffer_path()
    let l:root = lsp#utils#find_nearest_parent_file_directory(l:bufferpath , a:pathlist)
    return lsp#utils#path_to_uri(l:root)
endfunction

if executable('clangd')
    augroup lsp_clangd
	au!
	au User lsp_setup call lsp#register_server({
		    \ 'name': 'clangd',
		    \ 'cmd': {_ -> ['clangd', '--background-index', '--compile-commands-dir=build']},
		    \ 'root_uri': {_ -> s:find_root(['.git', 'compile_commands.json', 'CMakeLists.txt'])},
		    \ 'allowlist': ['c', 'cpp']
		    \ })
    augroup END
endif

if executable('elp')
    augroup lsp_elp
	au!
	au User lsp_setup call lsp#register_server({
		    \ 'name': 'elp',
		    \ 'cmd': {_ -> ['elp', 'server']},
		    \ 'root_uri': {_ -> s:find_root(["rebar.config", "rebar.lock", "Makefile", ".git"])},
		    \ 'allowlist': ['erlang']
		    \ })
    augroup END
endif

function! s:on_lsp_buffer_enabled() abort
    inoremap <expr> <tab> pumvisible() ? "\<c-n>" : "\<tab>"
    inoremap <expr> <s-tab> pumvisible() ? "\<c-p>" : "\<s-tab>"
    inoremap <expr> <cr> pumvisible() ? asyncomplete#close_popup() : "\<cr>"
    nmap <buffer> gd <plug>(lsp-definition)
    nmap <buffer> g2 <plug>(lsp-rename)
endfunction

augroup lsp_keymaps
    au!
    au User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END
