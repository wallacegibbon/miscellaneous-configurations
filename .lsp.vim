function! s:find_root(flist) abort
    let l:path = lsp#utils#get_buffer_path()
    let l:root = lsp#utils#find_nearest_parent_file_directory(l:path , a:flist)
    if empty(l:root)
	return lsp#utils#path_to_uri(l:path)
    else
	return lsp#utils#path_to_uri(l:root)
    endif
endfunction

function! s:do_reg_server(name) abort
    execute 'augroup lspcfg_' . a:name
    au!
    execute 'au User lsp_setup call lsp#register_server(g:lspcfg_' . a:name . ')'
    augroup END
endfunction

function! s:reg_server(name) abort
    if executable(a:name)
	call s:do_reg_server(a:name)
    endif
endfunction

"" The variable name have to be `lspcfg_` + executable name.

let g:lspcfg_clangd = {
\ "name": "clangd",
\ "cmd": {_ -> ["clangd", "--background-index", "--compile-commands-dir=build"]},
\ "root_uri": {_ -> s:find_root(["CMakeLists.txt", "Makefile"])},
\ "allowlist": ["c", "cpp"]
\ }

let g:lspcfg_elp = {
\ "name": "elp",
\ "cmd": {_ -> ["elp", "server"]},
\ "root_uri": {_ -> s:find_root(["rebar.config", "rebar.lock", "Makefile"])},
\ "allowlist": ["erlang"]
\ }

call s:reg_server("clangd")
call s:reg_server("elp")
