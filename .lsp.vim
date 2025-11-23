function! s:prjroot(flist) abort
  let l:path = fnamemodify(lsp#utils#get_buffer_path(), ":h")
  let l:root = lsp#utils#find_nearest_parent_file_directory(l:path, a:flist)
  return lsp#utils#path_to_uri(empty(l:root) ? l:path : l:root)
endfunction

function! s:register(languages, lspargs, prjfiles) abort
  let l:execname = a:lspargs[0]
  if executable(l:execname)
    execute 'au User lsp_setup call lsp#register_server({' .
          \ "'name': " . string(l:execname) . ',' .
          \ "'cmd': {_ -> " . string(a:lspargs) . '},' .
          \ "'root_uri': {_ -> s:prjroot(" . string(a:prjfiles) . ')},' .
          \ "'allowlist': " . string(a:languages) .
          \ '})'
  endif
endfunction

function! s:on_lsp_buffer_enabled() abort
  nnoremap <buffer> K <plug>(lsp-hover)
endfunction

augroup lsp_install
  au!
  autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END


call s:register(
      \ ["ocaml"],
      \ ["ocamllsp"],
      \ ["dune-project", "_build"]
      \ )

call s:register(
      \ ["erlang"],
      \ ["elp", "server"],
      \ ["rebar.config", "erlang.mk", "_build"]
      \ )

call s:register(
      \ ["c", "cpp"],
      \ ["clangd", "--background-index", "--compile-commands-dir=build"],
      \ ["CMakeLists.txt", "Makefile", "build"]
      \ )

call s:register(
      \ ["python"],
      \ ["pylsp"],
      \ ["pyproject.toml"]
      \ )
