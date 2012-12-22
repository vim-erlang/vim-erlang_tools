" default {{{1

if exists('g:erlang_tools#ct_run#autoloaded')
    finish
endif
let g:erlang_tools#ct_run#autoloaded = 1

if !exists('g:erlang_tools#ct_run#prg')
    let g:erlang_tools#ct_run#prg = 'ct_run'
endif

if !exists('g:erlang_tools#ct_run#dir')
    let g:erlang_tools#ct_run#dir = expand("<sfile>:p:h")
endif

if !exists('g:erlang_tools#ct_run#vet_io')
    let g:erlang_tools#ct_run#vet_io = 'vet_io'
endif

if !exists('g:erlang_tools#ct_run#vet_util')
    let g:erlang_tools#ct_run#vet_util = 'vet_util'
endif


if !exists('g:erlang_tools#ct_run#vet_cth')
    let g:erlang_tools#ct_run#vet_cth = 'vet_cth'
endif

" 1}}}

function erlang_tools#ct_run#prg() " {{{1
    return erlang_tools#utils#prg('ct_run')
endfunction " 1}}}

function erlang_tools#ct_run#opts(opts) " {{{1
    let opts = '-pa '. fnameescape(g:erlang_tools#ct_run#dir)
    let opts .= ' ' . erlang_tools#utils#opts('ct_run', a:opts)
    let ct_hook = g:erlang_tools#ct_run#vet_cth
    if empty(matchstr(opts, '-ct_hooks'))
        let opts = '-ct_hooks ' . ct_hook  . ' [] ' . opts
    else
        let pat = '-ct_hooks '
        let sub = '-ct_hooks ' . ct_hook . ' [] and '
        let opts = substitute(opts, pat, sub, '')
    endif
    return opts
endfunction " 1}}}

function erlang_tools#ct_run#parse_output(cwd, output) " {{{1
    return erlang_tools#parse#output(a:cwd, a:output)
endfunction " 1}}}

function erlang_tools#ct_run#compile_erl() " {{{1
    let dir = g:erlang_tools#ct_run#dir
    let result = 0
    let fnames = [
                \   g:erlang_tools#ct_run#vet_io,
                \   g:erlang_tools#ct_run#vet_util,
                \   g:erlang_tools#ct_run#vet_cth
                \   ]
    for fname in fnames
        if erlang_tools#utils#compile_erl(dir, fname) == -1
            let result = -1
        endif
    endfor
    return result
endfunction " 1}}}

call erlang_tools#ct_run#compile_erl()
