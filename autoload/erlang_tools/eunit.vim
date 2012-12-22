" defaults {{{1

if exists('g:erlang_tools#eunit#autoloaded')
    finish
endif
let g:erlang_tools#eunit#autoloaded = 1

if !exists('g:erlang_tools#eunit#prg')
    let g:erlang_tools#eunit#prg = 'erl'
endif

if !exists('g:erlang_tools#eunit#dir')
    let g:erlang_tools#eunit#dir = expand("<sfile>:p:h")
endif

if !exists('g:erlang_tools#eunit#vet_io')
    let g:erlang_tools#eunit#vet_io = 'vet_io'
endif

if !exists('g:erlang_tools#eunit#vet_util')
    let g:erlang_tools#eunit#vet_util = 'vet_util'
endif

if !exists('g:erlang_tools#eunit#vet_eunit')
    let g:erlang_tools#eunit#vet_eunit = 'vet_eunit'
endif

if !exists('g:erlang_tools#eunit#vet_eunit_listener')
    let g:erlang_tools#eunit#vet_eunit_listener = 'vet_eunit_listener'
endif

" 1}}}

function erlang_tools#eunit#prg() " {{{1
    return erlang_tools#utils#prg('eunit')
endfunction " 1}}}

function erlang_tools#eunit#opts(opts) " {{{1
    let opts_list = [
                \   '-noshell',
                \   '-sname eunit',
                \   '-pa ' . fnameescape(g:erlang_tools#eunit#dir),
                \   erlang_tools#utils#opts('eunit', a:opts),
                \   '-s ' . g:erlang_tools#eunit#vet_eunit . ' start',
                \   '-s erlang halt'
                \   ]
    return join(opts_list)
endfunction " 1}}}

function erlang_tools#eunit#parse_output(dir, output) " {{{1
    return erlang_tools#parse#output(a:dir, a:output)
endfunction " 1}}}

function erlang_tools#eunit#compile_erl() " {{{1
    let dir = g:erlang_tools#eunit#dir
    let result = 0
    let fnames = [
                \   g:erlang_tools#eunit#vet_io,
                \   g:erlang_tools#eunit#vet_util,
                \   g:erlang_tools#eunit#vet_eunit,
                \   g:erlang_tools#eunit#vet_eunit_listener
                \   ]
    for fname in fnames
        if erlang_tools#utils#compile_erl(dir, fname) == -1
            let result = -1
        endif
    endfor
    return result
endfunction " 1}}}

call erlang_tools#eunit#compile_erl()
