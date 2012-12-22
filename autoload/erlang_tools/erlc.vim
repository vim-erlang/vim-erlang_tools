" defaults {{{1

if exists('g:erlang_tools#erlc#autoloaded')
    finish
endif
let g:erlang_tools#erlc#autoloaded = 1

if !exists('g:erlang_tools#erlc#prg')
    let g:erlang_tools#erlc#prg = 'erlc'
endif

" 1}}}

function erlang_tools#erlc#prg() " {{{1
    return erlang_tools#utils#prg('erlc')
endfunction " 1}}}

function erlang_tools#erlc#opts(opts) " {{{1
    return erlang_tools#utils#opts('erlc', a:opts)
endfunction " 1}}}

function erlang_tools#erlc#parse_output(dir, output) " {{{1
    return erlang_tools#parse#output(a:dir, a:output)
endfunction " 1}}}
