" defaults {{{1

if exists('g:erlang_tools#dialyzer#autoloaded')
    finish
endif
let g:erlang_tools#dialyzer#autoloaded = 1

if !exists('g:erlang_tools#dialyzer#prg')
    let g:erlang_tools#dialyzer#prg = 'dialyzer'
endif

if !exists('g:erlang_tools#dialyzer#opts')
    let g:erlang_tools#dialyzer#opts = '--fullpath'
endif

" 1}}}

function erlang_tools#dialyzer#prg() " {{{1
    return erlang_tools#utils#prg('dialyzer')
endfunction " 1}}}

function erlang_tools#dialyzer#opts(opts) " {{{1
    return erlang_tools#utils#opts('dialyzer', a:opts)
endfunction " 1}}}

function erlang_tools#dialyzer#parse_output(dir, output) " {{{1
    return erlang_tools#parse#output(a:dir, a:output)
endfunction " 1}}}
