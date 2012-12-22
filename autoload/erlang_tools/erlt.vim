" defaults {{{1

if exists('g:erlang_tools#erlt#autoloaded')
    finish
endif
let g:erlang_tools#erlt#autoloaded = 1

if !exists('g:erlang_tools#erlt#prg')
    let g:erlang_tools#erlt#prg = 'erlc'
endif

if !exists('g:erlang_tools#erlt#opts')
    let g:erlang_tools#erlt#opts = '-DTEST'
endif

if !exists('g:erlang_tools#erlt#erlc_opts')
    let g:erlang_tools#erlt#erlc_opts = 1
endif

" 1}}}

function erlang_tools#erlt#opts(opts) " {{{1
    if g:erlang_tools#erlt#erlc_opts
        let opts = erlang_tools#utils#opts('erlt', a:opts)
        let opts = erlang_tools#utils#opts('erlc', opts)
    else
        let opts = erlang_tools#utils#opts('erlt', a:opts)
    endif
    return opts
endfunction " 1}}}

function erlang_tools#erlt#parse_output(dir, output) " {{{1
    return erlang_tools#parse#output(a:dir, a:output)
endfunction " 1}}}

