" config {{{1

if exists('g:autoloaded_erlang_tools_rebar')
    finish
endif
let g:autoloaded_erlang_tools_rebar = 1

if !exists('g:erlang_tools#rebar#prg')
    let g:erlang_tools#rebar#prg = 'rebar'
endif

" 1}}}

function erlang_tools#rebar#opts(opts) " {{{1
    return erlang_tools#utils#opts('rebar', a:opts)
endfunction " 1}}}

function erlang_tools#rebar#parse_output(cwd, output) " {{{1
   let unsupported_cmds = ['ct', 'eunit', 'xref']
   let unsupported_used = []
   for cmd in unsupported_cmds
       if !empty(matchstr(a:output, '==> [\n]* (' . cmd . ')\n'))
           call add(unsupported_used, cmd)
       endif
   endfor
   if !empty(unsupported_used)
       echomsg 'Unsupported commands:' join(unsupported_used)
   endif
   return erlang_tools#parse#output(a:cwd, a:output)
endfunction " 1}}}
