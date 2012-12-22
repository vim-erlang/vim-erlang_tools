" defaults {{{1

if exists('g:erlang_tools#vimerl_check#autoloaded')
    finish
endif
let g:erlang_tools#vimerl_check#autoloaded = 1

if !exists('g:erlang_tools#vimerl_check#bundle_dir')
    " likely <sfile>
    " bundle/vim-erlang_tools/autoload/erlang_tools/vimerl_check.vim
    let g:erlang_tools#vimerl_check#bundle_dir = expand("<sfile>:p:h:h:h:h")
endif

function erlang_tools#vimerl_check#find_prg() " {{{2
    let bundle_dir = g:erlang_tools#vimerl_check#bundle_dir
    let escript_path = 'vimerl/compiler/erlang_check.erl'
    let escript_list = split(globpath(bundle_dir, escript_path), "\n")
    if empty(escript_list)
        return ''
    else
        return 'escript '. escript_list[0]
    endif
endfunction " 2}}}

if !exists('g:erlang_tools#vimerl_check#prg')
    let g:erlang_tools#vimerl_check#prg = erlang_tools#vimerl_check#find_prg()
endif

" 1}}}

function erlang_tools#vimerl_check#prg() " {{{1
    return erlang_tools#utils#prg('vimerl_check')
endfunction " 1}}}

function erlang_tools#vimerl_check#opts(opts) " {{{1
    return erlang_tools#utils#opts('vimerl_check', a:opts)
endfunction " 1}}}

function erlang_tools#vimerl_check#parse_output(cwd, output) " {{{1
    return erlang_tools#parse#output(a:cwd, a:output)
endfunction " 1}}}


