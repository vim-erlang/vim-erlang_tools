" defaults {{{1

if exists('g:erlang_tools#utils#autoloaded')
    finish
endif
let g:erlang_tools#utils#autoloaded = 1

" }}}

function erlang_tools#utils#prg(tool) " {{{1
    return erlang_tools#utils#default(a:tool, 'prg')
endfunction " 1}}}

function erlang_tools#utils#opts(tool, opts) " {{{1
    let default_opts = erlang_tools#utils#default(a:tool, 'opts')
    if !empty(default_opts)
        return default_opts . ' ' . a:opts
    else
        return a:opts
    endif
endfunction " 1}}}

function erlang_tools#utils#default(tool, key) " {{{1
    let varname = 'erlang_tools#' . a:tool . '#' . a:key
    if exists('b:' . varname)
        return b:{varname}
    elseif exists('t:'. varname)
        return t:{varname}
    elseif exists('g:' . varname)
        return g:{varname}
    else
        return ''
    endif
endfunction " 1}}}

function erlang_tools#utils#read_file(dir, fname) "{{{1
    let cwd = getcwd()
    if a:dir != cwd && !isdirectory(a:dir)
        echomsg 'No such directory:' a:dir
        let output = []
    else
        if a:dir != cwd
            execute 'lcd' a:dir
        endif
        try
            let output = readfile(a:fname)
        catch /.*/
            echomsg 'No such file:' a:fname
            let output = []
        endtry
        if a:dir != cwd
            execute 'lcd' cwd
        endif
    endif
    return output
endfunction " 1}}}

function erlang_tools#utils#compile_erl(dir, module) " {{{1
    let fname = globpath(a:dir, a:module . '.erl')
    if empty(fname)
        echomsg 'Failed to find:' a:module
        return -1
    else
        let result = system('erlc -o ' . fnameescape(a:dir) . ' ' . fname)
        if empty(result)
            return 0
        else
            echomsg 'Failed to compile:' a:module
            return -1
        endif
    endif
endfunction " 1}}}
