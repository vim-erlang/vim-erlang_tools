" defaults {{{1

if exists('g:erlang_tools#erlv#autoloaded')
    finish
endif
let g:erlang_tools#erlv#autoloaded = 1

if !exists('g:erlang_tools#erlv#prg')
    let g:erlang_tools#erlv#prg = 'erlc'
endif

if !exists('g:erlang_tools#erlv#opts')
    let g:erlang_tools#erlv#opts = join([
                \'-v',
                \'-W',
                \'+strong_validation',
                \'+warn_bif_clash',
                \'+warn_export_all',
                \'+warn_export_vars',
                \'+warn_shadow_vars',
                \'+warn_unused_function',
                \'+warn_deprecated_function',
                \'+warn_obsolete_guard',
                \'+warn_unused_imports',
                \'+warn_unused_vars',
                \'+warn_unused_record',
                \'+warn_missing_spec'
                \], ' ')
endif

if !exists('g:erlang_tools#erlv#erlc_opts')
    let g:erlang_tools#erlv#erlc_opts = 1
endif

" 1}}}

function erlang_tools#erlv#opts(opts) " {{{1
    if g:erlang_tools#erlv#erlc_opts
        let opts = erlang_tools#utils#opts('erlc', a:opts)
        let opts = erlang_tools#utils#opts('erlv', opts)
    else
        let opts = erlang_tools#utils#opts('erlv', a:opts)
    endif
    return opts
endfunction " 1}}}

function erlang_tools#erlv#parse_output(dir, output) " {{{1
    return erlang_tools#parse#output(a:dir, a:output)
endfunction " 1}}}

