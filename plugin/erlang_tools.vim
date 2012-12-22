" config {{{1
if exists('g:loaded_erlang_tools')
    finish
endif
let g:loaded_erlang_tools = 1

" 1}}}

function Erlc(...) " {{{1
    return s:slave_run('erlc', a:000)
endfunction " 1}}}

function Erlt(...) " {{{1
    return s:slave_run('erlt', a:000)
endfunction " 1}}}

function Erlv(...) " {{{1
    return s:slave_run('erlv', a:000)
endfunction " 1}}}

function Dialyzer(...) " {{{1
    return s:slave_run('dialyzer', a:000)
endfunction " 1}}}

function Ctrun(...) " {{{1
    return s:slave_run('ct_run', a:000)
endfunction " 1}}}

function Ctrunsuite(...) " {{{1
    let suite = expand('%')
    if empty(matchstr(suite, '_SUITE.erl$'))
        echomsg 'Not a common test suite:' suite
        return -1
    else
        let opts = ['-suite ' . fnameescape(suite)]
        call extend(opts, a:000)
        return Ctrun(join(opts))
    endif
endfunction " 1}}}

function Ctruncase(...) " {{{1
    let testcase = expand('<cword>')
    if empty(matchstr(testcase, '^\(\l\w*\|''[^'']*''\)$'))
        echomsg 'Not a testcase:' testcase
        return -1
    else
        let opts = ['-case ' . testcase]
        call extend(opts, a:000)
        return Ctrunsuite(join(opts))
    endif
endfunction " 1}}}

function Eunit(...) " {{{1
    return s:slave_run('eunit', a:000)
endfunction " 1}}}

function Eunitmodule(...) " {{{1
    let module = expand('%:t:r')
    let opts = ['-module ' . module]
    call extend(opts, a:000)
    return Eunit(join(opts))
endfunction " 1}}}

function Eunittest(...) " {{{1
    let testcase = expand('<cword>')
    if empty(matchstr(testcase, '^\(\l\w*\|''[^'']*''\)$'))
        echomsg 'Not a testcase:' testcase
        return -1
    else
        let opts = ['-test ' . testcase]
        call extend(opts, a:000)
        return Eunitmodule(join(opts))
    endif
endfunction " 1}}}

function Rebar(...) " {{{1
    return s:slave_run('rebar', a:000)
endfunction " 1}}}

function s:slave_run(tool, opts) " {{{1
    let opts = join(a:opts)
    return erlang_tools#slave_run(a:tool, opts)
endfunction " 1}}}

function Erlp(...) " {{{1
    let tool = exists('a:1') ? a:1 : ''
    let cwd = exists('a:2') ? a:2 : ''
    return erlang_tools#slave_parse(tool, cwd)
endfunction " 1}}}

function Erlr(...) " {{{1
    if !a:0
        let tool = ''
        let position = 0
    elseif a:0 == 1 && !empty(matchstr(a:1, '^\d*$'))
        let tool = ''
        let position = a:1
    elseif a:0 == 1
        let tool = a:1
        let position = 0
    elseif !empty(matchstr(a:2, '^\d*$'))
        let tool = a:1
        let position = a:2
    endif
    return erlang_tools#slave_rerun(tool, position)
endfunction " 1}}}

function Erlh(...) " {{{1
    let tool = !a:0 ? '' : a:1
    return erlang_tools#show_history(tool)
endfunction " 1}}}

function Erls(tool, ...) " {{{1
    return erlang_tools#qf_select(a:tool, a:000)
endfunction " 1}}}

function Erld(...) " {{{1
    return erlang_tools#qf_delete(a:000)
endfunction " 1}}}

" command {{{1

command -nargs=? Erlc call Erlc(<f-args>)
command -nargs=? Erlt call Erlt(<f-args>)
command -nargs=? Erlv call Erlv(<f-args>)
command -nargs=? Dialyzer call Dialyzer(<f-args>)
command -nargs=? Ctrun call Ctrun(<f-args>)
command -nargs=? Ctrunsuite call Ctrunsuite(<f-args>)
command -nargs=? Ctruncase call Ctruncase(<f-args>)
command -nargs=? Eunit call Eunit(<f-args>)
command -nargs=? Eunitmodule call Eunitmodule(<f-args>)
command -nargs=? Eunittest call Eunittest(<f-args>)
command -nargs=? Rebar call Rebar(<f-args>)
command -nargs=* Erlp call Erlp(<f-args>)
command -nargs=* Erlr call Erlr(<f-args>)
command -nargs=? Erlh call Erlh(<f-args>)
command -nargs=+ Erls call Erls(<f-args>)
command -nargs=* Erld call Erld(<f-args>)

" 1}}}

function s:refresh(bufnr) " {{{1
    return erlang_tools#refresh(a:bufnr, 1)
endfunction " 1}}}

function s:check_run(bufnr) " {{{1
    return erlang_tools#check_run(a:bufnr)
endfunction " 1}}}

function s:check_remove(bufnr) " {{{1
    return erlang_tools#check_remove(a:bufnr)
endfunction " 1}}}

function s:cleanup() " {{{1
    return erlang_tools#tmux#cleanup()
endfunction " 1}}}

augroup erlang_tools " {{{1
    autocmd!
    autocmd BufWinEnter *.erl,*.hrl call s:refresh(expand('<abuf>') + 0)
    autocmd BufWritePost *.erl call s:check_run(expand('<abuf>') + 0)
    autocmd BufDelete *.erl call s:check_remove(expand('<abuf>') + 0)
    autocmd VimLeavePre * call s:cleanup()
augroup END " 1}}}

" disable vimerl compiler {{{1

if exists(':ErlangDisableShowErrors') == 2
    execute ':ErlangDisableShowErrors'
endif
let g:erlang_show_errors = 0

" 1}}}
