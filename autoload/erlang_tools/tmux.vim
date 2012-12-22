" config {{{1

if exists('g:autoloaded_erlang_tools_tmux')
    finish
endif
let g:autoloaded_erlang_tools_tmux = 1


if !exists('g:erlang_tools#tmux#slave_size')
    let g:erlang_tools#tmux#slave_size = 16
endif

if !exists('g:erlang_tools#tmux#slave_split')
    let g:erlang_tools#tmux#slave_split = 'v'
endif

if !exists('g:erlang_tools#tmux#slave_auto_close')
    let g:erlang_tools#tmux#slave_auto_close = 1
endif

if !exists('g:erlang_tools#tmux#slave_history_limit')
    let g:erlang_tools#tmux#slave_history_limit = 2000
endif

" 1}}}

function erlang_tools#tmux#run(cmd) " {{{1
    return system('tmux ' . a:cmd)
endfunction " 1}}}

function erlang_tools#tmux#available() " {{{1
    if !exists('g:erlang_tools#tmux')
        if empty(matchstr(system('echo $TMUX'), '^\_s*$'))
            let tmux_version = erlang_tools#tmux#run('-V')
            let list = matchlist(tmux_version, '^tmux \(\d\+\).\(\d\+\)')
            if !empty(list) && list[1] >= 1 && list[2] >= 6
                let g:erlang_tools#tmux = 1
            else
                let g:erlang_tools#tmux = 0
            endif
        else
            let g:erlang_tools#tmux = 0
        endif
    endif
    return g:erlang_tools#tmux
endfunction " 1}}}

function erlang_tools#tmux#vim_pane() " {{{1
    if !exists('g:erlang_tools#tmux#vim')
        let pane = erlang_tools#tmux#run('display-message -p ''#S:#I.#P''')
        let g:erlang_tools#tmux#vim = erlang_tools#tmux#pane_id(pane)
    endif
    return g:erlang_tools#tmux#vim
endfunction " 1}}}

function erlang_tools#tmux#pane_id(pane) " {{{1
    let list1 = matchlist(a:pane, '^\(.*\)\.\(\d\+\)\_s$')
    if empty(list1)
        echomsg 'No such pane:' pane
        return ''
    else
        let cmd = join([
                    \   'list-panes -t ' . list1[1],
                    \   '-F "#P-#{pane_id}"'
                    \])
        let panes = erlang_tools#tmux#run(cmd)
        let list2 = matchlist(panes, '[^\d]*' . list1[2] . '-\(%\d\+\)\n')
        if empty(list2)
            echomsg 'No such pane:' pane
            return ''
        else
            return list2[1]
        endif
    endif
endfunction " 1}}}

" erlang_tools#tmux#slave {{{1

function erlang_tools#tmux#slave_pane() " {{{2
    if !exists('g:erlang_tools#tmux#slave') || empty(g:erlang_tools#tmux#slave)
        let vim_pane = erlang_tools#tmux#vim_pane()
        if empty(vim_pane)
            let g:erlang_tools#tmux#slave = ''
        else
            let cmd = join([
                        \   'split -t ' . vim_pane,
                        \   '-' . g:erlang_tools#tmux#slave_split,
                        \   '-d',
                        \   '-P',
                        \   '-p ' . g:erlang_tools#tmux#slave_size
                        \   ])
            let pane = erlang_tools#tmux#run(cmd)
            if !empty(matchstr(pane, 'not found: ' . vim_pane . '\n$'))
                let g:erlang_tools#tmux#slave = ''
            else
                let g:erlang_tools#tmux#slave = erlang_tools#tmux#pane_id(pane)
            endif
        endif
    endif
    if empty(g:erlang_tools#tmux#slave)
        echomsg 'Failed to start slave'
    endif
    return g:erlang_tools#tmux#slave
endfunction " 2}}}

function erlang_tools#tmux#slave_run(cmd) " {{{2
    let pane = erlang_tools#tmux#slave_pane()
    if empty(pane)
        echomsg 'No such pane:' pane
        return -1
    else
        let prepare_cmd = join([
                    \   'send-keys -R -t ' . pane,
                    \   'C-c C-c',
                    \   shellescape('cd ' . fnameescape(getcwd())) . ' Enter',
                    \   shellescape('clear') . ' Enter'
                    \   ])
        call erlang_tools#tmux#run(prepare_cmd)
        call erlang_tools#tmux#run('clear-history -t ' . pane)
        let cmd = join([
                    \   'send-keys -t ' . pane,
                    \   shellescape(a:cmd) . ' Enter'
                    \   ])
        let result = erlang_tools#tmux#run(cmd)
        if !empty(matchstr(pane, 'not found: ' . pane . '\n$'))
            echomsg 'No such pane:' pane
            return -1
        else
            return 0
        endif
    endif
endfunction " 2}}}

function erlang_tools#tmux#slave_contents() " {{{2
    if !exists('g:erlang_tools#tmux#slave')
        echomsg 'No slave pane'
        return ''
    else
        let pane = g:erlang_tools#tmux#slave
        let limit = max([abs(g:erlang_tools#tmux#slave_history_limit + 0), 1])
        let cmd = join([
                    \   'capture-pane -t ' . pane,
                    \   '-S -' . limit,
                    \   '-E ' . limit
                    \   ])
        let result = erlang_tools#tmux#run(cmd)
        if !empty(matchstr(result, 'not found: ' . pane . '\n$'))
            echomsg 'No such pane:' pane
            return ''
        else
            let outputfile = tempname()
            let cmd2 = 'save-buffer ' . fnameescape(outputfile)
            let result = erlang_tools#tmux#run(cmd2)
            if !empty(matchstr(result, '^no buffers\n$'))
                echomsg 'No such buffer:' 0
                return ''
            else
                call erlang_tools#tmux#run('delete-buffer')
                return erlang_tools#read_file(getcwd(), outputfile)
            endif
        endif
    endif
endfunction " 2}}}

function erlang_tools#tmux#slave_close() " {{{2
    if exists('g:erlang_tools#tmux#slave')
        let pane = g:erlang_tools#tmux#slave
        unlet g:erlang_tools#tmux#slave
        return -1 * !empty(erlang_tools#tmux#run('kill-pane -t ' . pane))
    else
        return 0
    endif
endfunction " 2}}}

" 1}}}

function erlang_tools#tmux#cleanup() " {{{1
    if g:erlang_tools#tmux#slave_auto_close
        return erlang_tools#tmux#slave_close()
    else
        return 0
    endif
endfunction " 1}}}
