" defaults {{{1

if exists('g:erlang_tools#autoloaded')
    finish
endif
let g:erlang_tools#autoloaded = 1

if !exists('g:erlang_tools#check_tool')
    let g:erlang_tools#check_tool = 'vimerl_check'
endif

if !exists('g:erlang_tools#check_dict')
    let g:erlang_tools#check_dict = {}
endif

if !exists('g:erlang_tools#check_auto_open')
    let g:erlang_tools#check_auto_open = 0
endif

if !exists('g:erlang_tools#loclist')
    let g:erlang_tools#loclist = 1
endif

if !exists('g:erlang_tools#loclist_dict')
    let g:erlang_tools#loclist_dict = {}
endif

if !exists('g:erlang_tools#qf_auto_open')
    let g:erlang_tools#qf_auto_open = 1
endif

if !exists('g:erlang_tools#qf_dict')
    let g:erlang_tools#qf_dict = {}
endif

if !exists('g:erlang_tools#signs')
    let g:erlang_tools#signs = 1
endif

if !exists('g:erlang_tools#signs_dict')
    let g:erlang_tools#signs_dict = {}
endif

" 1}}}

" erlang_tools#slave {{{1

function erlang_tools#slave_run(tool, opts) " {{{2
    let cwd = getcwd()
    call erlang_tools#history#add(a:tool, cwd, a:opts)
    let prg = g:erlang_tools#{a:tool}#prg
    if empty(prg)
        echomsg 'No program for:' a:tool
        return -1
    else
        let opts = erlang_tools#{a:tool}#opts(a:opts)
        let cmd = empty(opts) ? prg : prg . ' ' . opts
        if !erlang_tools#tmux#available()
            echomsg 'Require tmux 1.6 or greater'
            return -1
        else
            return erlang_tools#tmux#slave_run(cmd)
        endif
    endif
endfunction " 2}}}

function erlang_tools#slave_rerun(tool, position) " {{{2
    let past_run = erlang_tools#history#get(a:tool, a:position)
    if empty(past_run)
        let desc = empty(a:tool) ? a:position : a:tool . ' ' . a:position
        echomsg 'No such previous run:' desc
        return -1
    else
        let [cwd, opts] = last_run[1:2]
        let current_dir = getcwd()
        if cwd != current_dir
            execute 'lcd ' . cwd
        endif
        let result = erlang_tools#slave_run(a:tool, opts)
        if cwd != current_dir
            execute 'lcd ' . current_dir
        endif
        return result
    endif
endfunction " 2}}}

function erlang_tools#slave_parse(tool, cwd) " {{{2
    if empty(a:tool)
        let last_run = erlang_tools#history#get('', 0)
        if empty(last_run)
            echomsg 'No previous run'
            return -1
        else
            let [tool, cwd] = last_run[0:1]
        endif
    else
        let tool = a:tool
        let cwd =  empty(a:cwd) ? getcwd() : a:cwd
    endif
    let output = erlang_tools#tmux#slave_contents()
    if empty(output)
        return 0
    else
        let qf_list = erlang_tools#parse_output(tool, cwd, output)
        return erlang_tools#qf_add(tool, cwd, qf_list)
    endif
endfunction " 2}}}

" 1}}}

function erlang_tools#show_history(tool) " {{{1
    let history_list = erlang_tools#history#all(a:tool)
    if empty(history_list)
        let output = 'No previous runs'
        let output .= empty(a:tool) ? '' : ': ' . a:tool
    else
        let output_list = []
        let position = 0
        for item in history_list
            let [tool, cwd, opts] = item
            let text = position . ' ' . tool . ' ' . opts
            if cwd != getcwd()
                let text.= ' (' . cwd . ')'
            endif
            call add(output_list, text)
            let position += 1
        endfor
        let output = join(output_list, "\n")
    endif
        echo output
    return output
endfunction " 1}}}

function erlang_tools#parse_output(tool, cwd, output) " {{{1
    try
        let result = erlang_tools#{a:tool}#parse_output(a:cwd, a:output)
    catch /E117/    "Unknown function
        let result = erlang_tools#parse#output(a:cwd, a:output)
    endtry
    return result
endfunction " 1}}}

function erlang_tools#external_parse(tool, cwd, output) " {{{1
    let qf_list = erlang_tools#parse_output(a:tool, a:cwd, a:output)
    return erlang_tools#qf_add(a:tool, a:cwd, qf_list)
endfunction " 1}}}

function erlang_tools#read_file(cwd, fname) "{{{1
    let current_cwd = getcwd()
    if a:cwd != current_cwd && !isdirectory(a:cwd)
        echomsg 'No such directory' a:cwd
        let output = []
    elseif a:cwd != current_cwd
        execute 'lcd' a:cwd
        let output = readfile(a:fname)
        execute 'lcd' current_cwd
    else
        let output = readfile(a:fname)
    endif
    return output
endfunction " 1}}}

" erlang_tools#check {{{1

function erlang_tools#check_run(bufnr) " {{{2
    let tool = g:erlang_tools#check_tool
    if empty(tool)
        return 0
    else
        let prg = g:erlang_tools#{tool}#prg
        if empty(prg)
            echomsg 'No program for: ' . tool
            return -1
        else
            let fname = bufname(a:bufnr)
            let opts = erlang_tools#{tool}#opts(fname)
            let cmd = prg . ' ' . opts
            let output = system(cmd)
            let cwd = getcwd()
            let qf_list = erlang_tools#parse_output(tool, cwd, output)
            let result = erlang_tools#check_add(a:bufnr, qf_list)
            if g:erlang_tools#check_auto_open && result
                lopen
            endif
            return result
        endif
    endif
endfunction " 2}}}

function erlang_tools#check_add(bufnr, qf_list) " {{{2
    echomsg type(a:bufnr)
    let fname = bufname(a:bufnr)
    let qf_list = []
    for qf_item in a:qf_list
        if qf_item['filename'] == fname
            let qf_item['bufnr'] = a:bufnr
            call add(qf_list, qf_item)
        endif
    endfor
    let g:erlang_tools#check_dict[a:bufnr] = qf_list
    call erlang_tools#refresh(a:bufnr, 0)
    return !empty(qf_list)
endfunction " 2}}}

function erlang_tools#check_get(bufnr) " {{{2
    return get(g:erlang_tools#check_dict, a:bufnr, [])
endfunction " 2}}}

function erlang_tools#check_remove(bufnr) " {{{2
    if has_key(g:erlang_tools#check_dict, bufnr)
        return remove(g:erlang_tools#check_dict, bufnr)
    else
        return []
    endif
endfunction " 2}}}

" 1}}}

" erlang_tools#qf {{{1

function erlang_tools#qf_delete(tools) " {{{2
    if empty(a:tools)
        let tools = keys(g:erlang_tools#qf_dict)
    else
        let tools = a:tools
    endif
    let bufs = []
    for tool in tools
        if has_key(g:erlang_tools#qf_dict, tool)
            for bufnr in keys(g:erlang_tools#qf_dict[tool])
                if index(bufs, bufnr) == -1
                    call add(bufs, bufnr)
                endif
            endfor
            unlet g:erlang_tools#qf_dict[tool]
        endif
    endfor
    for bufnr in bufs
        call erlang_tools#refresh(bufnr, 0)
    endfor
    return 0
endfunction " 2}}}


function erlang_tools#qf_select(tool, tools) " {{{2
    if empty(a:tools)
        let tools = []
        for tool in keys(g:erlang_tools#qf_dict)
            if tool != a:tool
                call add(tools, tool)
            endif
        endfor
    else
        let tools = a:tools
    endif
    let merged_dict = {}
    let merged_list = []
    let bufs = []
    for tool in tools
        let qf_dict = deepcopy(get(g:erlang_tools#qf_dict, tool, {}))
        for [bufnr, qf_list] in items(qf_dict)
            let current = get(merged_dict, bufnr, [])
            call extend(current, qf_list)
            let merged_dict[bufnr] = current
            call extend(merged_list, qf_list)
            if index(bufs, bufnr) == -1
                call add(bufs, bufnr)
            endif
        endfor
    endfor
    let g:erlang_tools#qf_dict[a:tool] = merged_dict
    for bufnr in bufs
        call erlang_tools#refresh(bufnr, 0)
    endfor
    call setqflist(merged_list)
    if !empty(merged_list) && g:erlang_tools#qf_auto_open
        copen
    endif
    return !empty(merged_list)
endfunction " 2}}}

function erlang_tools#qf_add(tool, cwd, qf_list) " {{{2
    let [qf_list, qf_dict] = erlang_tools#qf_parse(a:cwd, a:qf_list)
    let bufs = keys(qf_dict)
    let bufs_prev = keys(get(g:erlang_tools#qf_dict, a:tool, {}))
    for bufnr in bufs_prev
        if index(bufs, bufnr) == -1
            call add(bufs, bufnr)
        endif
    endfor
    let g:erlang_tools#qf_dict[a:tool] = qf_dict
    for bufnr in bufs
        call erlang_tools#refresh(bufnr, 0)
    endfor
    call setqflist(qf_list)
    if !empty(qf_list) && g:erlang_tools#qf_auto_open
        copen
    endif
    return !empty(qf_list)
endfunction " 2}}}

function erlang_tools#qf_parse(cwd, qf_list) " {{{2
    let qf_dict = {}
    let qf_list = []
    let fname_info_cache = {}
    let invalid_files = 0
    for qf_item in a:qf_list
        let fname_info = get(fname_info_cache, qf_item['filename'], [])
        if empty(fname_info)
            let fname = qf_item['filename']
            let fname_info = erlang_tools#qf_fname_info(a:cwd, fname)
            let fname_info_cache[fname] = fname_info
        endif
        let bufnr = fname_info[1]
        if bufnr != -1
            let qf_item['filename'] = fname_info[0]
            let qf_item['bufnr'] = bufnr
            call add(qf_list, qf_item)
            let qf_dict[bufnr] = get(qf_dict, bufnr, []) + [qf_item]
        else
            let invalid_files += 1
        endif
    endfor
    if invalid_files
        echomsg 'Unlisted errors: ' . invalid_files
    endif
    return [qf_list, qf_dict]
endfunction " 2}}}

function erlang_tools#qf_fname_info(cwd, fname) " {{{2
    let fname = simplify(a:fname)
    let current_cwd = getcwd()
    if a:cwd != current_cwd && isdirectory(a:cwd)
        execute 'lcd' a:cwd
        let bufnr = erlang_tools#qf_bufnr(fname)
        execute 'lcd' current_cwd
    elseif a:cwd == current_cwd
        let bufnr = erlang_tools#qf_bufnr(fname)
    else
        let bufnr = -1
    endif
    if bufnr == -1
        echomsg 'No such file: ' . fname
        return [fname, bufnr]
    else
        return [bufname(bufnr), bufnr]
    endif
endfunction " 2}}}

function erlang_tools#qf_bufnr(fname) " {{{2
    try
        let file_exists = empty(readfile(a:fname, '', 0))
    catch /.*/
        let file_exists = 0
    endtry
    if file_exists
        return bufnr(a:fname, 1)
    else
        return -1
    endif
endfunction " 2}}}

" 1}}}

function erlang_tools#refresh(bufnr, use_cache) " {{{1
    let qf_list = erlang_tools#loclist_set(a:bufnr, a:use_cache)
    return erlang_tools#signs_place(a:bufnr, qf_list)
endfunction " 1}}}

" erlang_tools#loclist {{{1

function erlang_tools#loclist_set(bufnr, use_cache) " {{{2
    if a:use_cache
        let qf_list = get(g:erlang_tools#loclist_dict, a:bufnr, [])
    else
        let qf_list = copy(erlang_tools#check_get(a:bufnr))
        for qf_dict in values(g:erlang_tools#qf_dict)
            call extend(qf_list, get(qf_dict, a:bufnr, []))
        endfor
        call sort(qf_list, 'erlang_tools#loclist_sort')
        let g:erlang_tools#loclist_dict[a:bufnr] = qf_list
    endif
    if g:erlang_tools#loclist
        for winnr in range(1, winnr('$'))
            if winbufnr(winnr) == a:bufnr
                call setloclist(winnr, qf_list)
            endif
        endfor
    endif
    return qf_list
endfunction " 2}}}

function erlang_tools#loclist_sort(qf_item_a, qf_item_b) " {{{2
    let lnum_a = a:qf_item_a['lnum'] + 0
    let lnum_b = a:qf_item_b['lnum'] + 0
    return lnum_a == lnum_b ? 0 : lnum_a > lnum_b ? 1 : -1
endfunction " 2}}}

" 1}}}

" erlang_tools#signs {{{1

" sign define {{{2
sign define ErlangToolsError text=>> texthl=Error
sign define ErlangToolsWarning text=>> texthl=Todo
sign define ErlangToolsFailed text=>> texthl=Error
sign define ErlangToolsSkipped text=>> texthl=Error
" 2}}}

function erlang_tools#signs_place(bufnr, qf_list) " {{{2
    let max_sign_id = get(g:erlang_tools#signs_dict, a:bufnr, 0)
    for sign_id in range(1, max_sign_id)
        execute 'sign unplace' sign_id 'buffer=' . a:bufnr
    endfor
    let sign_id = 0
    if g:erlang_tools#signs
        for qf_item in a:qf_list
            let sign_id += 1
            execute join([
                        \   'sign place ' . sign_id,
                        \   'name=ErlangTools' . qf_item['typename'],
                        \   'line=' . qf_item['lnum'],
                        \   'buffer=' . a:bufnr
                        \])
        endfor
    endif
    let g:erlang_tools#signs_dict[a:bufnr] = sign_id
    return !!sign_id
endfunction " 2}}}

" 1}}}

