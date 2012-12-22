" defaults {{{1

if exists('g:erlang_tools#history#autoloaded')
    finish
endif
let g:erlang_tools#history#autoloaded = 1

if !exists('g:erlang_tools#history#limit')
    let g:erlang_tools#history#limit = 10
endif

if !exists('g:erlang_tools#history#tool_dict')
    let g:erlang_tools#history#tool_dict = {}
endif

if !exists('g:erlang_tools#history#list')
    let g:erlang_tools#history#list = []
endif

" 1}}}

function erlang_tools#history#add(tool, cwd, opts) " {{{1
    let limit = g:erlang_tools#history#limit
    let global_history = g:erlang_tools#history#list
    call insert(global_history, [a:tool, a:cwd, a:opts])
    let g:erlang_tools#history#list = global_history[:limit-1]
    let tool_history = get(g:erlang_tools#history#tool_dict, a:tool, [])
    call insert(tool_history, [a:tool, a:cwd, a:opts])
    let g:erlang_tools#history#tool_dict[a:tool] = tool_history[:limit-1]
    return deepcopy(g:erlang_tools#history#list)
endfunction " 1}}}

function erlang_tools#history#get(tool, position) " {{{1
    if empty(a:tool)
        let history_list = g:erlang_tools#history#list
    else
        let history_list = get(g:erlang_tools#history#tool_dict, a:tool, [])
    endif
    try
        let item = history_list[a:position]
    catch /E684:/
        let item = []
    endtry
    return item
endfunction " 1}}}

function erlang_tools#history#all(tool) " {{{1
    if empty(a:tool)
        return g:erlang_tools#history#list
    else
        return get(g:erlang_tools#history#tool_dict, a:tool, [])
    endif
endfunction " 1}}}

function erlang_tools#history#clear() " {{{1
    let g:erlang_tools#history#list = []
    let g:erlang_tools#history#tool_dict = {}
    return []
endfunction " 1}}}
