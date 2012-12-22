" defaults {{{1

if exists('g:erlang_tools#parse#autoloaded')
    finish
endif
let g:erlang_tools#parse#autoloaded = 1

if !exists('g:erlang_tools#parse#max_depth')
    let g:erlang_tools#parse#max_depth = 2
endif

" }}}

function erlang_tools#parse#output(cwd, output, ...) " {{{1
    if a:0 != 2
        return erlang_tools#parse#output(a:cwd, a:output, [], 0)
    elseif type(a:output) == type('')
        return erlang_tools#parse#output(a:cwd, split(a:output, "\n"), a:1, a:2)
    else
        let qf_list = a:1
        let depth = a:2 + 1
        for line in a:output
            let [qf_item, fname] = erlang_tools#parse#line(line, depth)
            if !empty(qf_item)
                call add(qf_list, qf_item)
            elseif !empty(fname)
                let output = erlang_tools#utils#read_file(a:cwd, fname)
                call erlang_tools#parse#output(a:cwd, output, qf_list, depth)
            endif
        endfor
        return qf_list
    endif
endfunction " 1}}}

function erlang_tools#parse#line(line, depth) " {{{1
    " filename:1|none: (Typename:) Description
    let qf_item = {}
    let fname = ''
    let pat = '^\(\S.*\):\(\(\d\+\)\|none\):\s\=\(\(\(\u\)\a*\):\)\=\s\+\(.*\)$'
    let matchlist = matchlist(a:line, pat)
    if !empty(matchlist)
        let qf_item['filename'] = matchlist[1]
        let qf_item['lnum'] = !empty(matchlist[3]) ? matchlist[3] : '1'
        if matchlist[6] != 'W'
            let qf_item['typename'] = 'Error'
            let qf_item['type'] = 'E'
        else
            let qf_item['typename'] = matchlist[5]
            let qf_item['type'] = matchlist[6]
        endif
        let qf_item['text'] =  matchlist[7]
    elseif a:depth < g:erlang_tools#parse#max_depth
        let pat2 ='^\s*Check output file `\(.\+\)'' for details\s*$'
        let matchlist2 = matchlist(a:line, pat2)
        if !empty(matchlist2)
            let fname = matchlist2[1]
        endif
    endif
    return [qf_item, fname]
endfunction " 1}}}

