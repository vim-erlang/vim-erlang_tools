-module(vet_eunit).

-export([start/0]).

-spec start() -> 'ok'.
start() ->
    {ok, Cwd} = file:get_cwd(),
    io:format(user, "Eunit (via vim-erlang_tools) starting (cwd in ~s)~n~n",
              [Cwd]),
    Args = init:get_arguments(),
    true = replace_paths(Args),
    Tests = get_tests(Args),
    Options = get_options(Args),
    Cover = cover_start(Args, Tests),
    true = cd_to_logdir(Args),
    eunit:test(Tests, Options),
    ok = cover_stop(Cover).

%% internal

replace_paths(Args) ->
    true = code:add_patha(filename:absname(".")),
    Pas = get_pathas(Args),
    true = replace_paths(Pas, add_patha),
    Pzs = get_pathzs(Args),
    true = replace_paths(Pzs, add_pathz).

replace_paths(Paths, Fun) ->
    _ = [replace_path(Dir, Fun) || Dir <- Paths,
                                   filename:pathtype(Dir) /= absolute],
    true.

replace_path(Dir, Fun) ->
    Dir2 = filename:absname(Dir),
    io:format(user, "Converting \"~s\" to \"~s\" and re-inserting with ~s/1~n",
              [Dir, Dir2, Fun]),
    true = code:del_path(Dir),
    true = code:Fun(Dir2).

get_pathas(Args) ->
    get_paths(pa, Args).

get_pathzs(Args) ->
    get_paths(pz, Args).

get_paths(Flag, Args) ->
    [lists:flatten(Path) || Path <- proplists:get_all_values(Flag, Args)].

get_tests(Args) ->
    get_tests(Args, []).

get_tests([], Tests) ->
    lists:reverse(Tests);
get_tests([{dir, Dirs} | Rest], Tests) ->
    Fun = fun(Dir, Acc) ->
            [filename:absname(Dir) | Acc]
        end,
    Tests2 = lists:foldl(Fun, Tests, Dirs),
    get_tests(Rest, Tests2);
get_tests([{module, Modules}, {test, Testcases} | Rest], Tests) ->
    Tests2 = [get_tests_tuple(Module, Testcase) || Module <- Modules,
                                                   Testcase <- Testcases],
    get_tests(Rest, lists:reverse(Tests2, Tests));
get_tests([{module, Modules} | Rest], Tests) ->
    Fun = fun(Module, Acc) ->
            [list_to_atom(Module) | Acc]
        end,
    Tests2 = lists:foldl(Fun, Tests, Modules),
    get_tests(Rest, Tests2);
get_tests([_ | Rest], Tests) ->
    get_tests(Rest, Tests).

get_tests_tuple(Module, Testcase) ->
    Module2 = list_to_atom(Module),
    Testcase2 = list_to_atom(Testcase),
    case lists:reverse(Testcase) of
        [$_,$t,$s, $e, $t | _Rest] ->
            {generator, Module2, Testcase2};
        _ ->
            {test, Module2, Testcase2}
    end.

get_options(Args) ->
    Opts = get_options(Args, []),
    SurefireOpts = get_surefire_options(Args),
    SurefireOpts ++ Opts ++ [{report, {vet_eunit_listener, []}}].

get_options([], Opts) ->
    Opts;
get_options([{verbose, _} | Rest], Opts) ->
    get_options(Rest, [verbose, Opts]);
get_options([_ | Rest], Opts) ->
    get_options(Rest, Opts).

get_surefire_options(Args) ->
    case lists:keymember(surefire, 1, Args) of
        true ->
            [{report, {eunit_surefire, []}}];
        false ->
            []
    end.

cd_to_logdir(Args) ->
    case lists:keyfind(logdir, 1, Args) of
        {logdir, [LogDir | _]} ->
            Dir = filename:absname(LogDir),
            ok = file:set_cwd(Dir);
        A ->
            io:format(user, "~p~n", [A]),
            ok
    end,
    {{Year, Month, Day}, {Hours, Min, Secs}} = calendar:now_to_datetime(now()),
    NameList = io_lib:format("~s.~s.~4..0B-~2..0B-~2..0B_~2..0B.~2..0B.~2..0B",
                         [?MODULE, node(), Year, Month, Day, Hours, Min, Secs]),
    Name = lists:flatten(NameList),
    Dir2 = filename:absname(Name),
    ok = file:make_dir(Dir2),
    ok = file:set_cwd(Dir2),
    io:format(user, "~nCWD set to: \"~s\"~n~n", [Dir2]),
    true.

cover_start(Args, Tests) ->
    case lists:keymember(cover, 1, Args) of
        true ->
            {ok, _Pid} = cover:start(),
            ok = cover_compile(Args, Tests),
            true;
        false ->
            false
    end.

cover_compile(Args, Tests) ->
    CodePaths = get_pathas(Args) ++ get_pathzs(Args) ++
                [Testcase || Testcase <- Tests, is_list(Testcase)],
    _ = [cover_directory(Path) || Path <- CodePaths],
    Modules = [Testcase || Testcase <- Tests, is_atom(Testcase)] ++
              [Module || {Module, _Testcases} <- Tests],
    _ = [cover_module(Module) || Module <- Modules],
    ok.

cover_directory("") ->
    ok;
cover_directory(Dir) ->
    case lists:last(filename:split(Dir)) of
        "ebin" ->
            {ok, Filenames} = file:list_dir(Dir),
            _ = [cover_filename(Filename)  || Filename <- Filenames],
            ok;
        _ ->
            ok
    end.

cover_filename(Filename) ->
    case filename:extension(Filename) of
        ".beam" ->
            ModuleList = filename:rootname(filename:basename(Filename)),
            Module = list_to_atom(ModuleList),
            cover_module(Module);
        _ ->
            ok
    end.

cover_module(Module) ->
    case code:which(Module) of
        Filename when is_list(Filename) ->
            {ok, Module} = cover:compile_beam(Filename),
            ok;
        cover_compiled ->
            ok
    end.

cover_stop(false) ->
    ok;
cover_stop(true) ->
    ok = cover_write(),
    cover:stop().

cover_write() ->
    Modules = cover:modules(),
    _ = [cover_analyse(Module) || Module <- Modules],
    cover_write_recv(length(Modules)).

cover_write_recv(0) ->
    ok;
cover_write_recv(N) ->
    receive
        ok ->
            cover_write_recv(N-1)
    after
        5000 ->
            io:format(user, "Cover analyse timeout~n", [])
    end.

cover_analyse(Module) ->
    Parent = self(),
    Fun = fun() ->
            cover:analyse_to_file(Module),
            Parent ! ok,
            unlink(Parent)
        end,
    spawn_link(Fun).











