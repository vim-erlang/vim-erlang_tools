-module(vet_eunit_listener).

-behaviour(eunit_listener).

-export([start/1]).

-export([init/1,
         handle_begin/3,
         handle_end/3,
         handle_cancel/3,
         terminate/2]).

-record(state, {log :: vet_io:io_device(),
                sources = dict:new() :: dict(),
                groups = dict:new() :: dict()}).

start(Opts) ->
    eunit_listener:start(?MODULE, Opts).

init(_Opts) ->
    {ok, Log} = vet_io:open("vet.log"),
    #state{log=Log}.

handle_begin(group, Data, State = #state{groups=Groups}) ->
    {id, Id} = lists:keyfind(id, 1, Data),
    case lists:keyfind(desc, 1, Data) of
        {desc, ""} ->
            Groups2 = dict:store(Id, undefined, Groups),
            State#state{groups=Groups2};
        {desc, Desc} ->
            Groups2 = dict:store(Id, Desc, Groups),
            State#state{groups=Groups2};
        error ->
            State
    end;
handle_begin(test, Data, State = #state{sources=Sources}) ->
    Module = get_module(Data),
    case dict:is_key(Module, Sources) of
        true ->
            State;
        false ->
            File = vet_util:module_to_source(Module),
            Sources2 = dict:store(Module, File, Sources),
            State#state{sources=Sources2}
    end.

handle_end(group, Data, State = #state{log=Log}) ->
    vet_io:write(Log, Data),
    State;
handle_end(test, Data, State) ->
    case lists:keyfind(status, 1, Data) of
        {status, ok} ->
            State;
        {status, {skipped, Reason}} ->
            handle_error(skipped, Reason, Data, State);
        {status, {error, Reason}} ->
            handle_error(failed, Reason, Data, State)
    end.

handle_cancel(group, Data, State = #state{log=Log}) ->
    vet_io:write(Log, Data),
    State;
handle_cancel(test, Data, State) ->
    {reason, Reason} = lists:keyfind(reason, 1, Data),
    handle_error(skipped, Reason, Data, State).

terminate(Data, #state{log=Log}) ->
    vet_io:write(Log, Data),
    vet_io:close(Log).

%% internal

handle_error(ErrorType, Reason, Data, State = #state{log=Log, sources=Sources,
                                                     groups=Groups}) ->
    Module = get_module(Data),
    File = dict:fetch(Module, Sources),
    Line = proplists:get_value(line, Data, none),
    Reason2 = format_reason(ErrorType, Reason, Data, Groups),
    vet_io:format(Log, File, Line, ErrorType, Reason2),
    State.

get_module(Data) ->
    {source, {Module, _Function, _Arity}} = lists:keyfind(source, 1, Data),
    Module.

format_reason(ErrorType, Reason, Data, Groups) ->
    {source, {_Module, Function, Arity}} = lists:keyfind(source, 1, Data),
    {id, Id} = lists:keyfind(id, 1, Data),
    Group = get_group_desc(Id, Groups),
    vet_io:format_reason(Group, Function, Arity, {ErrorType, Reason}).

get_group_desc(Id, Groups) ->
    [_|GroupIdRev] = lists:reverse(Id),
    GroupId = lists:reverse(GroupIdRev),
    case dict:find(GroupId, Groups) of
        {ok, undefined} ->
            undefined;
        {ok, Desc} ->
            {group, Desc};
        error ->
            undefined
    end.
