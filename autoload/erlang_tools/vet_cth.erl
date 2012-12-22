-module(vet_cth).

%% api

-export([id/1,
         init/2,
         pre_init_per_suite/3,
         post_init_per_suite/4,
         pre_end_per_suite/3,
         post_end_per_suite/4,
         pre_init_per_group/3,
         post_init_per_group/4,
         pre_end_per_group/3,
         post_end_per_group/4,
         pre_init_per_testcase/3,
         post_end_per_testcase/4,
         on_tc_fail/3,
         on_tc_skip/3,
         terminate/1]).

-record(state,  {log :: vet_io:io_device(),
                 suite = undefined :: 'undefined' | {'suite', atom()},
                 file = undefined :: 'undefined' | string(),
                 dict = undefined :: 'undefined' | dict(),
                 group = undefined :: 'undefined' | {'group', atom()},
                 failed = 0 :: non_neg_integer(),
                 skipped = 0 :: non_neg_integer(),
                 total = 0 :: non_neg_integer()}).

id(_Opts) ->
    "vet.log".

init(Id, _Opts) ->
    {ok, Log} = vet_io:open(Id),
    {ok, #state{log=Log, total=0}}.

pre_init_per_suite(Suite, Config, State) ->
    File = vet_util:module_to_source(Suite),
    Dict = vet_util:module_to_dict(Suite),
    {Config, State#state{suite={suite, Suite}, file=File, dict=Dict}}.

post_init_per_suite(_Suite, _Config, Return, State) ->
    {Return, State}.

pre_end_per_suite(_Suite, Config, State) ->
    {Config, State}.

post_end_per_suite(_Suite, _Config, Return, State) ->
    {Return, State#state{suite=undefined, file=undefined, dict=undefined}}.

pre_init_per_group(Group, Config, State) ->
    {Config, State#state{group={group, Group}}}.

post_init_per_group(_Group, _Config, Return, State) ->
    {Return, State}.

pre_end_per_group(_Group, Config, State) ->
    {Config, State}.

post_end_per_group(_Group, _Config, Return, State) ->
    {Return, State#state{group=undefined}}.

pre_init_per_testcase(_TC, Config, State = #state{total=Total}) ->
    {Config, State#state{total=(Total+1)}}.

post_end_per_testcase(_TC, _Config, Return, State) ->
  {Return, State}.

on_tc_fail(TC, Reason, State = #state{suite={suite, _Suite}, file=File,
                                      dict=Dict, group=Group, failed=Failed,
                                      log=Log}) ->
    write(Log, Dict, failed, File, Group, TC, Reason),
    State#state{failed=(Failed+1)}.

on_tc_skip(TC, Reason, State = #state{suite={suite, _Suite}, file=File,
                                      dict=Dict, group=Group, skipped=Skipped,
                                      log=Log}) ->
    write(Log, Dict, skipped, File, Group, TC, Reason),
    State#state{skipped=(Skipped+1)}.

terminate(#state{log=Log}) ->
    vet_io:close(Log).

%% internal


write(Log, Dict, ErrorType, File, Group, TC, Reason) ->
    Line = get_line(TC, Dict),
    Arity = get_arity(TC),
    Reason2 = vet_io:format_reason(Group, TC, Arity, Reason),
    vet_io:format(Log, File, Line, ErrorType, Reason2).

get_line(TC, Dict) ->
    case dict:find(TC, Dict) of
        {ok, Line} ->
            Line;
        _error ->
            none
    end.

get_arity(TC) when TC == init_per_group orelse TC == end_per_group ->
    2;
get_arity(_TC) ->
    1.
