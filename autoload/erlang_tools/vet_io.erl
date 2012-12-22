-module(vet_io).

-export([open/0,
         open/1,
         write/2,
         format/5,
         close/1]).

-export([format_reason/4]).

-record(io_device, {mod :: 'undefined' | 'file',
                    data :: term()}).

-type io_device() :: #io_device{}.
-type line() :: pos_integer() | 'none'.
-type error_type() :: 'error' | 'warning' | 'failed' | 'skipped'.

-spec open() -> {ok, io_device()}.
open() ->
    IoDevice = erlang:group_leader(),
    open(IoDevice).

-spec open(io_device() | file:filename()) -> {ok, io_device()}.
open(Filename) when is_binary(Filename) orelse is_list(Filename) ->
    {ok, IoDevice} = file:open(Filename, [write, delayed_write]),
    Filename2 = filename:absname(Filename),
    io:format("Check output file `~s' for details~n~n", [Filename2]),
    {ok, #io_device{mod=file, data=IoDevice}};
open(IoDevice) ->
    {ok, #io_device{mod=undefined, data=IoDevice}}.

-spec write(io_device(), term()) -> 'ok'.
write(#io_device{data=IoDevice}, Term) ->
    io:write(IoDevice, Term).

-spec format(io_device(), file:filename(), line(), error_type(), iolist()) ->
    'ok'.
format(#io_device{data=IoDevice}, Filename, Line, ErrorType, Desc) ->
    Line2 = format_line(Line),
    ErrorType2 = format_error_type(ErrorType),
    io:format(IoDevice, "~s:~s: ~s~s~n", [Filename, Line2, ErrorType2, Desc]).

-spec close(io_device()) -> 'ok'.
close(#io_device{mod=file, data=IoDevice}) ->
    ok = file:close(IoDevice);
close(_) ->
    ok.

-spec format_reason(atom() | iolist(), atom(), non_neg_integer(), term()) ->
    iolist().
format_reason(Group, Function, Arity, Reason) ->
    Testcase = format_testcase(Group, Function, Arity),
    Reason2 = io_lib:format("~p", [Reason]),
    Reason3 = re:replace(Reason2, "\\s+", "\s", [global, {return, list}]),
    io_lib:format("~s ~s", [Testcase, Reason3]).

%% internal

format_error_type(warning) ->
    "Warning: ";
format_error_type(skipped) ->
    "Skipped: ";
format_error_type(failed) ->
    "Failed: ";
format_error_type(_) ->
    "".

format_line(none) ->
    "none";
format_line(Int) ->
    io_lib:format("~B", [Int]).

format_testcase(undefined, Function, Arity) ->
    format_testcase(Function, Arity);
format_testcase({group, Group}, Function, Arity) ->
    [format_testcase(Function, Arity), io_lib:format(" (~s)", [Group])].

format_testcase(Function, Arity) ->
    io_lib:format("~s/~B", [Function, Arity]).
