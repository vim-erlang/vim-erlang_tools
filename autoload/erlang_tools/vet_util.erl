-module(vet_util).

-export([module_to_source/1,
         module_to_dict/1]).

-spec module_to_source(module()) -> file:filename().
module_to_source(Module) ->
    case beam_to_source(Module) of
        File when is_list(File) ->
            File;
        error ->
            File = atom_to_list(Module) ++ ".erl",
            where_is_file(File)
    end.

-spec module_to_dict(module()) -> dict().
module_to_dict(Module) ->
    case beam_lookup(Module, abstract_code, raw_abstract_v1) of
        {raw_abstract_v1, AbsForm} ->
            abstract_form_to_dict(AbsForm);
        error ->
            File = atom_to_list(Module) ++ ".erl",
            File2 = where_is_file(File),
            source_to_dict(File2)
    end.

%% internal

beam_to_source(Beam) ->
    case beam_lookup(Beam, compile_info, source) of
        {source, File} ->
            File;
        error ->
            error
    end.

beam_lookup(Beam, Chunk, Key) when is_atom(Beam) ->
    Beam2 = atom_to_list(Beam) ++ ".beam",
    beam_lookup(Beam2, Chunk, Key);
beam_lookup(Beam, Chunk, Key) ->
    File = where_is_file(Beam),
    case beam_lib:chunks(File, [Chunk]) of
        {ok, {_, [{Chunk, Result}]}} ->
            lists:keyfind(Key, 1, Result);
        _ ->
            error
    end.

where_is_file(File) ->
    case code:where_is_file(File) of
        File2 when is_list(File2) ->
            File2;
        _ ->
            File
    end.

abstract_form_to_dict(AbsForm) ->
    List = [{Testcase, Line} || {function, Line, Testcase, 1, _} <- AbsForm,
                                Testcase /= init_per_group orelse
                                Testcase /= end_per_group],
    List2 = [{Function, Line} || {function, Line, Function, 2, _} <- AbsForm,
                              Function == init_per_group orelse
                              Function == end_per_group],
    dict:from_list(List2 ++ List).

source_to_dict(File) ->
    case epp:parse_file(File, [], []) of
        {ok, AbsForm} ->
            abstract_form_to_dict(AbsForm);
        {error, _Reason} ->
            dict:new()
    end.
