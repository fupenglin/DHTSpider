%%%-------------------------------------------------------------------
%%% @author linfupeng
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 三月 2017 下午10:28
%%%-------------------------------------------------------------------
-module(dht_bencode).
-author("linfupeng").

%% API
-export([decode/1, encode/1]).

decode(Data) ->
    try
        {Res, _} = dec(Data),
        {ok, Res}
    catch
        _:Reason -> {error, Reason}
    end.

encode(Data) ->
    try
        {ok, list_to_binary(enc(Data))}
    catch
        _:_ -> {error, 'encode error'}
    end.


%%  Internal functions
dec(<<$i, Tail/binary>>) ->
    dec_int(Tail, []);
dec(<<$l, Tail/binary>>) ->
    dec_list(Tail, []);
dec(<<$d, Tail/binary>>) ->
    dec_dict(Tail, dict:new());
dec(Tail) ->
    dec_str(Tail, []).

%% decode Integer Data.
dec_int(<<$e, Tail/binary>>, Temp) ->
    Int = list_to_integer(lists:reverse(Temp)),
    {Int, Tail};
dec_int(<<X/integer, Tail/binary>>, Temp) ->
    dec_int(Tail, [X | Temp]).

%% decode List Data.
dec_list(<<$e, Tail/binary>>, Temp) ->
    {{list, lists:reverse(Temp)}, Tail};
dec_list(Data, Temp) ->
    {Res, Tail} = dec(Data),
    dec_list(Tail, [Res | Temp]).

%% decode Dict Data.
dec_dict(<<$e, Tail/binary>>, Dict) ->
    {{dict, Dict}, Tail};
dec_dict(Data, Dict) ->
    {Key, Tail} = dec(Data),
    {Value, Tail1} = dec(Tail),
    dec_dict(Tail1, dict:store(Key, Value, Dict)).

%% decode String Data.
dec_str(<<$:, Tail/binary>>, Temp) ->
    Int = list_to_integer(lists:reverse(Temp)),
    <<Str:Int/binary, Tail1/binary>> = Tail,
    {Str, Tail1};
dec_str(<<X/integer, Tail/binary>>, Temp) ->
    dec_str(Tail, [X | Temp]).

%% encode
enc(Int) when erlang:is_integer(Int) ->
    [$i, list_to_binary(integer_to_list(Int)), $e];
enc(Str) when erlang:is_list(Str) ->
    StrLen = length(Str),
    [list_to_binary(integer_to_list(StrLen)), $:, list_to_binary(Str)];
enc(Str) when erlang:is_binary(Str) ->
    StrLen = size(Str),
    [list_to_binary(integer_to_list(StrLen)), $:, Str];
enc({list, List}) when erlang:is_list(List) ->
    [$l, [enc(Temp) || Temp <- List], $e];
enc({dict, Dict}) ->
    [$d, [[enc(Key), enc(Value)] || {Key, Value} <- lists:keysort(1, dict:to_list(Dict))], $e].