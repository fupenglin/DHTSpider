%%%-------------------------------------------------------------------
%%% @author linfupeng
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 三月 2017 下午4:58
%%%-------------------------------------------------------------------
-module(dht_id).
-author("linfupeng").

%% API
-export([to_hex/1, to_integer/1, to_binary/1, min/0, max/0, distance/2, random_id/0]).

-define(MAX_BYTE_INT, 255).
-define(ID_LENGTH, 20).

%% id to hex string to print
to_hex(ID) when is_integer(ID) ->
    BID = <<ID:160>>,
    to_hex(BID);
to_hex(ID) when is_binary(ID) ->
    to_hex(binary_to_list(ID));
to_hex(ID) when is_list(ID) ->
    lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- ID]).

%% min id
min() ->
    0.
%% max id
max() ->
    Max = list_to_binary([?MAX_BYTE_INT || _ <- lists:seq(1, ?MAX_BYTE_INT)]),
    to_integer(Max).

%% binary id or list id to integer id
to_integer(ID) when is_integer(ID) ->
    ID;
to_integer(ID) when is_binary(ID) ->
    <<NewID:160>> = ID,
    NewID;
to_integer(ID) when is_list(ID) ->
    to_integer(list_to_binary(ID)).

%% convert integer id or list id to binary id
to_binary(ID) when is_binary(ID) ->
    ID;
to_binary(ID) when is_integer(ID) ->
    <<ID:160>>;
to_binary(ID) when is_list(ID) ->
    list_to_binary(ID).

%% calculate the distance between ID0 and ID1
distance(ID0, ID1) when is_integer(ID0) and is_integer(ID1) ->
    ID0 bxor ID1;
distance(ID0, ID1) when is_binary(ID0) and is_binary(ID1) ->
    distance(to_integer(ID0), to_integer(ID1));
distance(_ID0, _ID1) ->
    max().

%% generate a random integer id
random_id() ->
    ID = [random:uniform(?MAX_BYTE_INT + 1) - 1 || _ <- lists:seq(1, ?ID_LENGTH)],
    to_integer(ID).
