%%%-------------------------------------------------------------------
%%% @author fupeng.lin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 三月 2017 20:05
%%%-------------------------------------------------------------------
-module(dht_msg).
-author("fupeng.lin").

-include("dht_common.hrl").

-define(KEY_T, <<"t">>).
-define(KEY_Y, <<"y">>).
-define(KEY_Q, <<"q">>).
-define(KEY_A, <<"a">>).
-define(KEY_R, <<"r">>).
-define(KEY_E, <<"e">>).
-define(KEY_ID, <<"id">>).
-define(KEY_NODES, <<"nodes">>).
-define(KEY_PORT, <<"port">>).
-define(KEY_TOKEN, <<"token">>).
-define(KEY_TARGET, <<"target">>).
-define(KEY_INFO_HASH, <<"info_hash">>).
-define(KEY_IMPLIED_PORT, <<"implied_port">>).

-define(MSG_TYPE_PING, <<"ping">>).
-define(MSG_TYPE_FIND_NODE, <<"find_node">>).
-define(MSG_TYPE_GET_PEERS, <<"get_peers">>).
-define(MSG_TYPE_ANNOUNCE_PEER, <<"announce_peer">>).

-define(QUERY_MSG_KEY, [?KEY_T, ?KEY_Y, ?KEY_Q, ?KEY_A]).
-define(RESPONSE_MSG_KEY, [?KEY_T, ?KEY_Y, ?KEY_R]).
-define(ERROR_MSG_KEY, [?KEY_T, ?KEY_Y, KEY_E]).

%% API
-export([decode/1, encode_query/1, encode_response/1]).

decode(RawData) ->
    case dht_bencode:decode(RawData) of
        {ok, Msg} ->
            decode_msg(Msg);
        {error, Reason} ->
            {error, Reason}
    end.

encode_query(Msg) ->
    DHTMsg = encode_query_msg(Msg),
    case dht_bencode:encode(DHTMsg) of
        {ok, BenMsg} -> {ok, BenMsg};
        {error, Reason} -> {error, Reason}
    end.

encode_response(Msg) ->
    DHTMsg= encode_response_msg(Msg),
    case dht_bencode:encode(DHTMsg) of
        {ok, BenMsg} -> {ok, BenMsg};
        {error, Reason} -> {error, Reason}
    end.

%% 内部函数
decode_msg({dict, DHTMsg}) ->
    try
        case dict:find(?KEY_Y, DHTMsg) of
            {ok, ?KEY_Q} ->
                case check_msg(?QUERY_MSG_KEY, DHTMsg) of
                    true ->
                        {ok, Tid} = dict:find(?KEY_T, DHTMsg),
                        {ok, Type} = dict:find(?KEY_Q, DHTMsg),
                        {ok, Args} = decode_msg_args(Type, DHTMsg),
                        {ok, {Tid, msg_type(Type), Args}};
                    false ->
                        {error, "unknown dht msg"}
                end;
            {ok, ?KEY_R} ->
                case check_msg(?RESPONSE_MSG_KEY, DHTMsg) of
                    true ->
                        {ok, Tid} = dict:find(?KEY_T, DHTMsg),
                        {ok, Args} = decode_msg_args(Tid, DHTMsg),
                        {ok, {Tid, msg_type(Tid), Args}};
                    false ->
                        {error, "unknown dht msg"}
                end;
            {ok, ?KEY_E} ->
                {ok, error, "protocol error."};
            error ->
                {error, "DHTMsg value <<y>> not found."}
        end
    catch
        _:Reason  -> {error, Reason}
    end .

decode_msg_args(?MSG_TYPE_FIND_NODE, DHTMsg) ->
    {ok, {dict, Args}} = dict:find(?KEY_A, DHTMsg),
    {ok, NodeID} = dict:find(?KEY_ID, Args),
    {ok, TargetID} = dict:find(?KEY_TARGET, Args),
    {ok, {dht_id:to_integer(NodeID), dht_id:to_integer(TargetID)}};
decode_msg_args(?MSG_TYPE_PING, DHTMsg) ->
    {ok, {dict, Args}} = dict:find(?KEY_A, DHTMsg),
    {ok, NodeID} = dict:find(?KEY_ID, Args),
    {ok, {dht_id:to_integer(NodeID)}};
decode_msg_args(?MSG_TYPE_GET_PEERS, DHTMsg) ->
    {ok, {dict, Args}} = dict:find(?KEY_A, DHTMsg),
    {ok, NodeID} = dict:find(?KEY_ID, Args),
    {ok, InfoHash} = dict:find(?KEY_INFO_HASH, Args),
    {ok, {dht_id:to_integer(NodeID), dht_id:to_integer(InfoHash)}};
decode_msg_args(?MSG_TYPE_ANNOUNCE_PEER, DHTMsg) ->
    {ok, {dict, Args}} = dict:find(?KEY_A, DHTMsg),
    {ok, NodeID} = dict:find(?KEY_ID, Args),
    {ok, InfoHash} = dict:find(?KEY_INFO_HASH, Args),
    case dict:find(?KEY_IMPLIED_PORT, Args) of
        {ok, Value} -> Implied = Value;
        _ -> Implied = 0
    end,
    {ok, Port} = dict:find(?KEY_PORT, Args),
    {ok, Token} = dict:find(?KEY_TOKEN, Args),
    {ok, {dht_id:to_integer(NodeID), dht_id:to_integer(InfoHash), Token, Implied, Port}};
decode_msg_args(<<H:9/binary, _T/binary>>, DHTMsg) when (H == ?MSG_TYPE_FIND_NODE) ->
    {ok, {dict, Args}} = dict:find(?KEY_R, DHTMsg),
    {ok, NodeID} = dict:find(?KEY_ID, Args),
    {ok, CompactNodes} = dict:find(?KEY_NODES, Args),
    {ok, {dht_id:to_integer(NodeID), decode_nodes(CompactNodes)}};
decode_msg_args(<<H:4/binary, _T/binary>>, DHTMsg) when (H == ?MSG_TYPE_PING) ->
    {ok, {dict, Args}} = dict:find(?KEY_R, DHTMsg),
    {ok, NodeID} = dict:find(?KEY_ID, Args),
    {ok, {dht_id:to_integer(NodeID)}}.

check_msg([], _) ->
    true;
check_msg([HKey | TKeys], DHTMsg) ->
    hash_key(HKey, DHTMsg) and check_msg(TKeys, DHTMsg).

hash_key(Key, DHTMsg) ->
    dict:is_key(Key, DHTMsg).

decode_nodes(<<>>) -> [];
decode_nodes(<<Header:26/binary, Tail/binary>>) ->
    <<ID:160, IP0:8, IP1:8, IP2:8, IP3:8, Port:16>> = Header,
    [#node{id = ID, ip = {IP0, IP1, IP2, IP3}, port = Port} | decode_nodes(Tail)].

encode_nodes([]) -> <<>>;
encode_nodes([#node{id = ID, ip = {IP0, IP1, IP2, IP3}, port = Port} | Tail]) ->
    Node = <<ID:160, IP0:8, IP1:8, IP2:8, IP3:8, Port:16>>,
    Nodes = encode_nodes(Tail),
    <<Node/binary, Nodes/binary>>.

encode_query_msg({ping, TID, {ID}}) ->
    Args = [{?KEY_ID, dht_id:to_binary(ID)}],
    Msg = [
        {?KEY_T, TID},
        {?KEY_Y, ?KEY_Q},
        {?KEY_Q, ?MSG_TYPE_PING},
        {?KEY_A, {dict, dict:from_list(Args)}}
    ],
    {dict, dict:from_list(Msg)};
encode_query_msg({find_node, TID, {ID, Target}}) ->
    Args = [
        {?KEY_ID, dht_id:to_binary(ID)},
        {?KEY_TARGET, dht_id:to_binary(Target)}
    ],
    Msg = [
        {?KEY_T, TID},
        {?KEY_Y, ?KEY_Q},
        {?KEY_Q, ?MSG_TYPE_FIND_NODE},
        {?KEY_A, {dict, dict:from_list(Args)}}
    ],
    {dict, dict:from_list(Msg)}.

encode_response_msg({ping, TID, {ID}}) ->
    Args = [{?KEY_ID, dht_id:to_binary(ID)}],
    Msg = [
        {?KEY_T, TID},
        {?KEY_Y, ?KEY_R},
        {?KEY_R, {dict, dict:from_list(Args)}}
    ],
    {dict, dict:from_list(Msg)};
encode_response_msg({find_node, TID, {ID, Nodes}}) ->
    Args = [
        {?KEY_ID, dht_id:to_binary(ID)},
        {?KEY_NODES, encode_nodes(Nodes)}
    ],
    Msg = [
        {?KEY_T, TID},
        {?KEY_Y, ?KEY_R},
        {?KEY_R, {dict, dict:from_list(Args)}}
    ],
    {dict, dict:from_list(Msg)};
encode_response_msg({get_peers, TID, {ID, Nodes, Token}}) ->
    Args = [
        {?KEY_ID, dht_id:to_binary(ID)},
        {?KEY_TOKEN, Token},
        {?KEY_NODES, encode_nodes(Nodes)}
    ],
    Msg = [
        {?KEY_T, TID},
        {?KEY_Y, ?KEY_R},
        {?KEY_R, {dict, dict:from_list(Args)}}
    ],
    {dict, dict:from_list(Msg)}.

msg_type(?MSG_TYPE_PING) -> q_ping;
msg_type(?MSG_TYPE_FIND_NODE) -> q_find_node;
msg_type(?MSG_TYPE_GET_PEERS) -> q_get_peers;
msg_type(?MSG_TYPE_ANNOUNCE_PEER) -> q_announce_peer;
msg_type(<<H:9/binary, _T/binary>>) when (H == ?MSG_TYPE_FIND_NODE) -> r_find_node;
msg_type(<<H:4/binary, _T/binary>>) when (H == ?MSG_TYPE_PING) -> r_ping;
msg_type(_) -> unknow.

