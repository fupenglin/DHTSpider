%%%-------------------------------------------------------------------
%%% @author linfupeng
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 四月 2017 上午10:21
%%%-------------------------------------------------------------------
-module(dht_torrent).
-author("linfupeng").

-behaviour(gen_server).
-include("dht_common.hrl").

%% API
-export([start_link/0, download_torrent/3, test/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(BT_PROTOCOL, <<"BitTorrent protocol">>).
-define(MES_ID, 20).
-define(EXT_MSG_ID, 0).
-define(EXTEND_FLAG, (1 bsl 20)).

-record(state, {id}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

download_torrent(InfoHash, IP, Port) ->
    gen_server:cast(?SERVER, {download, InfoHash, IP, Port}).

test() ->
    download_torrent(16#9107E31FD452786A401AECBBCCFC210C500732C7, {101,224,57,156}, 23424).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{id = dht_id:random_id()}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({download, InfoHash, IP, Port}, #state{id = ID} = State) ->
    ?DBG("handle_cast download~n", []),
    handle_download(InfoHash, IP, Port, ID),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_download(InfoHash, IP, Port, ID) ->
    case gen_tcp:connect(IP, Port, [binary, {active, false}]) of
        {ok, Sock} ->
            ok = make_handshake(InfoHash, ID, Sock),
            case make_extend_handshake(Sock) of
                {ok, ExtID, Size} ->
                    ?DBG("make_extend_handshake successed. ~p,~p~n", [ExtID, Size]),
                    ok;
                {error, Reason} ->
                    ?DBG("make_extend_handshake failed. ~p~n", [Reason]),
                    gen_tcp:close(Sock),
                    error
            end;
        {error, Reason} ->
            ?DBG("handle_download, connect error, ~p~n", [Reason]),
            error
    end,
    ok.

make_handshake(InfoHash, ID, Sock) ->
    send_handshake_message(InfoHash, ID, Sock),
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            check_handshake_message(InfoHash, Data);
        {error, Reason} ->
            ?DBG("make_handshake_message, read data error: ~p~n", [Reason]),
            error
    end.

make_extend_handshake(Sock) ->
    send_extend_handshake_message(Sock),
    case gen_tcp:recv(Sock, 4) of
        {ok, Data} ->
            ?DBG("make_extend_handshake, read data: ~p~n", [Data]),
            check_extend_handshake_message(Data, Sock);
        {error, Reason} ->
            ?DBG("make_exhandshake, read data error: ~p~n", [Reason]),
            {error, Reason}
    end.

send_handshake_message(InfoHash, ID, Sock) ->
    Msg = get_handshake_msg(InfoHash, ID),
    ?DBG("send_handshake_message: ~p~n", [Msg]),
    gen_tcp:send(Sock, Msg).

check_handshake_message(InfoHash, Data) ->
    case parser_handshake_message(Data) of
        {ok, {19, ?BT_PROTOCOL, 1, InfoHash}} ->
            ok;
        {ok, Msg} ->
            ?DBG("check_handshake_message, msg ~p\n", [Msg]),
            error;
        {error, Reason} ->
            ?DBG("check_handshake_message, error, ~p~n", [Reason]),
            error
    end.

send_extend_handshake_message(Sock) ->
    Msg = get_extend_handshake_msg(),
    ?DBG("send_extend_handshake_message, Msg: ~p~n", [Msg]),
    gen_tcp:send(Sock, Msg).

check_extend_handshake_message(<<Len:32>>, Sock) when (Len > 2) ->
    ?DBG("check_extend_handshake_message, Len = ~p~n", [Len]),
    case gen_tcp:recv(Sock, Len) of
        {ok, Data} ->
            ?DBG("check_extend_handshake_message, ~p~n", [Data]),
            case parser_extend_handshake_msg(Data, Len) of
                {ok, {20, 0, ExtID, Size}} -> {ok, ExtID, Size};
                {ok, _} -> {error, "msg id or extid no match"};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end;
check_extend_handshake_message(_, _) -> {error, "data len less 2"}.

get_handshake_msg(InfoHash, PeerID) ->
    BTStrLen = erlang:size(?BT_PROTOCOL),
    BTStr = ?BT_PROTOCOL,
    Reserved = (1 bsl 20) bor 1,
    <<BTStrLen:8, BTStr:BTStrLen/binary, Reserved:64, InfoHash:160, PeerID:160>>.

parser_handshake_message(<<BTLen:8, BTStr:BTLen/binary, Rev:64, InfoHash:160, _Rest/binary>> = Data)
    when (erlang:size(Data) >= 48) ->
    ?DBG("parser_handshake_message, ~p~n", [<<Rev:64>>]),
    Flag = (Rev bsr 20) band 1,
    {ok, {BTLen, BTStr, Flag, InfoHash}};
parser_handshake_message(_) ->
    {error, "protocol error"}.

get_extend_handshake_msg() ->
    D0 = {dict, dict:from_list([{"ut_metadata", 1}])},
    D1 = {dict, dict:from_list([{"m", D0}])},
    {ok, D2} = dht_bencode:encode(D1),
    Len = size(D2) + 2,
    <<Len:32, 20:8, 0:8, D2/binary>>.

parser_extend_handshake_msg(<<20:8, 0:8, Rest/binary>> = Data, Len) when (Len == erlang:size(Data)) ->
    case parser_extend_header(Rest) of
        {ok, ExtID, Size} -> {ok, {20, 0, ExtID, Size}};
        {error, Reason} -> {error, Reason}
    end;
parser_extend_handshake_msg(_, _) -> {error, "unknow data"}.

parser_extend_header(Data) ->
    case dht_bencode:decode(Data) of
        {ok, {dict, Msg}} ->
            {ok, {dict, M}} = dict:find(<<"m">>, Msg),
            {ok, ExtID} = dict:find(<<"ut_metadata">>, M),
            {ok, Size} = dict:find(<<"metadata_size">>, Msg),
            {ok, ExtID, Size};
        {error, Reason} ->
            {error, Reason}
    end.


