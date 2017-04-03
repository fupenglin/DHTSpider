%%%-------------------------------------------------------------------
%%% @author linfupeng
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 三月 2017 下午1:13
%%%-------------------------------------------------------------------
-module(dht_net).
-author("linfupeng").

-behaviour(gen_server).
-include("dht_common.hrl").

%% API
-export([start_link/2, stop/0, join/0, ping/1,find_node/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(MAX_TID, 10000).

-record(state, {id, socket, tid, timer}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(ID, Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ID, Port], []).

stop() ->
    gen_server:cast(?SERVER, {stop}).

join() ->
    gen_server:cast(?SERVER, {join}).

ping(Node) ->
    gen_server:cast(?SERVER, {ping, Node}).

find_node(TargetID, Node) ->
    gen_server:cast(?SERVER, {find_node, TargetID, Node}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([ID, Port]) ->
    do_init([ID, Port, ?DEFAULT_UDP_OPT]).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({stop}, State) ->
    {stop, normal, State};
handle_cast({ping, Node}, State) ->
    NewState = handle_make_query(ping, Node, "", State),
    {noreply, NewState};
handle_cast({find_node, TargetID, Node}, State) ->
    NewState = handle_make_query(find_node, Node, TargetID, State),
    {noreply, NewState};
handle_cast({join}, State) ->
    NewState = case dht_table:get_size() > ?JOIN_CONDITION of
        true -> State;
        false ->
            State1 = handle_join(?SUPER_NODES, State),
            handle_join(dht_table:get_all(), State1)
    end,
    {noreply, NewState};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({udp, _Socket, _IP, _Port, _Packet} = UdpMsg, #state{id = MyID} = State) ->
    NewState = handle_udp_message(MyID, UdpMsg, State),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    do_stop(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_init([ID, Port, Opt]) ->
    case gen_udp:open(Port, Opt) of
        {ok, Socket} ->
            {ok, #state{socket = Socket, id = ID, tid = 0}};
        {error, Reason} ->
            {stop, Reason}
    end.

do_stop(#state{socket = Socket} = _State) ->
    gen_udp:close(Socket).

handle_udp_message(_MyID, {udp, Socket, IP, Port, Packet}, State) ->
    case dht_msg:decode(Packet) of
        {ok, {Tid, Type, Args}} ->
            handle_dht_message(Type, {Tid, Args}, Socket, IP, Port, State);
        {ok, error, Reason} ->
            ?DBG("handle_udp_message, ~p~n", [Reason]),
            State;
        {error, Reason} ->
            ?DBG("handle_udp_message, error: ~p~n", [Reason]),
            State
    end.

handle_dht_message(q_ping, {Tid, {NodeID}}, Sock, IP, Port, #state{id = ID} = State) ->
    ?DBG("handle_get_query, ping~n", []),
    Node = #node{id = NodeID, ip = IP, port = Port},
    dht_table:add(Node),
    case dht_msg:encode_response({ping, Tid, {ID}}) of
        {ok, Msg} -> send_msg(Sock, IP, Port, Msg);
        _ -> error
    end,
    State;

handle_dht_message(q_find_node, {Tid, Args}, Sock, IP, Port, #state{id = ID} = State) ->
    ?DBG("handle_get_query, find_node~n", []),
    {NodeID, Target} = Args,
    Node = #node{id = NodeID, ip = IP, port = Port},
    dht_table:add(Node),
    Nodes = dht_table:get_kclosest(Target, ?K),
    case dht_msg:encode_response({find_node, Tid, {ID, Nodes}}) of
        {ok, Msg} -> send_msg(Sock, IP, Port, Msg);
        _ -> error
    end,
    State;

handle_dht_message(q_get_peers, {Tid, Args}, Sock, IP, Port, #state{id = ID} = State) ->
    {NodeID, InfoHash} = Args,
    ?DBG("handle_get_query, get_peers~n", []),
    Node = #node{id = NodeID, ip = IP, port = Port},
    dht_table:add(Node),
    Nodes = dht_table:get_kclosest(InfoHash, ?K),
    Token = make_token(IP, Port),
    {ok, Msg} = dht_msg:encode_response({get_peers, Tid, {ID, Nodes, Token}}),
    send_msg(Sock, IP, Port, Msg),
    State;

handle_dht_message(q_announce_peer, {_Tid, Args}, _Sock, IP, Port, State) ->
    {NodeID, InfoHash, Implied, _Port, _Token} = Args,
    dht_table:add(#node{id = NodeID, ip = IP, port = Port}),
    ?DBG("handle_get_query, announce_peer, InfoHash = ~p, IP = ~p, Port = ~p~n", [dht_id:to_hex(InfoHash), IP, Port]),
    ?DBG("handle_get_query, announce_peer, Implied = ~p, Port =~p~n", [Implied, Port]),
    State;

handle_dht_message(r_ping, {_Tid, {ID}}, _Sock, IP, Port, State) ->
    ?DBG("handle_dht_message, r_ping~n", []),
    Node = #node{id = ID, ip = IP, port = Port},
    dht_table:add(Node),
    State;
handle_dht_message(r_find_node, {_Tid, Args}, _Sock, IP, Port, State) ->
    ?DBG("handle_dht_message, r_find_node~n", []),
    {NodeID, Nodes} = Args,
    dht_table:add(#node{id = NodeID, ip = IP, port = Port}),
    dht_table:add(Nodes),
    State.

handle_make_query(ping, #node{ip = IP, port = Port}, _, #state{tid = Cnt, id = ID, socket = Sock} = State) ->
    Tid = make_tid(ping, Cnt),
    case dht_msg:encode_query({ping, Tid, {ID}}) of
        {ok, Msg} -> send_msg(Sock, IP, Port, Msg);
        _ -> error
    end,
    State#state{tid = (Cnt + 1) rem ?MAX_TID};
handle_make_query(find_node, #node{ip = IP, port = Port}, Target, #state{tid = Cnt, id = ID, socket = Sock} = State) ->
    Tid = make_tid(find_node, Cnt),
    case dht_msg:encode_query({find_node, Tid, {ID, Target}}) of
        {ok, Msg} -> send_msg(Sock, IP, Port, Msg);
        _ -> error
    end,
    State#state{tid = (Cnt + 1) rem ?MAX_TID}.

handle_join([], State) -> State;
handle_join([Node | Rest], #state{id = ID} = State) ->
    NewState = handle_make_query(find_node, Node, ID, State),
    handle_join(Rest, NewState).

send_msg(Sock, IP, Port, Msg) ->
    %%?DBG("send msg:~p~n", [Msg]),
    gen_udp:send(Sock, IP, Port, Msg).

make_token(_IP, _Port) ->
    Token = [random:uniform(256) - 1 || _ <- lists:seq(1, 5)],
    list_to_binary(Token).

make_tid(ping, Cnt) ->
    Tid = "ping_" ++ integer_to_list(Cnt),
    list_to_binary(Tid);
make_tid(find_node, Cnt) ->
    Tid = "find_node_" ++ integer_to_list(Cnt),
    list_to_binary(Tid).


