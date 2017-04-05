%%%-------------------------------------------------------------------
%%% @author linfupeng
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 三月 2017 上午11:50
%%%-------------------------------------------------------------------
-module(dht_table).
-author("linfupeng").

-behaviour(gen_server).
-include("dht_common.hrl").

%% API
-export([start_link/1, stop/0, add/1, delete/1, get_all/0, get_kclosest/2, get_size/0, timeout/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(IN_RANGE(X, Min, Max), ((X >= Min) and (X < Max))).

-record(state, {id, table}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(ID) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ID], []).

stop() ->
    gen_server:cast(?SERVER, {stop}).

add([]) ->
    ok;
add([Node | Tail]) ->
    gen_server:cast(?SERVER, {add, Node}),
    add(Tail);
add(Node) ->
    gen_server:cast(?SERVER, {add, Node}).

delete(Node) ->
    gen_server:cast(?SERVER, {delete, Node}).

get_all() ->
    gen_server:call(?SERVER, {get_all}).

get_kclosest(ID, K) ->
    gen_server:call(?SERVER, {get_kclosest, ID, K}, infinity).

get_size() ->
    gen_server:call(?SERVER, {get_size}).

timeout(Node) ->
    gen_server:cast(?SERVER, {timeout, Node}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ID]) ->
    {ok, State} = do_init(ID),
    {ok, State}.

handle_call({get_all}, _From, State) ->
    Nodes = do_get_all(State#state.table),
    {reply, Nodes, State};
handle_call({get_kclosest, ID, Num}, _From, State) ->
    Nodes = do_get_closest(ID, Num, State#state.table),
    {reply, Nodes, State};
handle_call({get_size}, _From, State) ->
    Cnt = do_get_size(State#state.table),
    {reply, Cnt, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add, #node{id = NodeID} = Node}, #state{id = MyID, table = Table} = State) ->
    case ((NodeID == MyID) or do_is_member(Node, Table)) of
        true ->
            NTable = Table;
        false ->
            {Res, NTable} = do_add(Node, Table),
            case Res of
                true -> dht_state:monitor(Node);
                false -> ok
            end
    end,
    {noreply, State#state{table = NTable}};
handle_cast({delete, Node}, #state{table = Table} = State) ->
    NTable = do_delete(Node, Table),
    {noreply, State#state{table = NTable}};
handle_cast({timeout, Node}, State) ->
    do_time_out(Node),
    {noreply, State};
handle_cast({stop}, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    ?DBG("dht_table, terminate, reason = ~p~n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_init(ID) ->
    Min = dht_id:min(),
    Max = dht_id:max(),
    Table = [#bucket{min = Min, max = Max, nodes = []}],
    State = #state{id = ID, table = Table},
    {ok, State}.

do_add(#node{id = NodeID} = Node, [#bucket{min = Min, max = Max, nodes = Nodes} = H | T])
    when ?IN_RANGE(NodeID, Min, Max) ->

    BucketLen = length(Nodes),
    if
        BucketLen < ?K ->
            {true, [H#bucket{nodes = [Node | Nodes]} | T]};
        (BucketLen == ?K) and ((Max - Min) > ?K) ->
            Mid = (Min + Max) div 2,
            Lower = [Tmp || #node{id = TmpID} = Tmp <- Nodes, ?IN_RANGE(TmpID, Min, Mid)],
            LowerBucket = H#bucket{max = Mid, nodes = Lower},
            Upper = [Tmp || #node{id = TmpID} = Tmp <- Nodes, ?IN_RANGE(TmpID, Mid, Max)],
            UpperBucket = H#bucket{min = Mid, nodes = Upper},
            NewTable = [LowerBucket, UpperBucket] ++ T,
            do_add(Node, NewTable);
        true ->
            {false, [H | T]}
    end;
do_add(Node, [H | T]) ->
    {Res, NewNodes} = do_add(Node, T),
    {Res, [H | NewNodes]}.

do_delete(_, []) -> [];
do_delete(#node{id = NodeID} = Node, [#bucket{min = Min, max = Max, nodes = Nodes} = H | T])
    when ?IN_RANGE(NodeID, Min, Max) ->
    NewNodes = lists:delete(Node, Nodes),
    [H#bucket{nodes = NewNodes} | T];
do_delete(Node, [H | T]) ->
    [H | do_delete(Node, T)].

do_get_all([]) -> [];
do_get_all([#bucket{nodes = Nodes} | T]) ->
    Nodes ++ do_get_all(T).

do_get_closest(ID, Num, Table) ->
    Nodes = do_get_all(Table),
    Distances = [{dht_id:distance(ID, TmpID), Node} || #node{id = TmpID} = Node <- Nodes],
    Sorted = lists:sort(Distances),
    case length(Sorted) > Num of
        true ->
            {PreList, _} = lists:split(Num, Sorted),
            [Node || {_, Node} <- PreList];
        false ->
            [Node || {_, Node} <- Sorted]
    end.

do_get_size([]) -> 0;
do_get_size([#bucket{nodes = Nodes} | T]) ->
    length(Nodes) + do_get_size(T).

do_is_member(_, []) -> false;
do_is_member(#node{id = NodeID} = Node, [#bucket{min = Min, max = Max, nodes = Nodes} | _T])
    when ?IN_RANGE(NodeID, Min, Max) ->
    lists:member(Node, Nodes);
do_is_member(Node, [_ | T]) ->
    do_is_member(Node, T).

do_time_out(Node) ->
    dht_net:ping(Node).