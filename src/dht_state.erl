%%%-------------------------------------------------------------------
%%% @author linfupeng
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 四月 2017 下午4:13
%%%-------------------------------------------------------------------
-module(dht_state).
-author("linfupeng").

-behaviour(gen_server).
-include("dht_common.hrl").

%% API
-export([start_link/1, stop/0, monitor/1, size/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {id, timer}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(ID) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ID], []).

stop() ->
    gen_server:cast(?SERVER, {stop}).

monitor(Node) ->
    gen_server:cast(?SERVER, {monitor, Node}).

size() ->
    gen_server:call(?SERVER, {size}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([ID]) ->
    do_init(ID).

handle_call({size}, _From, #state{timer = Timer} = State) ->
    Size = gb_trees:size(Timer),
    {reply, Size, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({monitor, Node}, State) ->
    NewState = update_monitor(Node, State),
    {noreply, NewState};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({timeout, Node}, State) ->
    NewState = handle_node_timeout(Node, State),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_init(ID) ->
    {ok, #state{id = ID, timer = gb_trees:empty()}}.

add_monitor(Node, #state{timer = Timer} = State) ->
    case gb_trees:is_defined(Node, Timer) of
        true ->
            State;
        false ->
            Msg = {timeout, Node},
            {ok, Ref} = timer:send_interval(?NODE_TIMEOUT, Msg),
            NewTimer = gb_trees:insert(Node, {os:timestamp(), Ref, 1}, Timer),
            State#state{timer = NewTimer}
    end.

update_monitor(Node, #state{timer = Timer} = State) ->
    case gb_trees:is_defined(Node, Timer) of
        true ->
            {_, Ref, _} = gb_trees:get(Node, Timer),
            NewTimer = gb_trees:update(Node, {os:timestamp(), Ref, 1}, Timer),
            State#state{timer = NewTimer};
        false ->
            add_monitor(Node, State)
    end.

remove_monitor(Node, #state{timer = Timer} = State) ->
    case gb_trees:is_defined(Node, Timer) of
        true ->
            {_, Ref, _} = gb_trees:get(Node, Timer),
            timer:cancel(Ref),
            NewTimer = gb_trees:delete(Node, Timer),
            State#state{timer = NewTimer};
        false ->
            State
    end.

handle_node_timeout(Node, #state{timer = Timer} = State) ->
    case gb_trees:is_defined(Node, Timer) of
        true ->
            {LastTime, Ref, Cnt} = gb_trees:get(Node, Timer),
            TimeDiff = timer:now_diff(os:timestamp(), LastTime) div 1000,
            if
                (Cnt > 3) and (TimeDiff > ?NODE_TIMEOUT) ->
                    dht_table:delete(Node),
                    remove_monitor(Node, State);
                (TimeDiff > ?NODE_TIMEOUT) ->
                    notify_node_timeout(Node),
                    NewTimer = gb_trees:update(Node, {os:timestamp(), Ref, Cnt + 1}, Timer),
                    State#state{timer = NewTimer};
                true ->
                    State
            end;
        false ->
            State
    end.

notify_node_timeout(Node) ->
    dht_table:timeout(Node).
