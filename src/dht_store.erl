%%%-------------------------------------------------------------------
%%% @author linfupeng
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 四月 2017 下午10:01
%%%-------------------------------------------------------------------
-module(dht_store).
-author("linfupeng").

-behaviour(gen_server).

%% API
-export([start_link/0, save/5, stop/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {fp}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, {stop}).

save(InfoHash, PeerIP, PeerPort, Implied, Port) ->
    gen_server:cast(?SERVER, {save, InfoHash, PeerIP, PeerPort, Implied, Port}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, FP} = file:open("log.txt", [write]),
    {ok, #state{fp = FP}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({stop}, #state{fp = FP} = State) ->
    file:close(FP),
    {stop, normal, State};
handle_cast({save, InfoHash, PeerIP, PeerPort, Implied, Port}, #state{fp = FP} = State) ->
    io:format(FP, "~p,~p,~p,~p,~p~n", [InfoHash, PeerIP, PeerPort, Implied, Port]),
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
