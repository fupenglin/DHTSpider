%%%-------------------------------------------------------------------
%%% @author linfupeng
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 四月 2017 下午12:58
%%%-------------------------------------------------------------------
-module(dht_spider).
-author("linfupeng").

-include("dht_common.hrl").

%% API
-export([start/0, stop/0]).

start() ->
    dht_table:start_link(?DEFAULT_NODE_ID),
    dht_net:start_link(?DEFAULT_NODE_ID, ?DEFAULT_PORT).

stop() ->
    dht_net:stop().
