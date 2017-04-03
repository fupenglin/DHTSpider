%%%-------------------------------------------------------------------
%%% @author linfupeng
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 三月 2017 上午11:52
%%%-------------------------------------------------------------------
-author("linfupeng").

%% 常量K
-define(K, 8).

%% UDP网络协议默认选项
-define(DEFAULT_UDP_OPT, [binary]).

%% 默认节点端口
-define(DEFAULT_PORT, 6881).

%% 节点信息记录
-record(node, {id, ip, port}).

%% 桶结构
-record(bucket, {min, max, nodes}).

%%超级节点
-define(SUPER_NODES, [#node{id = 0, ip = "router.bittorrent.com", port = 6881},
    #node{id = 0, ip = "dht.transmissionbt.com", port = 6881},
    #node{id = 0, ip = "router.utorrent.com", port = 6881}]).

%% 默认节点ID
-define(DEFAULT_NODE_ID, 649262719799963483759422800960489108797112648079).

-define(DEBUG, 1).
%% 时间间隔
-ifdef(DEBUG).
-define(JOIN_INTERVAL, 1 * 60 * 1000).
-define(PING_INTERVAL, 2 * 60 * 1000).
-define(JOIN_CONDITION, 5000).
-define(NODE_TIMEOUT, 2 * 60 * 1000).
-define(DBG(Fmt, Args), io:format(Fmt, Args)).
-else.
-define(JOIN_INTERVAL, 5 * 60 * 1000).
-define(PING_INTERVAL, 10 * 60 * 1000).
-define(JOIN_CONDITION, 2000).
-define(NODE_TIMEOUT, 5 * 60 * 1000).
-define(DBG(Fmt, Args), ok).
-endif.