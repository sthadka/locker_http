-module(locker_http_handler).
-export([handle/2, handle_event/3]).

-include_lib("locker_http.hrl").
-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

%% Home page
handle('GET', [], _Req) ->
    {200, [], <<"ok">>};

handle('GET',[<<"start_link">>, W], _Req) ->
    supervisor:start_child(locker_http_sup, locker_child(?b2i(W))),
    {200, [], <<"ok">>};

handle('GET',[<<"start_link">>, W, LeaseExpireInterval,
              LockExpireInterval, PushTransInterval], _Req) ->
    supervisor:add_child(locker_http_sup,
                         locker_child(?b2i(W), ?b2i(LeaseExpireInterval),
                                      ?b2i(LockExpireInterval),
                                      ?b2i(PushTransInterval))),
    {200, [], <<"ok">>};

handle('GET',[<<"set_w">>, ClusterCsv, W], _Req) ->
    Cluster = binary:split(ClusterCsv, <<",">>, [trim, global]),
    ok = locker:set_w(Cluster, ?b2i(W)),
    {200, [], <<"ok">>};

handle('GET',[<<"set_nodes">>, ClusterCsv, PrimariesCsv], Req) ->
    handle('GET',[<<"set_nodes">>, ClusterCsv, PrimariesCsv, <<>>], Req);

handle('GET',[<<"set_nodes">>, ClusterCsv, PrimariesCsv, ReplicasCsv], _Req) ->
    Cluster = csv_to_atom_list(ClusterCsv),
    Primaries = csv_to_atom_list(PrimariesCsv),
    Replicas = csv_to_atom_list(ReplicasCsv),

    ok = locker:set_nodes(Cluster, Primaries, Replicas),
    {200, [], <<"ok">>};

handle('GET',[<<"lock">>, Key, Value], _Req) ->
    Ret = locker:lock(Key, Value),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], ?t2b(Ret)};

handle('GET',[<<"lock">>, Key, Value, LeaseLength], _Req) ->
    Ret = locker:lock(Key, Value, ?b2i(LeaseLength)),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], ?t2b(Ret)};

handle('GET',[<<"lock">>, Key, Value, LeaseLength, Timeout], _Req) ->
    Ret = locker:lock(Key, Value, ?b2i(LeaseLength), ?b2i(Timeout)),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], ?t2b(Ret)};

handle('GET',[<<"update">>, Key, Value, NewValue], _Req) ->
    Ret = locker:update(Key, Value, NewValue),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], ?t2b(Ret)};

handle('GET',[<<"update">>, Key, Value, NewValue, Timeout], _Req) ->
    Ret = locker:update(Key, Value, NewValue, ?b2i(Timeout)),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], ?t2b(Ret)};

handle('GET',[<<"extend_lease">>, Key, Value, LeaseLength], _Req) ->
    Ret = locker:extend_lease(Key, Value, ?b2i(LeaseLength)),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], ?t2b(Ret)};

handle('GET',[<<"release">>, Key, Value], _Req) ->
    Ret = locker:release(Key, Value),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], ?t2b(Ret)};

handle('GET',[<<"wait_for">>, Key, Timeout], _Req) ->
    Ret = locker:wait_for(Key, ?b2i(Timeout)),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], ?t2b(Ret)};

handle('GET',[<<"wait_for_release">>, Key], _Req) ->
    Ret = locker:wait_for_release(Key),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], ?t2b(Ret)};

handle('GET',[<<"wait_for_release">>, Key, Timeout], _Req) ->
    Ret = locker:wait_for_release(Key, ?b2i(Timeout)),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], ?t2b(Ret)};

handle('GET',[<<"dirty_read">>, Key], _Req) ->
    Ret = locker:dirty_read(Key),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], ?t2b(Ret)};

handle('GET',[<<"master_dirty_read">>, Key], _Req) ->
    Ret = locker:master_dirty_read(Key),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], ?t2b(Ret)};

handle('GET',[<<"lag">>], _Req) ->
    Ret = locker:lag(),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], ?t2b(Ret)};

handle('GET',[<<"summary">>], _Req) ->
    Ret = locker:summary(),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], ?t2b(Ret)};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(request_error, Data, Args) ->
    error_logger:warning_msg("Request Error: ~p ~n ~p ~n", [Args, Data]);
handle_event(_Event, _Data, _Args) ->
    ok.

locker_child(W) ->
    {locker, {locker, start_link, [W]}, permanent, 2000, worker, [locker]}.

locker_child(W, LeaseExpireInterval, LockExpireInterval, PushTransInterval) ->
    {locker, {locker, start_link, [W, LeaseExpireInterval,
                                    LockExpireInterval, PushTransInterval]},
     permanent, 2000, worker, [locker]}.

csv_to_atom_list(CSV) ->
    lists:map(fun (B) -> ?b2a(B) end,
              binary:split(CSV, <<",">>, [trim, global])).
