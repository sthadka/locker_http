
-module(locker_http_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(PORT, 3001).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [webserver()]} }.

webserver_config() ->
    [{mods, [{locker_http_handler, []}]}].

webserver() ->
    MiddlewareConfig = webserver_config(),
    {webserver,
     {elli, start_link, [[{port, ?PORT},
                          {callback, elli_middleware},
                          {callback_args, MiddlewareConfig},
                          {name, {local, elli}}]]},
     permanent, 2000, worker, [elli]}.
