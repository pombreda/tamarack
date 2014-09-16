-module(tamarack_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-export([start_listeners/0, rebuild_routes/0]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
        {tamarack_http,
         {tamarack_sup, start_listeners, []},
         permanent, 1000, worker,
         [tamarack_sup]}
    ]} }.

start_listeners() ->
    Dispatch = cowboy_router:compile(tamarack_routes()),
    RanchOptions = [{port, 8080}],
    CowboyOptions = [
        {env, [{dispatch, Dispatch}]}
    ],

    cowboy:start_http(tamarack_http, 100, RanchOptions, CowboyOptions).

rebuild_routes() ->
    Dispatch = cowboy_router:compile(tamarack_routes()),
    cowboy:set_env(tamarack_http, dispatch, Dispatch).

tamarack_routes() ->
    [
        {'_', [
            {<<"/">>, tamarack_index_handler, []},
            {<<"/static/[...]">>, cowboy_static, {priv_dir, tamarack, "static"}},

            {<<"/receiver-api/v1/request-data">>, tamarack_request_data_handler, []},

            {<<"/explorer-api/v1/applications/[:app_key]">>, tamarack_applications_handler, []},
            {<<"/explorer-api/v1/applications/:app_key/chart/:chart_type">>,
             tamarack_app_chart_handler, []},
            {<<"/explorer-api/v1/applications/:app_key/aggregate/:chart_type">>,
             tamarack_app_aggregate_handler, []},
            {<<"/explorer-api/v1/applications/:app_key/endpoints/:endpoint/chart/:chart_type">>,
             tamarack_app_endpoint_chart_handler, []}
        ]}
    ].
