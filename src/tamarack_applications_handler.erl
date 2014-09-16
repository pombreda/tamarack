-module(tamarack_applications_handler).

-export([init/3]).
-export([content_types_provided/2, resource_exists/2]).
-export([as_json/2, as_transit_json/2, as_transit_msgpack/2]).

init(_Type, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, as_json},
        {<<"application/transit+json">>, as_transit_json},
        {<<"application/transit+msgpack">>, as_transit_msgpack}
    ], Req, State}.

resource_exists(Req, _State) ->
    case cowboy_req:binding(app_key, Req) of
        {undefined, Req2} -> {true, Req2, app_list};
        {AppKey, Req2} -> case tamarack_model:application(AppKey) of
            undefined -> {false, Req2, undefined};
            AppObj -> {true, Req2, AppObj}
        end
    end.

endpoint_data(app_list) -> tamarack_model:applications();
endpoint_data(AppObj) -> AppObj.

as_json(Req, State) ->
    tamarack_util:resp_json(Req, endpoint_data(State)).

as_transit_json(Req, State) ->
    tamarack_util:resp_ts_json(Req, endpoint_data(State)).

as_transit_msgpack(Req, State) ->
    tamarack_util:resp_ts_msgpack(Req, endpoint_data(State)).
