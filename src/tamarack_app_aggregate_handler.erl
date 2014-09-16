-module(tamarack_app_aggregate_handler).

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
    {AppKey, Req2} = cowboy_req:binding(app_key, Req),
    {ChartType, Req3} = cowboy_req:binding(chart_type, Req2),
    case tamarack_mapper:mapper(binary_to_atom(ChartType, utf8)) of
        undefined -> {false, Req3, undefined};
        {ok, MapperFn} -> case tamarack_model:application(AppKey) of
            undefined -> {false, Req3, undefined};
            #{id := AppId} -> {true, Req3, {MapperFn, AppId}}
        end
    end.

endpoint_data(Req, {MapperFn, AppId}) ->
    {RawFromTimestamp, _} = cowboy_req:qs_val(<<"from">>, Req),
    {RawToTimestamp, _} = cowboy_req:qs_val(<<"to">>, Req),
    FromTimestamp = iso8601:parse(RawFromTimestamp),
    ToTimestamp = iso8601:parse(RawToTimestamp),

    % Fetch {timestamp, endpoint, key, value} tuples
    {ok, _, Rows} = epgpb:equery(main,
        "SELECT timestamp, endpoint, key, SUM(value)
            FROM request_endpoint_by_minute
            WHERE app_id = $1 AND timestamp >= $2 AND timestamp <= $3
            GROUP BY app_id, timestamp, endpoint, key
            ORDER BY timestamp, key",
        [AppId, FromTimestamp, ToTimestamp]),

    % Group tuples to #{{unix_timestamp, endpoint} => #{key => value}}
    % per minute and endpoint.
    Datapoints = lists:foldl(fun ({TS, E, K, V}, M) ->
            UTS = tamarack_util:to_unixtime(TS) * 1000,
            PrevMap = maps:get({UTS, E}, M, #{}),
            maps:put({UTS, E}, maps:put(K, V, PrevMap), M) end,
        maps:new(), Rows),

    % Sum sensor data and count values per endpoint, result will be
    % #{endpoint -> {sum, count}}.
    Result = maps:fold(fun ({_, E}, D, Acc) ->
            {Sum, Count} = maps:get(E, Acc, {0, 0}),
            maps:put(E, {Sum + lists:sum(maps:values(MapperFn(D))), Count + 1}, Acc) end,
        maps:new(), Datapoints),

    % Compute the average from the above map: #{endpoint => average}
    Result2 = maps:map(fun (_, {Sum, Count}) -> Sum / Count end, Result),

    % Turn the map into sorted [K, V] lists, otherwise the thing will
    % turn into a JSON/Transit dictionary anyway.
    lists:map(fun ({K, V}) -> [K, V] end,
        lists:sort(fun ({_, L}, {_, R}) -> L > R end, maps:to_list(Result2))).

as_json(Req, State) ->
    tamarack_util:resp_json(Req, endpoint_data(Req, State)).

as_transit_json(Req, State) ->
    tamarack_util:resp_ts_json(Req, endpoint_data(Req, State)).

as_transit_msgpack(Req, State) ->
    tamarack_util:resp_ts_msgpack(Req, endpoint_data(Req, State)).
