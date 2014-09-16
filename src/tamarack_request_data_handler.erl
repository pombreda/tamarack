-module(tamarack_request_data_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_, Req, _Opts) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    Data = jsx:decode(Body),
    ok = process_request_data(Data),
    {ok, Req3} = cowboy_req:reply(200, [],
        <<"{\"status\": \"ok\"}">>,
        Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.


process_request_data(Data) ->
    AppKey = proplists:get_value(<<"app_name">>, Data),
    MinuteData = proplists:get_value(<<"by_minute">>, Data, []),
    {ok, _, [{AppId}]} = epgpb:equery(main,
        "SELECT id FROM application WHERE key = $1",
        [AppKey]),
    lists:map(fun (D) -> process_minute_datapoint(AppId, D) end, MinuteData),
    ok.

process_minute_datapoint(AppId, Datapoint) ->
    Timestamp = iso8601:parse(proplists:get_value(<<"timestamp">>, Datapoint)),
    Endpoint = proplists:get_value(<<"endpoint">>, Datapoint),
    RequestCount = proplists:get_value(<<"request_count">>, Datapoint),
    ErrorCount = proplists:get_value(<<"error_count">>, Datapoint),
    RawSensorData = proplists:get_value(<<"sensor_data">>, Datapoint),

    SensorData = lists:append(
        RawSensorData,
        [{<<"request_count">>, RequestCount}, {<<"error_count">>, ErrorCount}]
    ),

    lists:map(fun ({Key, Value}) ->
        insert_datapoint(AppId, Timestamp, Endpoint, Key, Value) end,
        SensorData),
    ok.

insert_datapoint(AppId, Timestamp, Endpoint, Key, Value) ->
    {ok, 1} = epgpb:equery(main,
        "INSERT INTO request_by_minute VALUES ($1, $2, $3, $4)",
        [AppId, Timestamp, Key, Value]),
    {ok, 1} = epgpb:equery(main,
        "INSERT INTO request_endpoint_by_minute VALUES ($1, $2, $3, $4, $5)",
        [AppId, Timestamp, Endpoint, Key, Value]),
    ok.
