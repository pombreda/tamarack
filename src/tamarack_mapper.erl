-module(tamarack_mapper).

-export([mapper/1]).

ms_per_req(SensorData) ->
    case maps:get(<<"request_count">>, SensorData, 0) of
        0 -> maps:map(fun (_, _) -> 0 end,
            maps:without([<<"error_count">>, <<"request_count">>], SensorData));
        ReqCountStr ->
            ReqCount = binary_to_integer(ReqCountStr),
            tamarack_util:map_with_keys(fun (K, V) ->
                    {binary_to_atom(K, utf8), (1/1000) * binary_to_integer(V) / ReqCount} end,
                maps:without([<<"error_count">>, <<"request_count">>], SensorData))
    end.

reqs_per_min(SensorData) ->
    ReqCount = binary_to_integer(maps:get(<<"request_count">>, SensorData, 0)),

    #{'request-count' => ReqCount}.

errs_per_req(SensorData) ->
    ErrCount = binary_to_integer(maps:get(<<"error_count">>, SensorData, 0)),

    ErrRatio = case binary_to_integer(maps:get(<<"request_count">>, SensorData, 0)) of
        0 -> 0;
        ReqCount -> ErrCount / ReqCount
    end,

    #{'error-count' => ErrRatio}.

total_time(SensorData) ->
    TotalTime = maps:fold(fun (_, V, Acc) -> Acc + binary_to_integer(V) end, 0,
        maps:without([<<"error_count">>, <<"request_count">>], SensorData)),

    #{'total-time' => TotalTime}.


mapper('ms-per-req') -> {ok, fun ms_per_req/1};
mapper('reqs-per-min') -> {ok, fun reqs_per_min/1};
mapper('errs-per-req') -> {ok, fun errs_per_req/1};
mapper('total-time') -> {ok, fun total_time/1};
mapper(_) -> undefined.
