-module(tamarack_util).

-export([resp_json/2, resp_ts_json/2, resp_ts_msgpack/2, to_unixtime/1, map_with_keys/2]).

resp_json(Req, Data) ->
    {jsx:encode(Data), Req, Data}.

resp_ts_json(Req, Data) ->
    {transit:write(Data, [{format, json}]), Req, Data}.

resp_ts_msgpack(Req, Data) ->
    {transit:write(Data, [{format, msgpack}]), Req, Data}.

to_unixtime({D, {H, M, S}}) ->
    qdate:to_unixtime({D, {H, M, trunc(S)}}).

map_with_keys(Fun, Map) ->
    maps:fold(fun (K, V, NewMap) ->
            {NewK, NewV} = Fun(K, V),
            maps:put(NewK, NewV, NewMap) end,
        #{}, Map).
