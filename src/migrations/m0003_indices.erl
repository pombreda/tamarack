-module(m0003_indices).

-behaviour(sql_migration).

-export([upgrade/1, downgrade/1]).

upgrade(C) ->
    {ok, _, _} = pgsql:squery(C,
        "CREATE INDEX request_by_minute_app_id_timestamp_key_idx
            ON request_by_minute (app_id, timestamp, key)"),
    {ok, _, _} = pgsql:squery(C,
        "CREATE INDEX request_endpoint_by_minute_app_id_timestamp_endpoint_key_idx
            ON request_endpoint_by_minute (app_id, endpoint, timestamp, key)"),
    ok.

downgrade(C) ->
    {ok, _, _} = pgsql:squery(C,
        "DROP INDEX request_by_minute_app_id_timestamp_key_idx"),
    {ok, _, _} = pgsql:squery(C,
        "DROP INDEX request_endpoint_by_minute_app_id_timestamp_endpoint_key_idx"),
    ok.
