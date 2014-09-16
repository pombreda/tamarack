-module(m0002_reqs_per_minute).

-behaviour(sql_migration).

-export([upgrade/1, downgrade/1]).

upgrade(C) ->
    {ok, _, _} = pgsql:squery(C,
        "CREATE TABLE request_by_minute (
            app_id INTEGER NOT NULL REFERENCES application(id),
            timestamp TIMESTAMPTZ NOT NULL,
            key VARCHAR(30) NOT NULL,
            value BIGINT NOT NULL
        )"),
    {ok, _, _} = pgsql:squery(C,
        "CREATE TABLE request_endpoint_by_minute (
            app_id INTEGER NOT NULL REFERENCES application(id),
            timestamp TIMESTAMPTZ NOT NULL,
            endpoint VARCHAR(100) NOT NULL,
            key VARCHAR(30) NOT NULL,
            value BIGINT NOT NULL
        )"),
    ok.

downgrade(C) ->
    {ok, _, _} = pgsql:squery(C,
        "DROP TABLE request_endpoint_by_minute"),
    {ok, _, _} = pgsql:squery(C,
        "DROP TABLE request_by_minute"),
    ok.
