-module(m0001_initial).

-behaviour(sql_migration).

-export([upgrade/1, downgrade/1]).

upgrade(C) ->
    {ok, _, _} = pgsql:squery(C,
        "CREATE TABLE application (
            id SERIAL PRIMARY KEY,
            key VARCHAR(30) NOT NULL UNIQUE,
            display_name VARCHAR(60) NOT NULL
        )"),
    ok.

downgrade(C) ->
    {ok, _, _} = pgsql:squery(C,
        "DROP TABLE application"),
    ok.
