-module(sql_migration).

-callback upgrade(_) -> 'ok'.
-callback downgrade(_) -> 'ok'.

-export([migrations/1, upgrade/2, downgrade/3]).

is_migration(Module) ->
    Info = Module:module_info(attributes),
    Behaviours = proplists:get_value(behaviour, Info, []),
    lists:member(sql_migration, Behaviours).

migrations(App) ->
    {ok, Modules} = application:get_key(App, modules),
    lists:usort(lists:filter(fun is_migration/1, Modules)).

has_migration_table(Pool) ->
    {ok, _, Rows} = epgpb:squery(Pool,
        "SELECT table_name FROM information_schema.tables
            WHERE table_name = 'sql_migrations'"),
    Rows /= [].

create_migration_table(Pool) ->
    io:format("migrations: creating migration table~n"),
    {ok, _, _} = epgpb:squery(Pool,
        "CREATE TABLE sql_migrations (
            name VARCHAR(80) PRIMARY KEY)"),
    ok.

ensure_has_migration_table(Pool) ->
    case has_migration_table(Pool) of
        true -> ok;
        false -> create_migration_table(Pool)
    end.

latest_version(Pool) ->
    {ok, _, [{Version}]} = epgpb:squery(Pool,
        "SELECT MAX(name) FROM sql_migrations"),

    case Version =:= null of
        true -> m0000_none;
        false -> binary_to_atom(Version, utf8)
    end.

run_single_upgrade(Pool, Version) ->
    epgpb:with_transaction(Pool, fun (C) ->
        io:format("Upgrading ~w~n", [Version]),
        ok = Version:upgrade(C),
        {ok, _} = pgsql:equery(C,
            "INSERT INTO sql_migrations VALUES ($1)", [Version])
    end).

run_single_downgrade(Pool, Version) ->
    epgpb:with_transaction(Pool, fun(C) ->
        io:format("Downgrading ~w~n", [Version]),
        ok = Version:downgrade(C),
        {ok, _} = pgsql:equery(C,
            "DELETE FROM sql_migrations WHERE name = $1", [Version])
    end).

upgrade(Pool, App) ->
    ok = ensure_has_migration_table(Pool),
    CurrentVersion = latest_version(Pool),
    NewerVersions = lists:dropwhile(
        fun (V) -> V =< CurrentVersion end, migrations(App)),
    lists:map(fun (V) -> run_single_upgrade(Pool, V) end, NewerVersions),
    ok.

downgrade(Pool, App, Version) ->
    ok = ensure_has_migration_table(Pool),
    CurrentVersion = latest_version(Pool),
    AppliedVersions = lists:takewhile(
        fun (V) -> V =< CurrentVersion end, migrations(App)),
    VersionsToRemove = lists:dropwhile(
        fun (V) -> V =< Version end, AppliedVersions),
    lists:map(fun (V) -> run_single_downgrade(Pool, V) end, VersionsToRemove),
    ok.
