-module(tamarack).

-export([start/0]).

init_database() ->
    MainPool = {main,
                [{size, 5}, {max_overflow, 10}],
                [{hostname, "127.0.0.1"},
                 {database, "tamarack"},
                 {username, "vagrant"},
                 {password, "vagrant"}]},
    ok = application:set_env(epgpb, pools, [MainPool]),
    ok.

start() ->
    ok = init_database(),
    ok = application:start(qdate),
    ok = application:start(epgpb),
    ok = application:start(crypto),
    ok = application:start(cowlib),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(tamarack),
    ok = sql_migration:upgrade(main, tamarack).
