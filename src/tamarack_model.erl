-module(tamarack_model).

-export([applications/0, application/1]).

applications() ->
    {ok, _, Rows} = epgpb:equery(main,
        "SELECT id, key, display_name FROM application ORDER BY display_name"),
    lists:map(fun ({Id, Key, DisplayName}) ->
        #{id => Id, name => Key, 'display-name' => DisplayName} end,
        Rows).

application(AppKey) ->
    {ok, _, Rows} = epgpb:equery(main,
        "SELECT id, display_name FROM application WHERE key = $1", [AppKey]),
    case Rows of
        [] -> undefined;
        [{Id, DisplayName}] ->
            #{id => Id, name => AppKey, 'display-name' => DisplayName}
    end.
