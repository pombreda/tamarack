-module(tamarack_index_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_, Req, _Opts) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Content} = index_dtl:render([
        {config, [{state_debug, true}, {js_debug, true}]}
    ]),
    {ok, Req2} = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/html">>}],
        Content,
        Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
