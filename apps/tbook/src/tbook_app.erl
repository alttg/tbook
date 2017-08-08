%%%-------------------------------------------------------------------
%% @doc tbook public API
%% @end
%%%-------------------------------------------------------------------

-module(tbook_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    ok = tbook_mysql:db_init(),
    {ok, Port} = application:get_env(tbook, http),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", tbook_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, tbook:to_integer(Port)}], #{
        env => #{dispatch => Dispatch}
    }),
    tbook_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================