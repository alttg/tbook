-module(tbook_tests).

-include_lib("eunit/include/eunit.hrl").


-define(bmovie, <<"tt5140878">>).
-define(bscreenid, <<"screen_1000000000">>).

-define(movie, 5140878).
-define(screenid, 1000000000).


tbook_test_() ->
    {setup,
        spawn,
        fun() ->
            _ = process_flag(trap_exit, true),
            application:set_env(tbook, tmdb, {"bc07843007f6ffa6e581cc50110a8749", "en-US"}),
            application:set_env(tbook, mysql, {"localhost", "tbook", "tbook", "tbook_test"}),
            application:set_env(tbook, http, 8080),
            [start_application(X) || X <- [crypto,asn1,public_key,ssl,emysql,jsx,tbook]]
        end,
        fun(Apps) ->
            application_stop(Apps)
        end,
        fun(_) ->
            [
                {"cowboy query" , ?_test(cowboy_request())},
                {"tmdb query" , ?_test(tmdb_request())},
                {"mysql_dbinit" , ?_test(mysql_dbinit())},
                {"mysql_addmovie" , ?_test(mysql_addmovie())},
                {"mysql_reserveseat" , ?_test(mysql_reserveseat())},
                {"mysql_info" , ?_test(mysql_info())},
                {"request_reserveseat" , ?_test(request_reserveseat())},
                {"request_reserveseat_error" , ?_test(request_reserveseat_error())},
                {"request_info" , ?_test(request_info())},
                {"mysql_remove" , ?_test(mysql_remove())}

            ]
        end
    }.

%%

cowboy_request() ->
    PostBody = jsx:encode(#{<<"imdbId">> => ?bmovie, <<"screenId">> =>?bscreenid, <<"test">> => 1}),
    Url = "http://localhost:8080/",
    {ok, {_,_,Ret}}  = httpc:request(post, {Url, [], "application/json", PostBody}, [], []),
    ?assertEqual(#{<<"result">> => 1}, jsx:decode(tbook:to_binary(Ret), [return_maps])).

tmdb_request() ->
    Title = tbook_handler:request_title(?movie),
    ?assertEqual({ok,<<"Annabelle: Creation">>}, Title).


mysql_dbinit() ->
    ?assertEqual(ok,tbook_mysql:db_init()).

mysql_addmovie() ->
    ?assertEqual(ok,tbook_mysql:add_movie(?movie, ?screenid, 2)).

mysql_reserveseat() ->
    ?assertEqual(ok,tbook_mysql:reserve_seat(?movie, ?screenid)).

mysql_info() ->
    ?assertEqual({ok,[?movie, ?screenid,2,1]},tbook_mysql:info(?movie, ?screenid)).

request_reserveseat() ->
    ?assertEqual(#{ <<"result">> => 1 },tbook_handler:handle_post(#{<<"imdbId">> => ?bmovie, <<"screenId">> => ?bscreenid})).

request_reserveseat_error() ->
    ?assertEqual(#{ <<"result">> => 0 },tbook_handler:handle_post(#{<<"imdbId">> => ?bmovie, <<"screenId">> => ?bscreenid})).

request_info() ->
    Res = #{<<"screenId">> => ?bscreenid,<<"imdbId">> => ?bmovie,
        <<"reservedSeats">> => 2,<<"result">> => 1,
        <<"movieTitle">> => <<"Annabelle: Creation">>,<<"availableSeats">> => 2},
    ?assertEqual(Res,tbook_handler:with_title(tbook_mysql:info(?movie, ?screenid))).

mysql_remove() ->
    ?assertEqual(ok,tbook_mysql:remove_movie(?movie, ?screenid)).


%%

start_application(AppName) ->
    case application:start(AppName) of
        ok ->
            [AppName];
        {error, {already_started, AppName}} ->
            [];
        {error, {not_started, DepName}} ->
            start_application(DepName) ++ start_application(AppName);
        {error, Reason} ->
            exit(Reason)
    end.

-spec application_stop([Application :: atom()]) -> ok.

application_stop(Apps) ->
    _ = [application:stop(App) || App <- lists:reverse(Apps)],
    ok.