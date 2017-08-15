-module(tbook_handler).

-ifdef(EUNIT).
-compile(export_all).
-endif.

-export([init/2, handle/2, terminate/3]).


-define(integer(A),tbook:to_integer(A)).
-define(binary(A), tbook:to_binary(A)).

init(Req0, Opts) ->
    handle(Req0, undefined).



handle(Req, State) ->
    Method = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req),
    Req3 = handle(Method, HasBody, Req),
    {ok, Req3, State}.

handle(<<"POST">>, true, Req1) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req1),
    Req = jsx:decode(Body, [return_maps]),
    Ret = handle_post(Req),
    reply(Ret, Req2);

handle(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{ <<"result">> => -1, <<"errorMessage">> => <<"Missing body">> }), Req);

handle(<<"GET">>, true, Req1) ->
    Get = cowboy_req:match_qs([{imdbId, [], undefined}, {screenId, [], undefined}], Req1),
    Ret = handle_get(Get),
    reply(Ret, Req1);

handle(_, _, Req) ->
    cowboy_req:reply(405, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{ <<"result">> => -1, <<"errorMessage">> => <<"Method not allowed.">> }), Req).



handle_post( #{<<"imdbId">> := <<"tt",_/binary>>, <<"screenId">> := <<"screen_",_/binary>>, <<"test">> := _} ) ->
    #{ <<"result">> => 1 };

handle_post( #{<<"imdbId">> := <<"tt",Id/binary>>, <<"screenId">> := <<"screen_",SId/binary>>, <<"availableSeats">> := Seats} ) ->
    case tbook_mysql:add_movie(?integer(Id), ?integer(SId), ?integer(Seats)) of
        ok ->
            spawn( fun() -> add_title(Id) end ), #{ <<"result">> => 1 };
        _ ->
            #{ <<"result">> => 0 }
    end;

handle_post( #{<<"imdbId">> := <<"tt",Id/binary>>, <<"screenId">> := <<"screen_",SId/binary>>} ) ->
    case tbook_mysql:reserve_seat(?integer(Id), ?integer(SId)) of
        ok ->
            #{ <<"result">> => 1 };
        _ ->
            #{ <<"result">> => 0 }
    end.


handle_get( #{imddbId := "tt" ++ Id, screenId := "screen_" ++ SId} ) ->
    case with_title( tbook_mysql:info(?integer(Id), ?integer(SId)) ) of
        error ->
            #{ <<"result">> => 0 };
        Result ->
            Result
    end.



reply({error, _}, Req) ->
    cowboy_req:reply(400, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{<<"result">> => -1}), Req);
reply(Ret, Req) ->
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(Ret), Req).



with_title(error) ->
    error;
with_title({ok, [ImdbId, ScreenId, Total, Reserved]}) ->
    BImdbId = ?binary(ImdbId),
    BScreenId = ?binary(ScreenId),
    case add_title(ImdbId) of
        {ok, Title} ->
            #{ <<"result">> => 1, <<"imdbId">> => <<"tt", BImdbId/binary>>, <<"screenId">> => <<"screen_", BScreenId/binary>>, <<"availableSeats">> => Total, <<"reservedSeats">> => Reserved, <<"movieTitle">> => ?binary(Title)};

        error ->
            #{ <<"result">> => 1, <<"imdbId">> => <<"tt", BImdbId/binary>>, <<"screenId">> => <<"screen_", BScreenId/binary>>, <<"availableSeats">> => Total, <<"reservedSeats">> => Reserved}
    end.



add_title(Id) ->
    case tbook_mysql:title(Id) of
        error ->
            case catch request_title(Id) of
                {ok, Title} ->
                    tbook_mysql:add_title(Id, Title),
                    {ok, Title};
                _ ->
                    error
            end;

        {ok, Title} ->
            {ok, Title};
        _ ->
            error
    end.


request_title(Id) ->
    {ok, {ApiKey, ApiLang}} = application:get_env(tbook, tmdb),

    Url = "https://api.themoviedb.org/3/find/tt" ++ integer_to_list(Id) ++ "?api_key="++ ApiKey ++"&language="++ ApiLang ++"&external_source=imdb_id",
    {ok, {_,_,Ret}}  = httpc:request(Url),

    #{ <<"movie_results">> := [#{ <<"title">> := Title }] } = jsx:decode( ?binary(Ret), [return_maps]),

    {ok, Title}.


terminate(_Reason, _Req, _State) ->
    ok.
