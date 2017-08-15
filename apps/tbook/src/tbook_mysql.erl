-module(tbook_mysql).

%% ====================================================================
%% API functions
%% ====================================================================

-ifdef(EUNIT).
-compile(export_all).
-endif.

-export([db_init/0, add_movie/3, reserve_seat/2, info/2, title/1, add_title/2, remove_movie/2]).

-include_lib("emysql/include/emysql.hrl").

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec db_init() -> 'ok'.

db_init() ->
    {ok, {DbHost, DbUser, DbPassword, DbDb}} = application:get_env(tbook, mysql),
    emysql:add_pool(?MODULE, [{size, 1}, {host, DbHost}, {user, DbUser}, {password, DbPassword}, {database, DbDb}, {encoding, latin1}]),

    MT = <<"CREATE TABLE IF NOT EXISTS `tbooks` ( `imdb_id` INT NOT NULL, `screen_id` INT NOT NULL, `total` INT NOT NULL, `reserved` INT NOT NULL default '0', PRIMARY KEY (`imdb_id`, `screen_id`))">>,
    AT = <<"CREATE TABLE IF NOT EXISTS `tbooks_titles` ( `imdb_id` INT NOT NULL PRIMARY KEY, `title` CHAR(255))">>,

    #ok_packet{} = emysql:execute(?MODULE, MT),
    #ok_packet{} = emysql:execute(?MODULE, AT),

    ok.



-spec add_movie(ImdbId, ScreenId, Total) -> 'ok' | 'error' when
    ImdbId :: binary(),
    ScreenId :: binary(),
    Total :: integer().

add_movie(ImdbId, ScreenId, Total) when is_number(ImdbId) andalso is_number(ScreenId) andalso is_number(Total) andalso ImdbId > 0 andalso ScreenId > 0 andalso Total > 0 ->
    Q = tbook:to_binary("INSERT INTO `tbooks` (`imdb_id`, `screen_id`, `total`) VALUES (?,?,?)"),
    emysql:prepare(insert_tbook, Q),
    case emysql:execute(?MODULE, insert_tbook, [ImdbId, ScreenId, Total]) of
        #ok_packet{} -> ok;
        _ -> error
    end.



-spec reserve_seat(ImdbId, ScreenId) -> 'ok' | 'error' when
    ImdbId :: integer(),
    ScreenId :: integer().

reserve_seat(ImdbId, ScreenId) when is_number(ImdbId) andalso is_number(ScreenId) ->
    Q = tbook:to_binary("UPDATE `tbooks` SET `reserved` = `reserved` + 1  WHERE `imdb_id`=? AND `screen_id`=? AND `reserved` < `total`"),
    emysql:prepare(reserve_tbook, Q),
    case emysql:execute(?MODULE, reserve_tbook, [ImdbId, ScreenId]) of
        #ok_packet{affected_rows = 1} -> ok;
        #ok_packet{affected_rows = 0} -> error;
        _ -> error
    end.



-spec info(ImdbId, ScreenId) -> 'ok' | 'error' when
    ImdbId :: integer(),
    ScreenId :: integer().

info(ImdbId, ScreenId) when is_number(ImdbId) andalso is_number(ScreenId) ->
    Q = tbook:to_binary("SELECT * FROM `tbooks` WHERE `imdb_id`=? AND `screen_id`=?"),
    emysql:prepare(insert_tbook, Q),
    case emysql:execute(?MODULE, insert_tbook, [ImdbId, ScreenId]) of
        #result_packet{rows = [Row]} -> {ok, Row};
        _ -> error
    end.



-spec title(ImdbId) -> 'ok' | 'error' when
    ImdbId :: integer().

title(ImdbId) when is_number(ImdbId) ->
    Q = tbook:to_binary("SELECT title FROM `tbooks_titles` WHERE `imdb_id`=?"),
    emysql:prepare(get_title, Q),
    case emysql:execute(?MODULE, get_title, [ImdbId]) of
        #result_packet{rows = [Row]} -> {ok, Row};
        _ -> error
    end.



-spec add_title(ImdbId, Title) -> 'ok' | 'error' when
    ImdbId :: integer(),
    Title :: binary().

add_title(ImdbId, Title) when is_number(ImdbId) ->
    Q = tbook:to_binary("INSERT `tbooks_titles` VALUES(?,?)"),
    emysql:prepare(add_title, Q),
    case emysql:execute(?MODULE, add_title, [ImdbId, Title]) of
        #ok_packet{} -> ok;
        _ -> error
    end.


-ifdef(EUNIT).

-spec remove_movie(ImdbId, ScreenId) -> 'ok' when
    ImdbId :: integer(),
    ScreenId :: integer().

remove_movie(ImdbId, ScreenId) when is_number(ImdbId) andalso is_number(ScreenId) ->
    Q = tbook:to_binary("DELETE FROM `tbooks` WHERE `imdb_id`=? AND `screen_id`=?"),
    emysql:prepare(delete_tbook, Q),
    #ok_packet{} = emysql:execute(?MODULE, delete_tbook, [ImdbId, ScreenId]),
    ok.

-endif.