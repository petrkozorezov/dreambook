-module(dreambook_server).

-include("logger.hrl").

%%-export([start/0, start/1, stop/1]).
-define(INIT_BALANCE, 10).

-export([
		 get_balance/1,
		 get_history/1,
		 is_in_history/2,
		 find_books/2,
		 find_keywords/2,
		 find_meaning/3
		]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Начало файла
%%% Параграф
%% Кусок кода
% строка
get_balance(UID) ->
	%%  create user
	Result = dreambook_db_server:get_balance(UID),
	?LOG_DEBUG("get_balance: ~p : ~p", [UID, Result]),
	case Result of
		{error, invalid_uid} ->
			dreambook_db_server:add_user(UID, ?INIT_BALANCE),
			?INIT_BALANCE;
		Value ->
			Value
	end.

get_history(UID) ->
	Result = dreambook_db_server:get_history(UID),
	?LOG_DEBUG("get_history: ~p: ~p", [UID, Result]),
    Result.

is_in_history(UID, Keyword) ->
	Result = dreambook_db_server:in_history(UID, Keyword),
	?LOG_DEBUG("is_in_history: ~p: ~p", [UID, Result]),
    Result.

find_books(UID, Keyword) ->
	Result = dreambook_db_server:find_books(Keyword),
	?LOG_DEBUG("find_books: ~p ~p: ~p", [UID, Keyword, Result]),
    Result.

find_keywords(UID, Keyword) ->
	Result = dreambook_db_server:find_keywords(Keyword),
	?LOG_DEBUG("find_keywords: ~p ~p: ~p", [UID, Keyword, Result]),
    Result.

find_meaning(UID, Book, Keyword) ->
	Result = dreambook_db_server:find_meaning(Keyword, Book),
	?LOG_DEBUG("find_meaning: ~p ~p ~p: ~p", [UID, Book, Keyword, Result]),
    case Result of
        [] ->
			erlang:error(no_meaning);
		{error, Reason} ->
			erlang:error(Reason);
        Data ->
            case dreambook_db_server:in_history(UID, Keyword) of
                true  -> noop;
                false ->
                    case dreambook_db_server:del_balance(UID, 1) of
                        ok ->
                            ok = dreambook_db_server:add_history(UID, Keyword);
                        _  ->
                            erlang:error(no_money)
                    end
            end,
            Data
    end.
