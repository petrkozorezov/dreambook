-module(dreambook_server).

-include("logger.hrl").

%%-export([start/0, start/1, stop/1]).

-export([
		 get_balance/1,
		 get_history/1,
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
    dreambook_db_server:get_balance(UID).

get_history(UID) ->
    dreambook_db_server:get_history(UID).

find_books(_UID, Keyword) ->
    dreambook_db_server:find_books(Keyword).

find_keywords(_UID, Keyword) ->
    dreambook_db_server:find_keywords(Keyword).

find_meaning(UID, Book, Keyword) ->
    case dreambook_db_server:find_meaning(Keyword, Book) of
        [] -> erlang:error(no_meaning);
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
