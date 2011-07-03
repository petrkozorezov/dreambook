
-module(dreambook_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Mod, Args), {Name, {Mod, start_link, Args}, permanent, 5000, worker, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Config = application:get_all_env(),
	Web = proplists:get_value(web, Config),
	DB = proplists:get_value(db, Config),
	DBServerName = dreambook_db_server,
    {ok, { {one_for_one, 5, 10}, [
								  ?CHILD(dreambook_web, dreambook_web, [[ {db_server, DBServerName} | Web]])
%%								  ?CHILD(DBServerName, dreambook_db_server, [DB])
								 ]} }.

