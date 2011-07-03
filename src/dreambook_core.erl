-module(dreambook_core).

-behavior(gen_server).

-include("logger.hrl").

-export([start_link/1, start_link/2, stop/1, stop/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

start_link(Options)            -> start_link( {local, ?MODULE}, Options ).
start_link(Name, Options)      -> gen_server:start_link( Name, ?MODULE, Options, [] ).
stop(Pid)           -> stop(Pid, shutdown).
stop(Pid, Reason)   -> gen_server:call(Pid, {shutdown, Reason}, infinity).


init(Options) ->
    process_flag(trap_exit, true),

    DBServer = proplists:get_value(db_server, Options, 3306),
    {ok, DBServer}.

handle_call(Msg, _, State) ->
    ?LOG_ERROR(": unexpected call received: ~p", [Msg]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?LOG_ERROR(": unexpected cast received: ~p", [Msg]),
    {noreply, State}.

handle_info( {'EXIT', Pid, Reason}, State ) ->
    ?LOG_INFO(": unexpected exit signal received from ~p: ~p", [Pid, Reason]),
    {noreply, State};

handle_info( Msg, State ) ->
    ?LOG_ERROR(": unexpected info received: ~p", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(Reason, _State) ->
    ?LOG_DEBUG(": terminated with reason ~p", [Reason]),
    ok.
