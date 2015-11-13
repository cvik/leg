%% Description: leg_router
%%

-module(leg_router).

-behaviour(gen_server).

%% Management API
-export([start_link/0]).

%% API
-export([add_route/2, del_route/1, dispatch/1, set_log_level/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Management Api -------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Api ------------------------------------------------------------------------

add_route(AppenderType, AppenderPid) ->
    gen_server:cast(?MODULE, {add_route, AppenderType, AppenderPid}).

del_route(AppenderType) ->
    gen_server:cast(?MODULE, {del_route, AppenderType}).

dispatch(Log) ->
    gen_server:cast(?MODULE, {dispatch, Log}).

set_log_level(Level) when is_atom(Level) ->
    case valid_level(Level) of
        true ->
            gen_server:cast(?MODULE, {set_log_level, Level});
        false ->
            {error, invalid_level}
    end.

%% gen_server callbacks -------------------------------------------------------

init(_) ->
    {ok, #{routes=>#{}, limit=>info}}.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({add_route, Type, Pid}, #{routes:=Routes} = State) ->
    {noreply, State#{routes:=Routes#{Type=>Pid}}};
handle_cast({del_route, Type}, #{routes:=Routes} = State) ->
    {noreply, State#{routes:=maps:remove(Type, Routes)}};
handle_cast({dispatch, #{level:=Level} = Log}, State) ->
    #{routes:=Routes, limit:=Limit} = State,
    case should_dispatch(Level, Limit) of
        true ->
            maps:map(fun(_, Pid) -> leg_appender:write(Pid, Log) end, Routes),
            {noreply, State};
        false ->
            {noreply, State}
    end;
handle_cast({set_log_level, Level}, State) ->
    {noreply, State#{limit=>Level}};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%% Internal -------------------------------------------------------------------

should_dispatch(LevelMsg, LevelLimit) ->
    #{LevelMsg:=OrderMsg} = levels(),
    #{LevelLimit:=OrderLimit} = levels(),
    OrderMsg =< OrderLimit.

valid_level(Level) ->
    maps:is_key(Level, levels()).

levels() ->
    {ok, #{} = Levels} = application:get_env(leg, levels),
    Levels.
