%% Module responsible for managing appenders
%%

-module(leg_appender_mgr).


-behaviour(gen_server).

%% Management API
-export([start_link/0]).

%% API
-export([add_appender/1, del_appender/1, get_appender/1, list_appenders/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Management Api -------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Api ------------------------------------------------------------------------

add_appender(#{type:=T, opts:=#{}} = Appender) when is_atom(T) ->
    gen_server:cast(?MODULE, {add_appender, Appender});
add_appender(_) ->
    {error, badarg}.

del_appender(AppenderType) ->
    gen_server:cast(?MODULE, {del_appender, AppenderType}).

get_appender(AppenderType) ->
    gen_server:call(?MODULE, {get_appender, AppenderType}).

list_appenders() ->
    gen_server:call(?MODULE, list_appenders).

%% gen_server callbacks -------------------------------------------------------

init(_) ->
    {ok, #{appenders=>#{}}}.

handle_call({get_appender, Type}, _, #{appenders:=Appenders} = State) ->
    case Appenders of
        #{Type:=Appender} ->
            {reply, Appender, State};
        _ ->
            {reply, {error, not_found}, State}
    end;
handle_call(list_appenders, _, #{appenders:=Appenders} = State) ->
    {reply, maps:keys(Appenders), State};
handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({add_appender, Appender}, #{appenders:=Appenders} = State) ->
    #{type:=Type, opts:=Opts} = Appender,
    {ok, Pid} = leg_appender:new(Type, Opts),
    ok = leg_router:add_route(Type, Pid),
    {noreply, State#{appenders=>Appenders#{Type=>Appender}}};
handle_cast({del_appender, Type}, #{appenders:=Appenders} = State) ->
    ok = leg_router:del_route(Type),
    %% ok = leg_appender:del(Pid),
    {noreply, State#{appenders=>maps:remove(Type, Appenders)}};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%% Internal -------------------------------------------------------------------
