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

add_appender(#{id:=Id, type:=Type, opts:=#{}} = Appender) when is_atom(Type),
                                                               is_atom(Id) ->
    gen_server:cast(?MODULE, {add_appender, Appender});
add_appender(_) ->
    {error, badarg}.

del_appender(Id) ->
    gen_server:cast(?MODULE, {del_appender, Id}).

get_appender(Id) ->
    gen_server:call(?MODULE, {get_appender, Id}).

list_appenders() ->
    gen_server:call(?MODULE, list_appenders).

%% gen_server callbacks -------------------------------------------------------

init(_) ->
    {ok, #{appenders=>#{}}}.

handle_call({get_appender, Id}, _, #{appenders:=Appenders} = State) ->
    case Appenders of
        #{Id:=Appender} ->
            {reply, Appender, State};
        _ ->
            {reply, {error, not_found}, State}
    end;
handle_call(list_appenders, _, #{appenders:=Appenders} = State) ->
    F = fun(Id, #{type:=Type}, M) -> M#{Id=>Type} end,
    {reply, maps:fold(F, #{}, Appenders), State};
handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({add_appender, Appender}, #{appenders:=Appenders} = State) ->
    #{id:=Id, type:=Type, opts:=Opts} = Appender,
    {ok, Pid} = leg_appender:new(Type, Opts),
    ok = leg_router:add_route(Id, Type, Pid),
    {noreply, State#{appenders=>Appenders#{Id=>Appender#{pid=>Pid}}}};
handle_cast({del_appender, Id}, #{appenders:=Appenders} = State) ->
    case Appenders of
        #{Id:=#{pid:=Pid}} ->
            ok = leg_router:del_route(Id),
            ok = leg_appender:del(Pid),
            {noreply, State#{appenders=>maps:remove(Id, Appenders)}};
        _ ->
            {noreply, State}
    end;
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%% Internal -------------------------------------------------------------------
