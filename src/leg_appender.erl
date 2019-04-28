%%  Service that handle a single appender (behaviour)
%%
%% ----------------------------------------------------------------------------

-module(leg_appender).

-copyright("Christoffer Vikström <chvi77@gmail.com>").

-behaviour(gen_server).

%% Management API
-export([start_link/3, new/3, del/1]).

%% API
-export([write/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

%% Callbacks ------------------------------------------------------------------

-type log_level() :: crit | error | warn | info | debug.

-callback init(map()) -> {ok, term()} | {error, term()}.
-callback handle_log_message(Level::log_level(),
                             Message::map(),
                             State::term()) -> {ok, NewState::term()}.

%% Management Api -------------------------------------------------------------

start_link(Id, Type, Opts) ->
    gen_server:start_link(?MODULE, [Id, Type, Opts], []).

new(Id, Type, Opts) ->
    leg_appender_sup:add_child([Id, Type, Opts]).

del(Pid) ->
    leg_appender_sup:del_child(Pid).

%% API ------------------------------------------------------------------------

write(Pid, Log) when is_pid(Pid) ->
    gen_server:cast(Pid, {write, Log}).

%% gen_server callbacks -------------------------------------------------------

init([Id, Type, Opts]) ->
    case catch Type:init(Opts) of
        {ok, ModState} ->
            leg_router:add_route(Id, Type, self()),
            {ok, #{type=>Type, mod_state=>ModState}};
        {error, Error} ->
            {stop, Error};
        {'EXIT', Error} ->
            {stop, Error}
    end.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({write, Log}, State) ->
    #{type:=Type, mod_state:=ModState} = State,
    case catch Type:handle_log_message(Log, ModState) of
        {ok, NewModState} ->
            {noreply, State#{mod_state:=NewModState}};
        {'EXIT', Reason} ->
            {stop, Reason, State}
    end;
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    State.

%% Internal -------------------------------------------------------------------
