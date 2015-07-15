%%  Service that handle a single appender
%%
%% ----------------------------------------------------------------------------

-module(leg_appender).

-copyright("Christoffer Vikström <chvi77@gmail.com>").

-behaviour(gen_server).

%% Management API
-export([start_link/2, new/2, del/1]).

%% API
-export([write/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

%% Callbacks ------------------------------------------------------------------

-type log_level() :: crit | error | warn | info | debug.

-callback init(map()) -> {ok, term()} | {error, term()}.
-callback handle_log_message(Level::log_level(),
                             Message::iodata(),
                             State::term()) -> {ok, NewState::term()}.

%% Management Api -------------------------------------------------------------

start_link(Type, Opts) ->
    gen_server:start_link(?MODULE, [Type, Opts], []).

new(Type, Opts) ->
    leg_appender_sup:add_child([Type, Opts]).

del(Pid) ->
    leg_appender_sup:del_child(Pid).

%% API ------------------------------------------------------------------------

write(Pid, Log) when is_pid(Pid) ->
    gen_server:cast(Pid, {write, Log}).

%% gen_server callbacks -------------------------------------------------------

init([Type, Opts]) ->
    DefaultOpts = #{format=><<"%d %t [%l] %m">>},
    FullOpts = maps:merge(DefaultOpts, Opts),
    case catch Type:init(Opts) of
        {ok, ModState} ->
            {ok, #{type=>Type, mod_state=>ModState, opts=>FullOpts}};
        {error, Error} ->
            {stop, Error};
        {'EXIT', Error} ->
            {stop, Error}
    end.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({write, Log}, State) ->
    #{type:=Type, mod_state:=ModState, opts:=Opts} = State,
    #{format:=Spec} = Opts,
    Message = leg_format:render(Spec, Log, Opts),
    case catch Type:handle_log_message(Message, ModState) of
        ok ->
            {noreply, State};
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
