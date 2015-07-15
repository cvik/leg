%%  Description: leg facade
%%

-module(leg).

%% Management Api
-export([start/0, stop/0, set_log_level/1]).

%% Appender Management
-export([add_appender/1, del_appender/1,
         get_appender/1, list_appenders/0]).

%% Logging API
-export([crit/2, err/2, wrn/2, nfo/2, dbg/2]).

-type log_level() :: crit | error | warn | info | debug.
-type appender_opts() :: #{format=>binary(),
                           colorize=>boolean(),
                           timezone=>local|utc}.
-type appender() :: #{type=>atom(), opts=>appender_opts()}.

%% Management Api -------------------------------------------------------------

-spec start() -> {ok, [atom()]} | {error, atom()}.
start() ->
    application:ensure_all_started(?MODULE).

-spec stop() -> ok | {error, atom()}.
stop() ->
    application:stop(?MODULE).

-spec set_log_level(log_level()) -> ok | {error, invalid_level}.
set_log_level(Level) ->
    leg_router:set_log_level(Level).

%% Appender Management --------------------------------------------------------
-spec add_appender(appender()) -> ok | {error, atom()}.
add_appender(Appender) ->
    leg_appender_mgr:add_appender(Appender).

-spec del_appender(atom()) -> ok | {error, atom()}.
del_appender(AppenderType) ->
    leg_appender_mgr:del_appender(AppenderType).

-spec get_appender(atom()) -> appender() | {error, atom()}.
get_appender(AppenderType) ->
    leg_appender_mgr:get_appender(AppenderType).

-spec list_appenders() -> [atom()] | {error, atom()}.
list_appenders() ->
    leg_appender_mgr:list_appenders().

%% Logging API ----------------------------------------------------------------

-spec nfo(iolist(), [term()]) -> ok.
nfo(Fmt, Args) ->
    leg_router:dispatch(compile_log(info, Fmt, Args)).

-spec dbg(iolist(), [term()]) -> ok.
dbg(Fmt, Args) ->
    leg_router:dispatch(compile_log(debug, Fmt, Args)).

-spec wrn(iolist(), [term()]) -> ok.
wrn(Fmt, Args) ->
    leg_router:dispatch(compile_log(warn, Fmt, Args)).

-spec err(iolist(), [term()]) -> ok.
err(Fmt, Args) ->
    leg_router:dispatch(compile_log(error, Fmt, Args)).

-spec crit(iolist(), [term()]) -> ok.
crit(Fmt, Args) ->
    leg_router:dispatch(compile_log(crit, Fmt, Args)).

%% Internal -------------------------------------------------------------------

compile_log(Level, Fmt, Args) ->
    Msg = iolist_to_binary(io_lib:format(Fmt, Args)),
    #{level=>Level, msg=>Msg, ts=>os:timestamp()}.
