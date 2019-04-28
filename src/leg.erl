%%  Description: leg facade
%%

-module(leg).

%% Management Api
-export([start/0, stop/0, set_log_level/1]).

%% Appender Management
-export([add_appender/1, del_appender/1,
         get_appender/1, list_appenders/0]).

%% Logging API
-export([crit/2, err/2, wrn/2, nfo/2, dbg/2, fmt/3]).

-type log_level() :: crit | error | warn | info | debug.
-type appender_opts() :: #{format=>binary(),
                           colorize=>boolean(),
                           timezone=>local|utc}.
-type appender() :: #{id=>atom(), type=>atom(), opts=>appender_opts()}.

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
del_appender(AppenderId) ->
    leg_appender_mgr:del_appender(AppenderId).

-spec get_appender(atom()) -> appender() | {error, atom()}.
get_appender(AppenderId) ->
    leg_appender_mgr:get_appender(AppenderId).

-spec list_appenders() -> [atom()] | {error, atom()}.
list_appenders() ->
    leg_appender_mgr:list_appenders().

%% Logging API ----------------------------------------------------------------

-spec nfo(iolist()|atom(), [term()]|map()) -> ok.
nfo(Fmt, Args) ->
    leg_router:dispatch(compile_log(info, Fmt, Args)).

-spec dbg(iolist()|atom(), [term()]|map()) -> ok.
dbg(Fmt, Args) ->
    leg_router:dispatch(compile_log(debug, Fmt, Args)).

-spec wrn(iolist()|atom(), [term()]|map()) -> ok.
wrn(Fmt, Args) ->
    leg_router:dispatch(compile_log(warn, Fmt, Args)).

-spec err(iolist()|atom(), [term()]|map()) -> ok.
err(Fmt, Args) ->
    leg_router:dispatch(compile_log(error, Fmt, Args)).

-spec crit(iolist()|atom(), [term()]|map()) -> ok.
crit(Fmt, Args) ->
    leg_router:dispatch(compile_log(crit, Fmt, Args)).

-spec fmt(atom(), iolist()|atom(), [term()]|map()) -> ok.
fmt(Level, Fmt, Args) ->
    leg_router:dispatch(compile_log(Level, Fmt, Args)).

%% Internal -------------------------------------------------------------------

compile_log(Level, Fmt, Args) when is_list(Fmt), is_list(Args) ->
    #{level=>Level, time=>os:timestamp(), event=>io_lib:format(Fmt, Args)};
compile_log(Level, Event, Values) when is_map(Values) ->
    Values#{level=>Level, time=>os:timestamp(), event=>Event}.
