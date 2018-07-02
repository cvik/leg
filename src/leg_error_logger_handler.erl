%% Description: leg_ctrl
%%

-module(leg_error_logger_handler).

-behaviour(gen_event).

%% Management API
-export([add/1, del/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

%% Management Api -------------------------------------------------------------

%% Api ------------------------------------------------------------------------

add(Opts) ->
    add_error_handler(Opts).

del() ->
    error_logger:delete_report_handler(?MODULE).

%% gen_server callbacks -------------------------------------------------------

init(Opts) ->
    {ok, #{opts=>Opts}}.

handle_event({info_report, _, {_, Type, Report}}, #{opts:=Opts} = State) ->
    write_info_report(Type, Report, Opts),
    {ok, State};
handle_event({warning_report, _, {_, Fmt, Args}}, #{opts:=_Opts} = State) ->
    leg:wrn(Fmt, Args),
    {ok, State};
handle_event({error_report, _, {_, Type, Report}}, #{opts:=Opts} = State) ->
    write_error_report(Type, Report, Opts),
    {ok, State};
handle_event({info_msg, _, {_, Fmt, Args}}, #{opts:=_Opts} = State) ->
    leg:nfo(Fmt, Args),
    {ok, State};
handle_event({warning_msg, _, {_, Fmt, Args}}, #{opts:=_Opts} = State) ->
    leg:wrn(Fmt, Args),
    {ok, State};
handle_event({error, _, {_, Fmt, Args}}, #{opts:=_Opts} = State) ->
    leg:err(Fmt, Args),
    {ok, State};
handle_event(Event, #{opts:=_Opts} = State) ->
    leg:wrn("UNKNOWN: ~100000p", [Event]),
    {ok, State}.

handle_call(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%% Internal -------------------------------------------------------------------

add_error_handler(Opts) ->
    clear_error_logger(),
    case lists:keymember(?MODULE, 1, sys:get_state(error_logger)) of
        true ->
            ok;
        false ->
            error_logger:add_report_handler(?MODULE, Opts)
    end.

clear_error_logger() ->
    ok = error_logger:start(),
    Handlers = sys:get_state(error_logger),
    [error_logger:delete_report_handler(element(1, H)) || H <- Handlers,
                                                          H /= ?MODULE].

write_info_report(progress, [{application, App}, {started_at, Node}], Opts) ->
    log(nfo, "~s: application ~p started at ~p", "PROGRESS", [App, Node], Opts);
write_info_report(progress, Event, Opts) ->
    log(dbg, "~s: ~100000p", "PROGRESS", [Event], Opts);
write_info_report(std_info, [{application,App}, {exited,Reason}|_], Opts) ->
    log(nfo, "~s: application ~p exited with reason ~p", "PROGRESS",
        [App, Reason], Opts);
write_info_report(std_info, Event, Opts) ->
    log(nfo, "~s: {std_info, ~100000p}", "UNKNOWN", [Event], Opts);
write_info_report(Fmt, Args, _Opts) when is_list(Fmt) ->
    leg:nfo(Fmt, Args);
write_info_report(Type, Event, Opts) ->
    log(nfo, "~s: {~p, ~100000p}", "UNKNOWN", [Type, Event], Opts).

write_error_report(crash_report, Error, Opts) ->
    log(err, "~s: ~100000p", "CRASH", [Error], Opts);
write_error_report(supervisor_report, Error, Opts) ->
    log(err, "~s: ~100000p", "SUPERVISOR_REPORT", [Error], Opts);
write_error_report(std_error, Error, Opts) ->
    log(err, "~s: {std_error, ~p}", "UNKNOWN", [Error], Opts);
write_error_report(Fmt, Args, _Opts) when is_list(Fmt) ->
    leg:err(Fmt, Args);
write_error_report(Type, Event, Opts) ->
    log(err, "~s: {~p, ~100000p}", "UNKNOWN", [Type, Event], Opts).

log(Lvl, Fmt, Type, Args, Opts) ->
    leg:Lvl(Fmt, [bold(Type, Opts)|Args]).

bold(Str, #{colorize:=true}) -> ["\e[1;39m", Str, "\e[0m"];
bold(Str, _) -> Str.
