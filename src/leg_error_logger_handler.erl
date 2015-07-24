%% Description: leg_ctrl
%%

-module(leg_error_logger_handler).

-behaviour(gen_event).

%% Management API
-export([add/1, del/0]).

%% gen_server callbacks
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
    {ok, Opts}.

handle_event({info_report, _, {_, Type, Report}}, State) ->
    write_info_report(Type, Report),
    {ok, State};
handle_event({warning_report, _, {_, Fmt, Args}}, State) ->
    leg:wrn(Fmt, Args),
    {ok, State};
handle_event({error_report, _, {_, Type, Report}}, State) ->
    write_error_report(Type, Report),
    {ok, State};
handle_event({info, _, {_, _, Report}}, State) ->
    leg:nfo("~100000p", [Report]),
    {ok, State};
handle_event({error, _, {_, _, Report}}, State) ->
    leg:err("~100000p", [Report]),
    {ok, State};
handle_event(Event, State) ->
    leg:wrn("UNKNOWN: ~100000p", [Event]),
    {ok, State}.

write_info_report(progress, [{application, App}, {started_at, Node}]) ->
    leg:nfo("\e[1;39mPROGRESS:\e[0m application ~p started at ~p",
            [App, Node]);
write_info_report(progress, Event) ->
    leg:dbg("\e[1;39mPROGRESS:\e[0m ~100000p", [Event]);
write_info_report(std_info, [{application,App}, {exited,Reason}|_]) ->
    leg:nfo("\e[1;39mPROGRESS:\e[0m application ~p exited with reason ~p",
            [App, Reason]);
write_info_report(std_info, Event) ->
    leg:nfo("~100000p", [Event]);
write_info_report(Type, Event) ->
    leg:nfo("~p: ~1000000p", [Type, Event]).

write_error_report(std_error, Error) ->
    leg:err("~p", [Error]);
write_error_report(crash_report, Error) ->
    leg:err("\e[1;39mCRASH\e[0m ~100000p", [Error]);
write_error_report(supervisor_report, Error) ->
    leg:err("\e[1;39mSUPERVISOR_REPORT\e[0m ~100000p", [Error]);
write_error_report(Type, Event) ->
    leg:err("~p: ~100000p", [Type, Event]).

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
    Handlers = sys:get_state(error_logger),
    [error_logger:delete_report_handler(element(1, H)) || H <- Handlers,
                                                          H /= ?MODULE].
