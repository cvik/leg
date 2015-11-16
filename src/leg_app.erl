%%  Description: leg_app
%%

-module(leg_app).

-behaviour(application).

%% application callbacks
-export([start/2, prep_stop/1, stop/1]).

-export([setup_error_logger_handler/0]).

%% application callbacks ------------------------------------------------------

start(normal, no_arg) ->
    case leg_sup:start_link() of
        {ok, Pid} ->
            setup_appenders(),
            setup_error_logger_handler(),
            {ok, Pid};
        {error, Error} ->
            {error, Error}
    end.

prep_stop(State) ->
    State.

stop(_) ->
    leg_error_logger_handler:del(),
    ok.

%% Internal -------------------------------------------------------------------

setup_appenders() ->
    case application:get_env(leg, appenders) of
        undefined ->
            ok;
        {ok, Appenders} ->
            [leg_appender_mgr:add_appender(A) || A <- Appenders]
    end.

setup_error_logger_handler() ->
    {ok, Opts} = application:get_env(leg, error_logger_opts, {ok, #{}}),
    leg_error_logger_handler:add(Opts).
