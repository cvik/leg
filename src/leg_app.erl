%%  Description: leg_app
%%

-module(leg_app).

-behaviour(application).

%% application callbacks
-export([start/2, prep_stop/1, stop/1]).

%% application callbacks ------------------------------------------------------

start(normal, no_arg) ->
    case leg_sup:start_link() of
        {ok, Pid} ->
            {ok, Opts} = application:get_env(leg, sasl_options, {ok, #{}}),
            leg_error_logger_handler:add(Opts),
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
