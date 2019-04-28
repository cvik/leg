%% Default console appender plugin
%%
-module(leg_appender_console).

-export([init/1, handle_log_message/2]).

%% leg_appender callbacks -----------------------------------------------------

init(Opts) ->
    {ok, #{opts=>Opts}}.

handle_log_message(Log,  #{opts:=Opts} = State) ->
    Line = leg_format:render(Log, Opts),
    io:format("~ts~n", [Line]),
    {ok, State}.
