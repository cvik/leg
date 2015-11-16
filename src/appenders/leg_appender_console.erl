%% Default console appnder plugin
%%
-module(leg_appender_console).

-export([init/1, handle_log_message/2]).

%% leg_appender callbacks -----------------------------------------------------

init(#{}) ->
    {ok, #{}}.

handle_log_message(Message, State) ->
    io:format("~ts~n", [Message]),
    {ok, State}.
