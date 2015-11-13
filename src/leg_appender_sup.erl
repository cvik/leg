%%  Supervisor for leg_appender jobs
%%
%% ----------------------------------------------------------------------------

-module(leg_appender_sup).

-copyright("Christoffer Vikström <chvi77@gmail.com>").

-export([start_link/0, add_child/1, del_child/1]).

-export([init/1]).

-behaviour(supervisor).

%% ----------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_arg).

add_child(Args) ->
    supervisor:start_child(?MODULE, Args).

del_child(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

%% ----------------------------------------------------------------------------

init(no_arg) ->
    Appender = child(leg_appender, leg_appender, worker, []),
    Strategy = {simple_one_for_one, 1, 60},
    {ok, {Strategy, [Appender]}}.

%% ----------------------------------------------------------------------------

child(Name, Mod, Type, Args) ->
    {Name, {Mod, start_link, Args}, transient, 3000, Type, [Mod]}.
