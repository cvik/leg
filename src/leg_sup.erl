%%  Description: leg_sup
%%

-module(leg_sup).

-behaviour(supervisor).

%% Management Api
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% Management Api -------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_arg).

%% supervisor callbacks -------------------------------------------------------

init(no_arg) ->
    AppenderSup = child(leg_appender_sup, leg_appender_sup, worker, []),
    Router = child(leg_router, leg_router, worker, []),
    AppenderMgr = child(leg_appender_mgr, leg_appender_mgr, worker, []),
    Strategy = {one_for_one, 1, 5},
    {ok, {Strategy, [AppenderSup, Router, AppenderMgr]}}.

%% Internal -------------------------------------------------------------------

child(Name, Mod, Type, Args) ->
    {Name, {Mod, start_link, Args}, permanent, 3000, Type, [Mod]}.
