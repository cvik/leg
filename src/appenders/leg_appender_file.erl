%% Default file appnder plugin
%%
-module(leg_appender_file).

-export([init/1, handle_log_message/2]).

-include_lib("kernel/include/file.hrl").

%% leg_appender callbacks -----------------------------------------------------

init(#{path:=LogPath, name:=Name, max_size:=MaxSize} = Opts) ->
    Filename = setup_new_file(LogPath, Name),
    {ok, Fd} = file:open(Filename, [append]),
    Size = read_file_size(Filename),
    {ok, #{fd=>Fd,
           name=>Name,
           path=>LogPath,
           file=>Filename,
           size=>Size,
           max_size=>MaxSize,
           opts=>Opts}}.

handle_log_message(Log, #{opts:=Opts} = State) ->
    Line = leg_format:render(Log, Opts),
    case should_rotate(Line, State) of
        true ->
            {NewFd, Filename} = rotate_file(State),
            NewState = State#{fd:=NewFd,file:=Filename, size:=0},
            {ok, output_log(Line, NewState)};
        false ->
            {ok, output_log(Line, State)}
    end.

%% Internal -------------------------------------------------------------------

should_rotate(Message, #{size:=CurrentSize, max_size:=MaxSize}) ->
    iolist_size(Message) + CurrentSize >= MaxSize.

rotate_file(#{fd:=Fd, path:=Path, name:=Name}) ->
    file:close(Fd),
    NewFilename = setup_new_file(Path, Name),
    {ok, NewFd} = file:open(NewFilename, [append]),
    {NewFd, NewFilename}.


read_file_size(Filename) ->
    {ok, #file_info{size=Size}} = file:read_file_info(Filename),
    Size.

output_log(Message, #{fd:=Fd, size:=Size} = State) ->
    io:format(Fd, "~ts~n", [Message]),
    State#{size:=(Size+iolist_size(Message))}.

%% TODO: Should only count files starting with Name
%% TODO: Should check if it can continue on an old file instead of a new one
%%       i.e. check if it's size < MaxSize
setup_new_file(Path, Name) ->
    filelib:ensure_dir(filename:join([Path, "a"])),
    {ok, Files} = file:list_dir(Path),
    Filename = io_lib:format("~s.~6.6.0w.log", [Name, length(Files)]),
    filename:join(Path, Filename).
