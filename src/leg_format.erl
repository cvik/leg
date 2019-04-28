%% Module responsible for rendering messages according to format specification
%%

-module(leg_format).

%% API
-export([render/2]).

%% API ------------------------------------------------------------------------

render(Log, Opts) ->
    DefaultOpts = #{colorize=>true, timezone=>utc,
                    time_format=>default, format=>tty,
                    re_sort=>true},
    FullOpts = maps:merge(DefaultOpts, Opts),
    render_parts(Log, FullOpts).

%% Internal -------------------------------------------------------------------

render_parts(Log, #{format:=tty} = Opts) ->
    render_tty(Log, Opts);
render_parts(Log, #{format:=structured} = Opts) ->
    string:join([render_part(Pair, Opts) || Pair <- maps:to_list(Log)], " ");
render_parts(#{time:=Ts} = Log, #{format:=json} = Opts) ->
    TsStr = render_ts(Ts, Opts#{colorize:=false}),
    JsonMap = render_json(Log#{time=>TsStr}),
    lejson:encode(JsonMap).

render_part({time, Ts}, #{format:=structured, colorize:=Colorize} = Opts) ->
    ColorKey = colorize(key, to_iolist("time"), Colorize),
    [ColorKey, "=", render_ts(Ts, Opts#{colorize:=false})];
render_part({time, Ts}, Opts) ->
    ["time=", render_ts(Ts, Opts)];
render_part({level, Level}, #{format:=structured, colorize:=Colorize} = Opts) ->
    ColorKey = colorize(key, to_iolist("level"), Colorize),
    [ColorKey, "=", render_level(Level, Opts#{colorize:=false})];
render_part({level, Level}, Opts) ->
    ["level=", render_level(Level, Opts)];
render_part({Key, Val}, #{colorize:=Colorize}) ->
    ColorKey = colorize(key, to_iolist(Key), Colorize),
    [io_lib:format("~ts", [ColorKey]), "=", to_iolist(Val), ""].

render_tty(#{time:=Ts, level:=Level, event:=Event} = Log, Opts) ->
    Values = maps:without([time, level, event], Log),
    LevelStr = string:to_upper(render_level(Level, Opts)),
    EventPadded = io_lib:format("~-30.30.\ss", [to_iolist(Event)]),
    Main = [LevelStr, $[, render_ts(Ts, Opts), $], $ , EventPadded],
    ValuePairs = [render_part(Pair, Opts) || Pair <- maps:to_list(Values)],
    [Main, $\t | string:join(ValuePairs, " ")].

to_iolist(T) when is_atom(T) -> atom_to_list(T);
to_iolist(T) when is_integer(T) -> integer_to_list(T);
to_iolist(T) when is_float(T) -> io_lib:format("~w", [T]);
to_iolist(T) when is_pid(T) -> pid_to_list(T);
to_iolist(T) when is_reference(T) -> ref_to_list(T);
to_iolist(T) when is_binary(T) -> T;
to_iolist(T) when is_list(T) -> T.

render_json(Map) ->
    F = fun(K, V, M) -> M#{K=>to_bin(V)} end,
    maps:fold(F, #{}, Map).

to_bin(T) when is_list(T) -> iolist_to_binary(T);
to_bin(T) -> T.

render_ts(NowTs, #{time_format:=TimeFormat, timezone:=Tz, colorize:=Colorize}) ->
    Time = get_date_time(NowTs, Tz),
    colorize(time, time_to_rfc3339(Time, TimeFormat), Colorize).

time_to_rfc3339({{{Y,M,D},{HH,MM,SS}}, _}, default) ->
    [dt(Y), $-, dt(M), $-, dt(D), $T, dt(HH), $:, dt(MM), $:, dt(SS), $Z];
time_to_rfc3339({{{Y,M,D},{HH,MM,SS}}, Us}, micros) ->
    MicrosStr = io_lib:format("~6.6.0w", [Us]),
    [dt(Y), $-, dt(M), $-, dt(D), $T,
     dt(HH), $:, dt(MM), $:, dt(SS), $., MicrosStr, $Z].

get_date_time({_, _, Ms} = Ts, local) ->
    {calendar:now_to_local_time(Ts), Ms};
get_date_time({_, _, Ms} = Ts, utc) ->
    {calendar:now_to_universal_time(Ts), Ms}.

dt(I) when I < 10 -> [$0, $0+I];
dt(I) -> integer_to_list(I).

%ft(F) when F < 10.0 -> [$0, float_to_list(F, [{decimals, 6}])];
%fr(F) -> float_to_list(F, [{decimals, 6}]).

render_level(crit, #{colorize:=Colorize, format:=tty}) ->
    colorize(crit, "CRIT", Colorize);
render_level(crit, #{colorize:=Colorize}) ->
    colorize(crit, "crit", Colorize);
render_level(error, #{colorize:=Colorize, format:=tty}) ->
    colorize(error, "ERRO", Colorize);
render_level(error, #{colorize:=Colorize}) ->
    colorize(error, "error", Colorize);
render_level(warn, #{colorize:=Colorize, format:=tty}) ->
    colorize(warn, "WARN", Colorize);
render_level(warn, #{colorize:=Colorize}) ->
    colorize(warn, "warn", Colorize);
render_level(info, #{colorize:=Colorize, format:=tty}) ->
    colorize(info, "INFO", Colorize);
render_level(info, #{colorize:=Colorize}) ->
    colorize(info, "info", Colorize);
render_level(debug, #{colorize:=Colorize, format:=tty}) ->
    colorize(debug, "DEBU", Colorize);
render_level(debug, #{colorize:=Colorize}) ->
    colorize(debug, "debug", Colorize).

colorize(time, IoData, true) ->
    ["\e[0;32m", IoData, "\e[0m"];
colorize(crit, IoData, true) ->
    ["\e[1;91m", IoData, "\e[0m"];
colorize(error, IoData, true) ->
    ["\e[0;31m", IoData, "\e[0m"];
colorize(warn, IoData, true) ->
    ["\e[0;33m", IoData, "\e[0m"];
colorize(info, IoData, true) ->
    ["\e[0;36m", IoData, "\e[0m"];
colorize(debug, IoData, true) ->
    ["\e[0;35m", IoData, "\e[0m"];
colorize(key, IoData, true) ->
    ["\e[0;36m", IoData, "\e[0m"];
colorize(_, IoData, false) ->
    IoData.
