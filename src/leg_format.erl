%% Module responsible for rendering messages according to format specification
%%

-module(leg_format).

%% API
-export([render/3]).

%% API ------------------------------------------------------------------------

render(Spec, Log, Opts) when is_binary(Spec) ->
    render(binary_to_list(Spec), Log, Opts);
render(Spec, #{} = Log, #{} = Opts) ->
    DefaultOpts = #{colorize=>false, timezone=>local},
    parse_spec(Spec, Log, maps:merge(DefaultOpts, Opts)).

%% Internal -------------------------------------------------------------------

parse_spec(Spec, Log, Opts) ->
    parse_spec(Spec, Log, Opts, []).

parse_spec([$%, $d|Rest], #{ts:=Ts} = Log, Opts, Res) ->
    parse_spec(Rest, Log, Opts, [ts(date, Ts, Opts)|Res]);
parse_spec([$%, $t|Rest], #{ts:=Ts} = Log, Opts, Res) ->
    parse_spec(Rest, Log, Opts, [ts(time, Ts, Opts)|Res]);
parse_spec([$%, $T|Rest], #{ts:=Ts} = Log, Opts, Res) ->
    parse_spec(Rest, Log, Opts, [ts(time_micro, Ts, Opts)|Res]);
parse_spec([$%, $l|Rest], #{level:=Level} = Log, Opts, Res) ->
    parse_spec(Rest, Log, Opts, [format_level(Level, Opts)|Res]);
parse_spec([$%, $m|Rest], #{msg:=Msg} = Log, Opts, Res) ->
    parse_spec(Rest, Log, Opts, [Msg|Res]);
parse_spec([C|Rest], Log, Opts, Res) ->
    parse_spec(Rest, Log, Opts, [C|Res]);
parse_spec([], _, _, Res) ->
    iolist_to_binary(lists:reverse(Res)).

ts(date, Ts, #{colorize:=Colorize, timezone:=Tz}) ->
    {{Y, M, D}, _} = get_date_time(Ts, Tz),
    colorize(date, [dt(Y), $-, dt(M), $-, dt(D)], Colorize);
ts(time, Ts, #{colorize:=Colorize, timezone:=Tz}) ->
    {_, {H, M, S}} = get_date_time(Ts, Tz),
    colorize(time, [dt(H), $:, dt(M), $:, dt(S)], Colorize);
ts(time_micro, {_, _, Micro} = Ts, #{colorize:=Colorize, timezone:=Tz}) ->
    {_, {H, M, S}} = get_date_time(Ts, Tz),
    MicroSecs = S + Micro / 1000000,
    colorize(time, [dt(H), $:, dt(M), $:, ft(MicroSecs)], Colorize).

get_date_time(Ts, local) ->
    calendar:now_to_local_time(Ts);
get_date_time(Ts, utc) ->
    calendar:now_to_universal_time(Ts).

dt(I) when I < 10 -> [$0, $0+I];
dt(I) -> integer_to_list(I).

ft(F) when F < 10.0 -> [$0, float_to_list(F, [{decimals, 6}])];
ft(F) -> float_to_list(F, [{decimals, 6}]).

colorize(date, IoData, true) ->
    ["\e[0;32m", IoData, "\e[0m"];
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
colorize(_, IoData, false) ->
    IoData.

format_level(crit, #{colorize:=Colorize}) ->
    colorize(crit, "crit", Colorize);
format_level(error, #{colorize:=Colorize}) ->
    colorize(error, "error", Colorize);
format_level(warn, #{colorize:=Colorize}) ->
    colorize(warn, "warn", Colorize);
format_level(info, #{colorize:=Colorize}) ->
    colorize(info, "info", Colorize);
format_level(debug, #{colorize:=Colorize}) ->
    colorize(debug, "debug", Colorize).
