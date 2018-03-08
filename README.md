## leg - Lightweight Erlang Logger

Requires erlang 18.0 due to its use of maps. Work in progress.

## Example usage

```erlang
leg:start().
leg:nfo("Hello leg:ly world!", []).
```

## Default configuration

```erlang
{leg, [{appenders, [#{id=>tty,
                      type=>leg_appender_console,
                      opts=>#{}}
                   ]},
       {default_level, info}]}
```

## License

Apache license version 2.0. See the LICENSE file for details.
