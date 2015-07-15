## leg - Lightweight Erlang Logger

Requires erlang 18.0 due to its use of maps. Work in progress.

Latest Tag: 0.1.0


## Example usage

```erlang
leg:start().
leg:add_appender(#{type=>leg_appender_console, opts=>#{}}).
leg:nfo("Hello leg:ly world!", []).
```

## License

Apache license version 2.0. See the LICENSE file for details.
