unicorn
=======

Unicorn is a basic config server that gives you ability to subscribe your application processes to exact parts of config file (or files) and read events when config is changed.

Usage
=====

Unicorn app consists of a root supervisor and a number of workers. When you load config file another workers starts and serves your requests.

Interface:

 - `unicorn:load/3` to load config file (e.g. start worker);
 - `unicorn:subscribe/1,2` to subscribe process for config changes;
 - `unicorn:unsubscribe/1,2` to unsubscribe process;
 - `unicorn:unload/1` to unload config file (e.g. stop worker);
 - `unicorn:reload/1` to reload config file and notify subscribers.

Loader and document
===================

Loader is function that is responsible for parsing of your config file format. Loader is second argument to `unicorn:load/3` function and is very important.

The one agrument of loader function is `binary()`, contents of config file. Function should return `{ok, Document} | {error, Reason}`. Document is `unicorn:document/1` term, that in fact is a term in jiffy-like json notation. This notation is a proplist with tupled "objects". For example, proplist

```erlang
[
    {<<"foo">>, 1},
    {<<"bar">>, [1,2,3,4]},
    {<<"baz">>, [
        {<<"bux">>, <<"buz">>}
    ]}
]
```

will be following term in jiffy-like json notation:

```erlang
{[
    {<<"foo">>, 1},
    {<<"bar">>, [1,2,3,4]},
    {<<"baz">>, {[
        {<<"bux">>, <<"buz">>}
    ]}}
]}
```

If your parser returns exact proplist, you can use `unicorn:to_document/1` function to transform proplist to document.

Validator
=========

Validator is another function, that is passed to `unicorn:load/3` function. Validator is responsible for config file validation stuff.

The one argument of validator function is `unicorn:document/0` term. Function should return `{ok, Document} | {error, ErrorList}`. As validation routines are optional, you can use `fun(Document) -> {ok, Document} end.` fun to stub validation.

`ErrorList` is obviously a list of errors. This list will return on erroneous `unicorn:load/3` and `unicorn:reload/1` calls.

Test usage
==========

Test unicorn launch uses TOML as config format (usin `etoml` as parser) and `jiffy_v` as validator.

Clone repository, run `make dev` and use standalone app.

Load `priv/test.toml` file and subscribe for changes:

```erlang
$ make dev
rebar -Crebar_dev.config get-deps
==> etoml (get-deps)
==> jiffy_v (get-deps)
==> unicorn (get-deps)
rebar -Crebar_dev.config -DUNICORN_DEVEL compile
==> etoml (compile)
==> jiffy_v (compile)==> unicorn (compile)erl -sname unicorn -cookie unicorn -pa ebin -pa deps/etoml/ebin -pa deps/jiffy_v/ebin -s unicorn dev_startErlang R16B03-1 (erts-5.10.4) [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V5.10.4  (abort with ^G)
(unicorn@shizz-worktop)1> unicorn:load(<<"priv/test.toml">>).
[unicorn debug] 'priv/test.toml' started for '<<"priv/test.toml">>' file
ok
(unicorn@shizz-worktop)2> unicorn:subscribe(<<"priv/test.toml">>, [<<"database">>]).
[unicorn debug] <0.44.0> subscribed for 'priv/test.toml':[<<"database">>]
ok
(unicorn@shizz-worktop)3> unicorn:subscribe(<<"priv/test.toml">>, [<<"worker">>]).
[unicorn debug] <0.44.0> subscribed for 'priv/test.toml':[<<"worker">>]
ok
```

After that make some changes in the file:

```sh
$ cp priv/test3.toml priv/test.toml
```

Reload config and get some incoming messages:

```erlang
(unicorn@shizz-worktop)4> unicorn:reload(<<"priv/test.toml">>).
[unicorn debug] 'priv/test.toml' received reload signal
[unicorn debug] Diff for file <<"priv/test.toml">>: [{<<"database">>,
                                                      [{<<"max_overflow">>,0},
                                                       {<<"pool_size">>,200}]},
                                                     {<<"binding">>,
                                                      [{<<"proto1">>,
                                                        [{<<"port">>,8713}]}]},
                                                     {<<"worker">>,
                                                      [{<<"proto2">>,
                                                        [{<<"transaction_loglevel">>,
                                                          4}]}]}]
[unicorn debug] 'priv/test.toml' notified 2 subscribers
{ok,2}
(unicorn@shizz-worktop)5> flush().
Shell got {unicorn_notify,<<"priv/test.toml">>,
                          [<<"database">>],
                          [{<<"max_overflow">>,0},{<<"pool_size">>,200}],
                          [{<<"max_overflow">>,0},
                           {<<"pool_size">>,200},
                           {<<"port">>,1234},
                           {<<"server">>,<<"1.2.3.4">>}]}
Shell got {unicorn_notify,<<"priv/test.toml">>,
                          [<<"worker">>],
                          [{<<"proto2">>,[{<<"transaction_loglevel">>,4}]}],
                          [{<<"proto2">>,
                            [{<<"transaction_loglevel">>,4},
                             {<<"max_overflow">>,200},
                             {<<"pool_size">>,400}]},
                           {<<"proto1">>,
                            [{<<"transaction_loglevel">>,2},
                             {<<"max_overflow">>,200},
                             {<<"pool_size">>,300}]}]}
ok
```

Unload the file and get termination messages:

```erlang
(unicorn@shizz-worktop)6> unicorn:unload(<<"priv/test.toml">>).
ok
(unicorn@shizz-worktop)7> flush().
Shell got {unicorn_terminate,<<"priv/test.toml">>,[<<"database">>]}
Shell got {unicorn_terminate,<<"priv/test.toml">>,[<<"worker">>]}
ok
(unicorn@shizz-worktop)8>
(unicorn@shizz-worktop)8>
```

Test usage can be found in `unicorn.erl` file in `-ifdef(UNICORN_DEVEL).` section.

ToDo
====

 - Write tests