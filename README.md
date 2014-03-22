unicorn
=======

Simple config-server.

About
=====

Unicorn is a basic config server that is responsible for your applications configuration management.

Most simple workflow is:

 - `unicorn:load/1` to start config file worker;
 - `unicorn:subscribe/2` to subscribe for config changes;
 - `unicorn:unsubscribe/1,2` to unsubscribe;
 - `unicorn:reload/1` to reload config and notify all subscribers.
 - `unicorn:unload/1` to terminate worker.

Multiple subscriptions is not an error: subscriber will receive multiple messages by number of subscriptions on config change.

Development usage
=================

Clone repository, collect deps (actually, `etoml`), run `make dev` and use standalone app.

Load `priv/test.toml` file and subscribe for changes:

```erlang
shizz@shizz-worktop:~/code/__my/local/unicorn [develop [68b9714] MOD UTR] > make dev
rebar -DDEBUG compile
==> etoml (compile)
==> unicorn (compile)
Compiled src/unicorn_worker.erl
erl -sname unicorn -cookie unicorn -pa ebin -pa deps/etoml/ebin -s unicorn dev_start
Erlang R15B02 (erts-5.9.2) [source] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9.2  (abort with ^G)
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

After that copy `test2.toml` to `test.toml` or just make some changes to the file:

```sh
shizz@shizz-worktop:~/code/__my/local/unicorn [develop [68b9714] MOD UTR] > cp priv/test2.toml priv/test.toml
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

ToDo
====

Things to do are:

 - Write documentation