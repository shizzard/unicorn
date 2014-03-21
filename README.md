unicorn
=======

Simple config-server.

About
=====

Unicorn is a basic config server that is responsible for your applications configuration management.

Most simple workflow is:

 - `unicorn:use/1` to start config file worker;
 - `unicorn:subscribe/2` to subscribe for config changes;
 - `unicorn:unsubscribe/1,2` to unsubscribe;
 - `unicorn:reload/1` to reload config and notify all subscribers.

Multiple subscriptions is not an error: subscriber will receive multiple messages by number of subscriptions on config change.

Development usage
=================

Clone repository, run `make dev` and use standalone app.

ToDo
====

Things to do are:

 - `unicorn_worker:do_diff/2` should build diffs between two nested proplists;
 - `unicorn_worker:do_notify/2` should notify all subscribers.