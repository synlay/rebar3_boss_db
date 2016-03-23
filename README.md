rebar3_boss_db
=====

A rebar3 plugin for compiling boss_db models.

Build
-----

    $ rebar3 compile

Usage
-----

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_boss_db, {git, "https://github.com/synlay/rebar3_boss_db.git", {branch, "master"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 boss_db
    ===> Fetching rebar3_boss_db
    ===> Compiling rebar3_boss_db
    <Plugin Output>

The `boss_db` model compile function is located under the namespace `boss_db`. To automatically compile `boss_db` models before the normal compilation process, add a `pre` hook to your rebar.config:

    {provider_hooks, [
        {pre, [{compile, {boss_db, compile}}]}
    ]}.
