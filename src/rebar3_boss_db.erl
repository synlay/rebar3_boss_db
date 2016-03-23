-module(rebar3_boss_db).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_boss_db_prv:init(State),
    {ok, State1}.
