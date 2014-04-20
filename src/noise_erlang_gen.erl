-module(noise_erlang_gen).

-include("noise_erlang.hrl").

-behaviour(gen_server).

-export([]).

-record(state,  {perm, gradP}).

init(Arg) ->
    State = #state{perm = [], gradP = []},
    NewState = noise_erlang_gen:seed(0, State),
    {ok, NewState}.

    