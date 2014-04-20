-module(noise_erlang_util).

-include("noise_erlang.hrl").

export([]).

seed(seedValue, State) -> 
    NewValue = if
        (seedValue > 0 andalso seedValue < 1) ->
            %%Scale the seed out
            seedValue * 65536;
        true ->
            seedValue
        end,
    NewValue2 = floor(NewValue),
    NewValue3 = if
                    NewValue2 <256 ->
                        TempVal = NewValue2 bsl 8,
                        TempVal bor NewValue2;
                    true ->
                        NewValue2
                end,
    NewState = seed_loop(State, NewValue3, 1),
    NewState.
    

seed_loop(State, Seed, 257) ->
    State;
seed_loop(State, Seed, Count) ->
    Value = if
                Count band 1 == true ->
                    V = p[Count] bxor (Seed band 255);
                true ->
                    V = p[Count] bxor ((Seed bsr 8) band 255)
            end,
    NewState = State#state{
                           perm = lists:concat(State#state.perm, Value),
                           gradP = lists:concat(State#state.perm, lists:nth(v rem 12, ?grad3))
                          },
    seed_loop(NewState, Seed, Count +1).

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.