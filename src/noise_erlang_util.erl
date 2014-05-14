-module(noise_erlang_util).

-include("noise_erlang.hrl").

-export([seed/2, fade/1, lerp/3, floor/1, dot2/3, dot3/4]).

seed(SeedValue, State) -> 
    NewValue = if
        (SeedValue > 0 andalso SeedValue < 1) ->
            %%Scale the seed out
            SeedValue * 65536;
        true ->
            SeedValue
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
                    V = lists:nth(Count, ?p) bxor (Seed band 255);
                true ->
                    V = lists:nth(Count, ?p) bxor ((Seed bsr 8) band 255)
            end,
    NewState = State#state{
                           perm = lists:concat(State#state.perm, Value),
                           gradP = lists:concat(State#state.gradP, lists:nth(Value rem 12, ?grad3))
                          },
    seed_loop(NewState, Seed, Count +1).

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

% ceiling(X) ->
    % T = erlang:trunc(X),
    % case (X - T) of
        % Neg when Neg < 0 -> T;
        % Pos when Pos > 0 -> T + 1;
        % _ -> T
    % end.

fade(t) ->
    t*t*t*(t*(t*6-15)+10).
lerp(a,b,t) ->
    (1-t)*a + t*b.

dot2(#grad{x = X, y = Y}, X0 , Y0) ->
    X * X0 + Y * Y0.
dot3(#grad{x = X, y = Y, z = Z}, X0 , Y0, Z0) ->
    X * X0 + Y * Y0 + Z * Z0.