-module(noise_erlang_util).

-include("noise_erlang.hrl").

-export([seed/2, fade/1, lerp/3, floor/1, dot2/3, dot3/4]).

seed(SeedValue, State) -> 
	io:fwrite("seeding ~n"),
    NewValue = if
        (SeedValue > 0 andalso SeedValue < 1) ->
            %%Scale the seed out
            SeedValue * 65536;
        true ->
            SeedValue
        end,
    NewValue2 = floor(NewValue),
	%%io:fwrite("floored seed ~p~n",[NewValue2]),
    NewValue3 = if
                    NewValue2 <256 ->
                        TempVal = NewValue2 bsl 8,
                        TempVal bor NewValue2;
                    true ->
                        NewValue2
                end,
	%%io:fwrite("NewValue3 ~p~n",[NewValue3]),
    NewState = seed_loop(State, NewValue3, 1),
	%%io:fwrite("NewState#state.gradP ~p~n",[NewState#state.gradP]),
	%%io:fwrite("NewState#state.perm ~p~n",[NewState#state.perm]),
    NewState.
    

seed_loop(State, _Seed, 513) -> %% 512 from the js perlin lib seed function
    State;
seed_loop(State, Seed, Count) ->
	%%io:fwrite("Count ~p~n",[Count]),
	%%io:fwrite("Seed ~p~n",[Seed]),
	
	%% the two try cathes are because the ?p list traversal goes over the list size of 256
	%% javascript automatically handles this case but erlang does not and so must be manually handled
    Value = if
                Count band 1 == true ->
					try lists:nth(Count, ?p) of
						Val ->
							Val bxor (Seed band 255)
					catch
						_Err:_Patt ->
							0 bxor (Seed band 255)
					end;
                true ->
					try lists:nth(Count, ?p) of
						Val ->
							Val bxor ((Seed bsr 8) band 255)
					catch
						_Err:_Patt ->
							0 bxor ((Seed bsr 8) band 255)
					end
            end,
	%%io:fwrite("Value ~p~n",[Value]),
	%%TempVal =lists:append([[1],[a]]),
	%%io:fwrite("TempVal ~p~n",[TempVal]),
	%%io:fwrite("lists:append(State#state.perm, Value) ~p~n",[lists:append(State#state.perm, [Value])]),
	%%io:fwrite("lists:nth(Value rem 12, ?grad3) ~p~n",[lists:nth((Value rem 12)+1, ?grad3)]),
	%%io:fwrite("lists:append(State#state.gradP, lists:nth(Value rem 12, ?grad3)) ~p~n",[lists:append(State#state.gradP, [lists:nth((Value rem 12)+1, ?grad3)])]),
    NewState = State#state{
                           perm = lists:append(State#state.perm, [Value]),
                           gradP = lists:append(State#state.gradP, [lists:nth((Value rem 12)+1, ?grad3)])
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

fade(T) ->
    T * T * T * (T * (T * 6 - 15) + 10).
lerp(A,B,T) ->
    (1 - T) * A + T * B.

dot2(#grad{x = X, y = Y}, X0 , Y0) ->
    X * X0 + Y * Y0.
dot3(#grad{x = X, y = Y, z = Z}, X0 , Y0, Z0) ->
    X * X0 + Y * Y0 + Z * Z0.