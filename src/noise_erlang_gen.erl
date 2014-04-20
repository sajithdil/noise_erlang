-module(noise_erlang_gen).

-include("noise_erlang.hrl").

-behaviour(gen_server).

-export([]).

-record(state,  {perm, gradP, f2, g2, f3, g3}).

init(Arg) ->
    State = #state{perm = [], gradP = [], f2 = 0.5*(math:sqrt(3)-1), g2 = (3-math:sqrt(3))/6, f3 = 1/3, g3 = 1/6},
    NewState = noise_erlang_gen:seed(0, State),
    {ok, NewState}.

handle_call({simplex2, Xin, Yin}, State) ->
    S = (xin+yin)*State#state.f2,
    I = noise_erlang_util:floor(Xin + S),
    J = noise_erlang_util:floor(Yin + S),
    T = (I + J) * State#state.g2,
    X0 = Xin-I+T,
    Y0 = Yin-J+T,
    
    I1 = if
             X0 > Y0 ->
                 1;
             true ->
                 0
        end,
    J1 = if
             X0 > Y0 ->
                 0;
             true ->
                 1
        end,
    X1 = X0 - I1 + State#state.g2,
    Y1 = Y0 - J1 + State#state.g2,
    X2 = X0 - 1 + 2 * State#state.g2,
    Y2 = Y0 - 1 + 2 * State#state.g2,
    
    I2 = I + 256, %%256 instead of 255 because erlang lists is 1 base, not 0 base 
    IJ = I + 256, %%256 instead of 255 because erlang lists is 1 base, not 0 base
    
    Gi0 = lists:nth(I2 + lists:nth(J2 + 1, State#state.perm) + 1,State#state.gradP), %%+ in the lists:nth because erlang lists is 1 based not zero based
    Gi1 = lists:nth(I2 + I1 + lists:nth(J2 + J1 + 1, State#state.perm) + 1,State#state.gradP), %%+ in the lists:nth because erlang lists is 1 based not zero based
    Gi2 = lists:nth(I2 + 1 + lists:nth(J2 + 1 + 1, State#state.perm) + 1,State#state.gradP), %%+ in the lists:nth because erlang lists is 1 based not zero based
    
    T0 = 0.5 - X0*X0-Y0*Y0,
    N0 = if
             T0< 0 ->
                 0;
             true ->
                 T00 = T0 * T0,
                T00 * T00 * noise_erlang_util:dot2(Gi0,X0, Y0)
        end,
    T1 = 0.5 - X1*Y1-Y1*Y1,
    N1 = if
             T1<0 ->
                 0;
             true ->
                 T11 = T1 * T1,
                 T11 * T11 * noise_erlang_util:dot2(Gi1,X1, Y1)
        end,
    T2 = 0.5 - X2*X2-Y2*Y2,
    N2 = if
             T2 < 0 ->
                 0;
             true ->
                 T22 = T2 * T2,
                 T22 * T22 * noise_erlang_util:dot2(Gi2,X2, Y2)
        end,
    70 * (N0 + N1 + N2).
