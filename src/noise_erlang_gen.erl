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
    S = (Xin+Yin)*State#state.f2,
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
    J2 = J + 256, %%256 instead of 255 because erlang lists is 1 base, not 0 base
    
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

handle_call({simplex2, Xin, Yin, Zin}, State) ->
    S = (Xin+Yin+Zin)*State#state.f2,
    I = noise_erlang_util:floor(Xin + S),
    J = noise_erlang_util:floor(Yin + S),
    K = noise_erlang_util:floor(Zin + S),
    T = (I + J) * State#state.g2,
    X0 = Xin-I+T,
    Y0 = Yin-J+T,
    Z0 = Zin-K+T,
    {I1, J1, K1, I2, J2, K2} = if
                                   X0 >=Y0 ->
                                       if
                                           Y0 >= Z0 ->
                                               { 1, 0, 0, 1, 1, 0 };
                                           X0 >= Z0 ->
                                               { 1, 0, 0, 1, 0, 1 };
                                            true ->
                                               { 0, 0, 1, 1, 0, 1 }
                                        end;
                                   true ->
                                       if
                                           Y0 < Z0 ->
                                               { 0, 0, 1, 0, 1, 1 };
                                           X0 < Z0 ->
                                               { 0, 1, 0, 0, 1, 1 };
                                            true ->
                                               { 0, 1, 0, 1, 1, 0 }
                                        end;
                                end,
    X1 = X0 - I1 + State#state.g3,
    Y1 = Y0 - J1 + State#state.g3,
    Z1 = Z0 - K1 + State#state.g3,
    
    X2 = X0 - I2 * State#state.g3,
    Y2 = Y0 - J2 * State#state.g3,
    Z2 = Y0 - K2 * State#state.g3,
    
    X3 = X0 - 1 + 3 * State#state.g3,
    Y3 = Y0 - 1 + 3 * State#state.g3,
    Z3 = Y0 - 1 + 3 * State#state.g3,
    
    I3 = I + 256, %%256 instead of 255 because erlang lists is 1 base, not 0 base 
    J3 = J + 256, %%256 instead of 255 because erlang lists is 1 base, not 0 base
    K3 = K + 256, %%256 instead of 255 because erlang lists is 1 base, not 0 base
    
    Gi0 = lists:nth(I3 + lists:nth(J3 + 1, lists:nth(K3 + 1, State#state.perm)) + 1,State#state.gradP), %%+ in the lists:nth because erlang lists is 1 based not zero based
    Gi1 = lists:nth(I3 + I1 + lists:nth(J3 + J1 + 1, lists:nth(K3 + K1 + 1, State#state.perm)) + 1,State#state.gradP), %%+ in the lists:nth because erlang lists is 1 based not zero based
    Gi2 = lists:nth(I3 + I2 + lists:nth(J3 + J2 + 1, lists:nth(K3 + K2 + 1, State#state.perm)) + 1,State#state.gradP), %%+ in the lists:nth because erlang lists is 1 based not zero based
    Gi3 = lists:nth(I3 + 1 + lists:nth(J3 + 1 + 1, lists:nth(K3 + 1 + 1, State#state.perm)) + 1,State#state.gradP), %%+ in the lists:nth because erlang lists is 1 based not zero based
    
    T0 = 0.5 - X0*X0-Y0*Y0-Z0*Z0,
    N0 = if
             T0< 0 ->
                 0;
             true ->
                 T00 = T0 * T0,
                T00 * T00 * noise_erlang_util:dot3(Gi0,X0, Y0,Z0)
        end,
    T1 = 0.5 - X1*Y1-Y1*Y1-Z1*Z1,
    N1 = if
             T1<0 ->
                 0;
             true ->
                 T11 = T1 * T1,
                 T11 * T11 * noise_erlang_util:dot2(Gi1,X1, Y1, Z1)
        end,
    T2 = 0.5 - X2*X2-Y2*Y2-Z2*Z2,
    N2 = if
             T2 < 0 ->
                 0;
             true ->
                 T22 = T2 * T2,
                 T22 * T22 * noise_erlang_util:dot2(Gi2,X2, Y2, Z2)
        end,
    T3 = 0.5 - X3*X3-Y3*Y3-Z3*Z3,
    N3 = if
             T3 < 0 ->
                 0;
             true ->
                 T33 = T3 * T3,
                 T33 * T33 * noise_erlang_util:dot2(Gi3,X3, Y3, Z3)
        end,
    70 * (N0 + N1 + N2 + N3).

handle_call({perlin2, Xin, Yin}, State) ->
    X2 = noise_erlang_util:floor(Xin),
    Y2 = noise_erlang_util:floor(Yin),
    
    X3 = Xin - X2,
    Y3 = Yin - Y2,
    
    X4 = Xin + 255,
    Y4 = Yin + 255,
    
    N00 = noise_erlang_util:dot2(lists:nth(X2 + lists:nth(Y2, State#state.perm),State#state.gradP),X4,Y4),
    N01 = noise_erlang_util:dot2(lists:nth(X2 + lists:nth(Y2 +1, State#state.perm),State#state.gradP),X4,Y4 -1),
    N10 = noise_erlang_util:dot2(lists:nth(X2 + 1 + lists:nth(Y2, State#state.perm),State#state.gradP),X4 -1,Y4),
    N11 = noise_erlang_util:dot2(lists:nth(X2 + 1 + lists:nth(Y2 + 1, State#state.perm),State#state.gradP),X4 -1,Y4 -1),
    
    U = noise_erlang_util:fade(X4),
    
    noise_erlang_util:lerp(    
        noise_erlang_util:lerp(N00, N10, U),
        noise_erlang_util:lerp(N01, N11, U),
       noise_erlang_util:fade(y)).