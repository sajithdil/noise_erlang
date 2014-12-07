-module(noise_erlang_gen).

-include("noise_erlang.hrl").

-behaviour(gen_server).

-export([handle_call/3, code_change/3, handle_cast/2, handle_info/2, init/1, terminate/2, start_link/0]).

start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [] ,[]).

init(_Arg) ->
	io:fwrite("starting noise erlang server ~n"),
    State = #state{perm = [], gradP = [], f2 = 0.5*(math:sqrt(3)-1), g2 = (3-math:sqrt(3))/6, f3 = 1/3, g3 = 1/6},
    NewState = noise_erlang_util:seed(0, State),
	%%io:fwrite("perm: ~p~n",[NewState#state.perm]),
    %%{ok, State}.
    {ok, NewState}.

handle_call({simplex2, Xin, Yin}, _From, State) ->
	%%io:fwrite("perm: ~p~n",[State#state.perm]),
    S = (Xin+Yin)*State#state.f2,
    I = noise_erlang_util:floor(Xin + S),
    J = noise_erlang_util:floor(Yin + S),
    T = (I + J) * State#state.g2,
    X0 = Xin-I+T,
    Y0 = Yin-J+T,
	
	%io:fwrite("S: ~p, I: ~p, J: ~p, T: ~p, X0: ~p, Y0: ~p ~n",[S,I,J,T,X0,Y0]),
    
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
    
    I2 = I band 255, 
    J2 = J band 255, 
    
	%io:fwrite("I1: ~p, J1: ~p, X1: ~p, Y1: ~p, X2: ~p, Y2: ~p, I2: ~p, J2: ~p ~n",[I1,J1,X1,Y1,X2,Y2,I2,J2]),
	
	% PermHolder = State#state.perm,
	% io:fwrite("PermHolder: ~p~n",[PermHolder]),
	% io:fwrite("islist: ~p~n",[is_list(PermHolder)]),
	% J2Holder = J2 + 1,
	% io:fwrite("islist: ~p~n",[is_list(PermHolder)]),
	% PermVal = lists:nth(J2Holder, [PermHolder]),
	% io:fwrite("nth: ~p~n",[PermVal]),
	

    Gi0 = lists:nth(I2 + 1 + lists:nth(J2 + 1, State#state.perm),State#state.gradP), %% extra +1 because erlang list is 1 based
	%io:fwrite("Gi0: ~p ~n",[Gi0]),
    Gi1 = lists:nth(I2 + I1 + 1 + lists:nth(J2 + J1 + 1, State#state.perm),State#state.gradP), %% extra +1 because erlang list is 1 based
    %io:fwrite("Gi1: ~p ~n",[Gi1]),
	Gi2 = lists:nth(I2 + 1 + 1 + lists:nth(J2 + 1 + 1, State#state.perm),State#state.gradP), %% extra +1 because erlang list is 1 based
    %io:fwrite("Gi2: ~p ~n",[Gi2]),
	
    T0 = 0.5 - X0*X0-Y0*Y0,
	%io:fwrite("T0: ~p ~n",[T0]),
    N0 = if
             T0< 0 ->
                 0;
             true ->
                 T00 = T0 * T0,
                T00 * T00 * noise_erlang_util:dot2(Gi0,X0, Y0)
        end,
	%io:fwrite("N0: ~p ~n",[N0]),
    T1 = 0.5 - X1*X1-Y1*Y1,
	%io:fwrite("T1: ~p ~n",[T1]),
    N1 = if
             T1<0 ->
                 0;
             true ->
                 T11 = T1 * T1,
                 T11 * T11 * noise_erlang_util:dot2(Gi1,X1, Y1)
        end,
	%io:fwrite("N1: ~p ~n",[N1]),
    T2 = 0.5 - X2*X2-Y2*Y2,
	%io:fwrite("T2: ~p ~n",[T2]),
    N2 = if
             T2 < 0 ->
                 0;
             true ->
                 T22 = T2 * T2,
                 T22 * T22 * noise_erlang_util:dot2(Gi2,X2, Y2)
        end,
	%io:fwrite("N2: ~p ~n",[N2]),
    NoiseValue = 70 * (N0 + N1 + N2),
	{reply, NoiseValue, State};

handle_call({simplex3, Xin, Yin, Zin}, _From, State) ->
    S = (Xin+Yin+Zin)*State#state.f3,
    I = noise_erlang_util:floor(Xin + S),
    J = noise_erlang_util:floor(Yin + S),
    K = noise_erlang_util:floor(Zin + S),
    T = (I + J + K) * State#state.g3,
    X0 = Xin-I+T,
    Y0 = Yin-J+T,
    Z0 = Zin-K+T,
	%%io:fwrite("S: ~p, I: ~p, J: ~p, K: ~p, T: ~p, X0: ~p, Y0: ~p, Z0: ~p ~n",[S,I,J,K,T,X0,Y0,Z0]),
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
                                        end
                                end,
    X1 = X0 - I1 + State#state.g3,
    Y1 = Y0 - J1 + State#state.g3,
    Z1 = Z0 - K1 + State#state.g3,
    
    X2 = X0 - I2 + 2 * State#state.g3,
    Y2 = Y0 - J2 + 2 * State#state.g3,
    Z2 = Z0 - K2 + 2 * State#state.g3,
    
    X3 = X0 - 1 + 3 * State#state.g3,
    Y3 = Y0 - 1 + 3 * State#state.g3,
    Z3 = Z0 - 1 + 3 * State#state.g3,
	
	%%io:fwrite("X1: ~p,Y1: ~p,Z1: ~p,X2: ~p,Y2: ~p, Z2: ~p,X3: ~p, Y3: ~p, Z3: ~p~n",[X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3]),
    
    I3 = I band 255,
    J3 = J band 255,
    K3 = K band 255,
	
	% io:fwrite("I3: ~p,J3: ~p,K3: ~p,I1: ~p,J1: ~p, K1: ~p,I2: ~p, J2: ~p, K2: ~p~n",[I3,J3,K3,I1,J1,K1,I2,J2,K2]),
	
	
	% io:fwrite("lists:nth(K3 + 1, State#state.perm)~p~n",[lists:nth(K3 + 1, State#state.perm)]),
    % io:fwrite("lists:nth(J3 + 1 + lists:nth(K3 + 1, State#state.perm),State#state.perm)~p~n",
	% [lists:nth(J3 + 1 + lists:nth(K3 + 1, State#state.perm),State#state.perm)]),
	% io:fwrite("I3 + 1 + lists:nth(J3 + 1 + lists:nth(K3 + 1, State#state.perm),State#state.perm)~p~n",
	% [I3 + 1 + lists:nth(J3 + 1 + lists:nth(K3 + 1, State#state.perm),State#state.perm)]),
	
    Gi0 = lists:nth(I3 + 1 + lists:nth(J3 + 1 + lists:nth(K3 + 1, State#state.perm),State#state.perm),State#state.gradP), %% extra +1 because erlang list is 1 based
    Gi1 = lists:nth(I3 + I1 + 1 + lists:nth(J3 + J1 + 1 + lists:nth(K3 + K1 + 1, State#state.perm),State#state.perm),State#state.gradP), %% extra +1 because erlang list is 1 based
    Gi2 = lists:nth(I3 + I2 + 1 + lists:nth(J3 + J2 + 1 + lists:nth(K3 + K2 + 1, State#state.perm),State#state.perm),State#state.gradP), %% extra +1 because erlang list is 1 based
    Gi3 = lists:nth(I3 + 1 + 1 + lists:nth(J3 + 1 + 1 + lists:nth(K3 + 1 + 1, State#state.perm),State#state.perm),State#state.gradP), %% extra +1 because erlang list is 1 based
    
	%%io:fwrite("Gi0: ~p,Gi1: ~p,Gi2: ~p,Gi3: ~p~n",[Gi0,Gi1,Gi2,Gi3]),
	
    T0 = 0.5 - X0*X0-Y0*Y0-Z0*Z0,
	%%io:fwrite("T0~p~n",[T0]),
    N0 = if
             T0< 0 ->
                 0;
             true ->
                 T00 = T0 * T0,
                T00 * T00 * noise_erlang_util:dot3(Gi0,X0, Y0,Z0)
        end,
	%%io:fwrite("N0~p~n",[N0]),
    T1 = 0.5 - X1*X1-Y1*Y1-Z1*Z1,
	%%io:fwrite("T1~p~n",[T1]),
    N1 = if
             T1<0 ->
                 0;
             true ->
                 T11 = T1 * T1,
                 T11 * T11 * noise_erlang_util:dot3(Gi1,X1, Y1, Z1)
        end,
	%%io:fwrite("N1~p~n",[N1]),
    T2 = 0.5 - X2*X2-Y2*Y2-Z2*Z2,
	%%io:fwrite("T2~p~n",[T2]),
    N2 = if
             T2 < 0 ->
                 0;
             true ->
                 T22 = T2 * T2,
                 T22 * T22 * noise_erlang_util:dot3(Gi2,X2, Y2, Z2)
        end,
	%%io:fwrite("N2~p~n",[N2]),
    T3 = 0.5 - X3*X3-Y3*Y3-Z3*Z3,
	%%io:fwrite("T3~p~n",[T3]),
    N3 = if
             T3 < 0 ->
                 0;
             true ->
                 T33 = T3 * T3,
                 T33 * T33 * noise_erlang_util:dot3(Gi3,X3, Y3, Z3)
        end,
	%%io:fwrite("N3~p~n",[N3]),
    NoiseValue = 32 * (N0 + N1 + N2 + N3),
	{reply, NoiseValue, State};

handle_call({perlin2, Xin, Yin}, _From, State) ->
    X2 = noise_erlang_util:floor(Xin),
    Y2 = noise_erlang_util:floor(Yin),
    
    X3 = Xin - X2,
    Y3 = Yin - Y2,
    
    X4 = X2 band 255,
    Y4 = Y2 band 255,
    
    N00 = noise_erlang_util:dot2(lists:nth(X4 + 1 + lists:nth(Y4 + 1, State#state.perm),State#state.gradP),X3,Y3),  %% extra +1 because erlang list is 1 based
    N01 = noise_erlang_util:dot2(lists:nth(X4 + 1 + lists:nth(Y4 + 1 + 1, State#state.perm),State#state.gradP),X3,Y3 -1),  %% extra +1 because erlang list is 1 based
    N10 = noise_erlang_util:dot2(lists:nth(X4 + 1 + 1 + lists:nth(Y4 + 1, State#state.perm),State#state.gradP),X3 -1,Y3),  %% extra +1 because erlang list is 1 based
    N11 = noise_erlang_util:dot2(lists:nth(X4 + 1 + 1 + lists:nth(Y4 + 1 + 1, State#state.perm),State#state.gradP),X3 -1,Y3 -1),  %% extra +1 because erlang list is 1 based
    
    U = noise_erlang_util:fade(X3),
    
    NoiseValue = noise_erlang_util:lerp(    
        noise_erlang_util:lerp(N00, N10, U),
        noise_erlang_util:lerp(N01, N11, U),
       noise_erlang_util:fade(Y3)),
	   {reply, NoiseValue, State};

handle_call({perlin3, Xin, Yin, Zin}, _From, State) ->
    X2 = noise_erlang_util:floor(Xin),
    Y2 = noise_erlang_util:floor(Yin),
    Z2 = noise_erlang_util:floor(Zin),
	
	%% io:fwrite("X2: ~p, Y2: ~p, Z2:~p ~n",[X2,Y2,Z2]),
    
    X3 = Xin - X2,
    Y3 = Yin - Y2,
    Z3 = Zin - Z2,
	
	%% io:fwrite("X3: ~p, Y3: ~p, Z3:~p ~n",[X3,Y3,Z3]),
    
    X4 = X2 band 255,
    Y4 = Y2 band 255,
    Z4 = Z2 band 255,
	
	%% io:fwrite("X4: ~p, Y4: ~p, Z4:~p ~n",[X4,Y4,Z4]),
	
	%% io:fwrite("lists:nth(X4 + lists:nth(Y4 + lists:nth(Z4, State#state.perm), State#state.perm),State#state.gradP):~p ~n",
	%%[lists:nth(X4 + lists:nth(Y4 + lists:nth(Z4, State#state.perm), State#state.perm),State#state.gradP)]),
    N000 = noise_erlang_util:dot3(lists:nth(X4 + 1 + lists:nth(Y4 + 1 + lists:nth(Z4 + 1, State#state.perm), State#state.perm),State#state.gradP),X3,Y3,Z3),  %% extra +1 because erlang list is 1 based
    %% io:fwrite("N000:~p ~n",[N000]),
	
	%% io:fwrite("lists:nth(X4 + lists:nth(Y4 + lists:nth(Z4 + 1, State#state.perm), State#state.perm),State#state.gradP):~p ~n",
	%%[lists:nth(X4 + lists:nth(Y4 + lists:nth(Z4 + 1, State#state.perm), State#state.perm),State#state.gradP)]),
	N001 = noise_erlang_util:dot3(lists:nth(X4 + 1 + lists:nth(Y4 + 1 + lists:nth(Z4 + 1 + 1, State#state.perm), State#state.perm),State#state.gradP),X3,Y3,Z3 -1),  %% extra +1 because erlang list is 1 based
    %% io:fwrite("N000:~p ~n",[N000]),
	
	%% io:fwrite("lists:nth(X4 + lists:nth(Y4 + 1 + lists:nth(Z4, State#state.perm), State#state.perm),State#state.gradP):~p ~n",
	%%[lists:nth(X4 + lists:nth(Y4 + 1 + lists:nth(Z4, State#state.perm), State#state.perm),State#state.gradP)]),
	N010 = noise_erlang_util:dot3(lists:nth(X4 + 1 + lists:nth(Y4 + 1 + 1 + lists:nth(Z4 + 1, State#state.perm), State#state.perm),State#state.gradP),X3,Y3 -1,Z3),  %% extra +1 because erlang list is 1 based
	%% io:fwrite("N010:~p ~n",[N010]),
	
	
	
	%% io:fwrite("lists:nth(Z4 + 1, State#state.perm):~p ~n",
	%%[lists:nth(Z4 + 1, State#state.perm)]),
	%% io:fwrite("lists:nth(Y4 + 1 + lists:nth(Z4 + 1, State#state.perm), State#state.perm):~p ~n",
	%%[lists:nth(Y4 + 1 + lists:nth(Z4 + 1, State#state.perm), State#state.perm)]),
	%% io:fwrite("X4 + lists:nth(Y4 + 1 + lists:nth(Z4 + 1, State#state.perm), State#state.perm):~p ~n",
	%%[X4 + lists:nth(Y4 + 1 + lists:nth(Z4 + 1, State#state.perm), State#state.perm)]),
	%% io:fwrite("259,State#state.gradP):~p ~n",
	%%[lists:nth(259,State#state.gradP)]),
	%% io:fwrite("lists:nth(X4 + lists:nth(Y4 + 1 + lists:nth(Z4 + 1, State#state.perm), State#state.perm),State#state.gradP):~p ~n",
	%%[lists:nth(X4 + lists:nth(Y4 + 1 + lists:nth(Z4 + 1, State#state.perm), State#state.perm),State#state.gradP)]),
    N011 = noise_erlang_util:dot3(lists:nth(X4 + 1 + lists:nth(Y4 + 1 + 1 + lists:nth(Z4 + 1 + 1, State#state.perm), State#state.perm),State#state.gradP),X3,Y3 -1,Z3 -1),  %% extra +1 because erlang list is 1 based
	%% io:fwrite("N011:~p ~n",[N011]),
	
	
	
	
	%% io:fwrite("lists:nth(X4 + 1 + lists:nth(Y4 + lists:nth(Z4, State#state.perm), State#state.perm),State#state.gradP):~p ~n",
	%%[lists:nth(X4 + 1 + lists:nth(Y4 + lists:nth(Z4, State#state.perm), State#state.perm),State#state.gradP)]),
    N100 = noise_erlang_util:dot3(lists:nth(X4 + 1 + 1 + lists:nth(Y4 + 1 + lists:nth(Z4 + 1, State#state.perm), State#state.perm),State#state.gradP),X3 -1,Y3,Z3),  %% extra +1 because erlang list is 1 based
    %% io:fwrite("N100:~p ~n",[N100]),
	
	%% io:fwrite("lists:nth(X4 + 1 + lists:nth(Y4 + lists:nth(Z4 + 1, State#state.perm), State#state.perm),State#state.gradP):~p ~n",
	%%[lists:nth(X4 + 1 + lists:nth(Y4 + lists:nth(Z4 + 1, State#state.perm), State#state.perm),State#state.gradP)]),
	N101 = noise_erlang_util:dot3(lists:nth(X4 + 1 + 1 + lists:nth(Y4 + 1 + lists:nth(Z4 + 1 + 1, State#state.perm), State#state.perm),State#state.gradP),X3 -1,Y3,Z3 -1),  %% extra +1 because erlang list is 1 based
    %% io:fwrite("N101:~p ~n",[N101]),
	
	%% io:fwrite("lists:nth(X4 + 1 + lists:nth(Y4 + 1 + lists:nth(Z4, State#state.perm), State#state.perm),State#state.gradP):~p ~n",
	%%[lists:nth(X4 + 1 + lists:nth(Y4 + 1 + lists:nth(Z4, State#state.perm), State#state.perm),State#state.gradP)]),
	N110 = noise_erlang_util:dot3(lists:nth(X4 + 1 + 1 + lists:nth(Y4 + 1 + 1 + lists:nth(Z4 + 1, State#state.perm), State#state.perm),State#state.gradP),X3 -1,Y3 -1,Z3),  %% extra +1 because erlang list is 1 based
    %% io:fwrite("N110:~p ~n",[N110]),
	
	%% io:fwrite("lists:nth(X4 + 1 + lists:nth(Y4 + 1 + lists:nth(Z4 + 1, State#state.perm), State#state.perm),State#state.gradP):~p ~n",
	%%[lists:nth(X4 + 1 + lists:nth(Y4 + 1 + lists:nth(Z4 + 1, State#state.perm), State#state.perm),State#state.gradP)]),
	N111 = noise_erlang_util:dot3(lists:nth(X4 + 1 + 1 + lists:nth(Y4 + 1 + 1 + lists:nth(Z4 + 1 + 1, State#state.perm), State#state.perm),State#state.gradP),X3 -1,Y3 -1,Z3 -1),  %% extra +1 because erlang list is 1 based
    %% io:fwrite("N111:~p ~n",[N111]),
	
	
    U = noise_erlang_util:fade(X3),
    V = noise_erlang_util:fade(Y3),
    W = noise_erlang_util:fade(Z3),
    
    NoiseValue = noise_erlang_util:lerp(
        noise_erlang_util:lerp(
          noise_erlang_util:lerp(N000, N100, U),
          noise_erlang_util:lerp(N001, N101, U), W),
        noise_erlang_util:lerp(
          noise_erlang_util:lerp(N010, N110, U),
          noise_erlang_util:lerp(N011, N111, U), W),
       V),
	   {reply, NoiseValue, State}.

handle_cast(_Request,State) ->
{reply,{},State}.

handle_info(_Request, State) ->
{reply, {}, State}.

code_change(_OldVsn, State, _Extra)->
{ok, State}.

terminate(Reason, _State)->
{ok, Reason}.