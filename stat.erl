% TODO kurtosis https://de.wikipedia.org/wiki/W%C3%B6lbung_(Statistik)
-module(stat).

-include_lib("eunit/include/eunit.hrl").
-define( assertApprox(Val1,Val2), ?assert(approx(Val1,Val2,1.0e-7)) ).

-export( [minmax/1, print/1, avg/1, variance/1, variance_welford/1,
			median/1, stddev/1, stddev_sample/1, sample_error/1,
			covar/2, covar_sample/2, correlation/2,
			entropy/1
		 ] ).

-record( stats, {	num = 0 ::non_neg_integer(),
					min = 0 ::number(),
					max = 0 ::number(),
					avg = 0 ::number(),
					sum = 0 ::number(),
					var = 0.0 ::float()
				}
		).

-type stats() :: #stats{}.

% @doc return basic statistics: number of elements, min, max, avg, sum, stdev
%
-spec minmax( list(number()) ) -> #stats{}.
minmax( L ) ->
	S = lists:foldl( fun( X, S ) ->
			#stats{
				min = min(X,S#stats.min),
				max = max(X,S#stats.max),
				sum = S#stats.sum + X
			}
		end, #stats{ min=hd(L), max=hd(L) }, L ),
	S#stats{ num=length(L), avg=S#stats.sum/length(L), var=variance_welford(L) }.
	

-spec print( stats() ) -> ok.
print(S) ->
	io:format("num= ~p, min= ~p, avg= ~p, max= ~p, sum= ~p, variance= ~g~n",[S#stats.num,S#stats.min,S#stats.avg,S#stats.max,S#stats.sum,S#stats.var]).


avg(L) ->
	lists:sum(L)/length(L).
	%{S,N} = lists:foldl( fun(E,{S,N}) -> {S + E,N+1} end, {0.0,0}, L ),
	%S/N.
	

variance(L) ->
	Len = length(L),
	Avg = lists:sum(L)/Len,
	lists:foldl( fun(E,Acc) -> (E-Avg)*(E-Avg) + Acc end, 0, L) / Len.


variance_welford(L) ->
	% https://jonisalonen.com/2013/deriving-welfords-method-for-computing-variance/
	{ N,_,S } = lists:foldl( fun(Element,{Index,Mean,Sum}) ->
								Diff = Element-Mean,
								M2 = Mean + Diff/Index,
								Sum2 = Sum + (Element-M2)*Diff,
								{ Index+1, M2, Sum2 }
							end,
							{1,0.0,0.0},L
						),
	S/(N-1).
		

stddev(L) when is_list(L) ->
	math:sqrt(variance(L)).

stddev_sample(L) when is_list(L) ->
	% https://en.wikipedia.org/wiki/Standard_deviation
	math:sqrt(sample_var(L)).

% SEM https://en.wikipedia.org/wiki/Standard_error
sample_error(L) ->
	stddev_sample(L)/math:sqrt(length(L)).


covar( A1,A2 ) ->
	_ = length(A1) == length(A2),
	loop(A1,A2,avg(A1),avg(A2),0) / length(A1).


covar_sample( A1,A2 ) ->
	_ = length(A1) == length(A2),
	loop(A1,A2,avg(A1),avg(A2),0) / (length(A1)-1.0).


correlation(A1,A2) ->
	covar(A1,A2) / ( stddev(A1)*stddev(A2)).

median(L) ->
	Len = length(L),
	case Len rem 2 of
		1 ->
			quickselect(L,Len div 2 );
		0 ->
			(quickselect(L,Len div 2) + quickselect(L,Len div 2 - 1))*0.5
	end.

% shannon entropy, https://en.wikipedia.org/wiki/Entropy_(information_theory)
-spec entropy(list(number())) -> float().
entropy(L) ->
	- lists:foldl( fun(P,Acc) -> Acc + ex(P) end, 0.0, L).

% -------- private ------------

ex(V) when V == 0.0 -> 0.0;

ex(V) ->
	V*math:log2(V).


sample_var(L) when is_list(L) ->
	Len = length(L),
	Avg = lists:sum(L)/Len,
	lists:foldl( fun(E,Acc) -> (E-Avg)*(E-Avg) + Acc end, 0, L) / (Len-1).


loop([],_,_,_,Sum) ->
	Sum;

loop([H1|R1],[H2|R2],Avg1,Avg2,Sum) ->
	loop(R1,R2,Avg1,Avg2, Sum + (H1-Avg1)*(H2-Avg2) ).

quickselect(L,_) when length(L) == 1 ->
	hd(L);

quickselect(L,K) ->
	%Pivot = lists:nth(rand:uniform(length(L)),L),
	Pivot = hd(L),
	{Lows,Highs,Pivs} = psplit(L,Pivot),

	Llow = length(Lows),
	if
		K < Llow ->
			quickselect(Lows,K);
		K < Llow+length(Pivs) ->
			hd(Pivs);
		true ->
			quickselect(Highs,K - Llow - length(Pivs))
	end.
	
	
psplit(L,Pivot) ->
	{Low,Temp} = lists:partition( fun(E) -> E < Pivot end, L),
	{P,High} = lists:partition( fun(E) -> E > Pivot end, Temp),
	{Low,P,High}.

% --------- test ------------
approx(A,B,Eps) ->
	abs(A - B) < Eps.

s_test() ->
	L = [ 0,1,2,3,4,5 ],
	io:format("~p~n",[L]),
	minmax(L).

var_test() ->
	L = [17,12,15,12,14],
	?assertApprox(3.6,variance(L)),
	?assertApprox(3.6,variance_welford(L)),
	?assertApprox(1.8973666,stddev(L)).

psplit_test() ->
	L = [17,12,15,12,14,0,99],
	?assertEqual({[12,12,0],[17,15,14,99],[]},psplit(L,13)),
	?assertEqual({[0],[17,15,14,99],[12,12]},psplit(L,12)),
	?assertEqual({[12,12,0],[17,15,99],[14]},psplit(L,14)),
	?assertEqual({[17,12,15,12,14,0,99],[],[]},psplit(L,130)).

median_test() ->
	?assertEqual(57,median([60,53,168,59,52,55,57])),
	?assertEqual(4,median([4, 1, 15, 2, 4, 5, 4])),
	?assertEqual(30,median([11,23,30,47,56])),
	?assertEqual(38.5,median([11,23,30,47,52,56])).
