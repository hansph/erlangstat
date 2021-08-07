-module(hist).

-export( [ create/4 ] ).

-include_lib("eunit/include/eunit.hrl").

-record( histogram, { min, max, nbins, binsize, counters } ).


%
% @doc create a histogram.
%
% @param Numbers is a list of numbers
% @param Bins is the number of slots (bins)
% @param Min is the smallest number
% @param Max is the largest number
%
% @returns a histogram as a list
%
-spec create( list(number()), non_neg_integer(), number(), number() ) -> list().
create( Numbers, Bins, Min, Max ) ->
	Bs = abs((Max-Min)/Bins),
	H = #histogram{
		binsize = Bs,
		min = Min,
		max = Max,
		counters = counters:new(Bins,[atomics]),
		nbins = Bins
	},
	?debugFmt("Histarray Bins=~p, Min=~p, Max=~p, Binsize=~p~n",[Bins,Min,Max,H#histogram.binsize]),
	carray_tolist(hist(Numbers,H)).


%
% ------- private -------
%

% counters to list
carray_tolist(Arr) ->
	Inf=counters:info(Arr#histogram.counters),
	Size=maps:get(size,Inf),
	carray_tolist(Arr, Size, [] ).

carray_tolist(_,0,List) -> List;

carray_tolist(Arr,ArrIter,List) ->
	carray_tolist( Arr, ArrIter-1, [ counters:get(Arr#histogram.counters,ArrIter) | List ] ).

%
% create histogram from list
%
hist([],Histarray) ->
	Histarray;

hist([Head|Tail],Histarray) ->
	if
		Head =< Histarray#histogram.min ->
			counters:add(Histarray#histogram.counters, 1, 1);
		Head >= Histarray#histogram.max ->
			counters:add(Histarray#histogram.counters, Histarray#histogram.nbins,1);
		true ->
			X = index(Head,Histarray),
			?debugFmt("~p inc bin[~p]~n",[Head,X]),
			counters:add(Histarray#histogram.counters, X, 1)
	end,
	hist(Tail,Histarray).


index(Val,H) ->
	%trunc(abs((Val - H#histogram.min)/H#histogram.binsize)).
	trunc( Val/H#histogram.binsize ) - H#histogram.min.

% ----------- unit test -----------
hist_test() ->
	Nums = [1,2,3,4,4,5,5,4,3],
	H = create(Nums,10,0,10),
	io:format("~p~n",[H]),
	?assertEqual( [1,1,2,3,2,0,0,0,0,0],H ),

	?assertEqual( [0,0,0,0,0,1,1,2,3,2], create(Nums,10,-5,5) ),
	?assertEqual( [1,0,0,1,0,2,0,0,3,2], create(Nums,10,1,5) ),

	%N2 = [-2,3,3,3.33,0,0,4.5,9,12312],
	%io:fwrite("Zahlen: ~w\n", [N2]),
	%print(create(N2,10,0,10)),
	%print(create(N2,10,-5,5)),

	%N3 = [rand:normal() || _ <- lists:seq(1, 100000)],
	%stat:print( stat:minmax(N3) ),
	
	%io:fwrite("Zahlen: ~w\n", [N3]),
	%o:format("~p~n",[create(N3,10,-10,10)] ).
	ok.
