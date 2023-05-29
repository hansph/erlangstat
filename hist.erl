-module(hist).

-export( [ create/2, create/4, print/1 ] ).

-include_lib("eunit/include/eunit.hrl").

-record( histogram, { min, max, nbins, binsize, frequency } ).

-type histogram() :: #histogram{}.

-export_type( [histogram/0] ).


%
% @param Numbers : a list containing the data
% @param Bins : number of bins, starting from min(Numbers) to max(Numbers)
%
-spec create( list(number()), non_neg_integer() ) -> histogram().
create( Numbers, Bins ) ->
	create(Numbers,Bins,lists:min(Numbers),lists:max(Numbers)).

%
% @param Numbers : a list containing the data
% @param Bins : number of bins
% @param Min : minimum, "left" side of the histogram, everything small than Min is counted in the first bin
% @param Max : maximum, "right" side, everything larger is counted in the last bin
%
-spec create( list(number()), non_neg_integer(), number(), number() ) -> histogram().
create( Numbers, Bins, Min, Max ) ->
	H = #histogram{
		min = Min,
		nbins = Bins,
		binsize = abs((Max-Min)/Bins),
		max = Max - abs((Max-Min)/Bins),
		frequency = []
	},
	Hc = counters:new(Bins,[atomics]),
	hist(Numbers,H,Hc).

-spec print( histogram() ) -> ok.
print( H ) ->
	io:format("Histarray Bins=~p, Min=~p, Max=~p, Binsize=~p~n",[H#histogram.nbins,H#histogram.min,H#histogram.max,H#histogram.binsize]),
	?debugFmt("Histarray Bins=~p, Min=~p, Max=~p, Binsize=~p",[H#histogram.nbins,H#histogram.min,H#histogram.max,H#histogram.binsize]),
	print(H#histogram.nbins, H).

print(0,_) -> ok;

print( I, H ) ->
	Lower = H#histogram.min + (I-1.0)*H#histogram.binsize,
	Upper = Lower + H#histogram.binsize,
	io:format("~p: Val ~f < ~f : ~p~n",[I,Lower, Upper, lists:nth(I,H#histogram.frequency)]),
	?debugFmt("~p: Val ~f < ~f : ~p",[I,Lower, Upper, lists:nth(I,H#histogram.frequency)]),
	print(I-1,H).

%
% ------- private -------
%

% counters to list
-spec carray_tolist(counters:counters_ref()) -> list().
carray_tolist(Arr) ->
	Inf=counters:info(Arr),
	Size=maps:get(size,Inf),
	carray_tolist(Arr, Size, [] ).

carray_tolist(_,0,List) -> List;

carray_tolist(Arr,ArrIter,List) ->
	carray_tolist( Arr, ArrIter-1, [ counters:get(Arr,ArrIter) | List ] ).

%
% create histogram from list
%
-spec hist(list(), histogram(), counters:counters_ref()) -> histogram().
hist([],H,C) ->
	H#histogram{frequency = carray_tolist(C)};

hist([Head|Tail],H,C) ->
	if
		Head < (H#histogram.min + H#histogram.binsize) ->
			%io:format("inc min bin 1~n"),
			counters:add(C, 1, 1);
		Head > H#histogram.max ->
			%io:format("inc bin ~p~n",[index(Histarray#histogram.max,Histarray)]),
			counters:add(C, H#histogram.nbins,1);
		true ->
			X = index(Head,H),
			%io:format("inc bin ~p~n",[X]),
			counters:add(C, X, 1)
	end,
	hist(Tail,H,C).


index(Val,H) ->
	trunc(abs((Val - H#histogram.min)/H#histogram.binsize)) +1.

% ----------- unit test -----------

assertHist( Expected, Hist ) ->
	?debugFmt("--- Input: ~p",[Expected]),
%	print(Hist),
	?assertEqual( Expected, Hist#histogram.frequency ),
	?assertEqual( length(Expected), Hist#histogram.nbins ),
	?assertEqual( length(Hist#histogram.frequency), Hist#histogram.nbins ).

% check if we counted every element; N shall be the length of the original data vector
assertNum(N,H) ->
	?assertEqual( N, lists:sum(H#histogram.frequency) ).

hist_test() ->
	assertHist( [0,0,0,0,0], create([],5,0,10) ),
	assertNum( 0,create([],5,0,10)),

	assertHist( [1,0,0,0,0], create([1],5,0,10) ),
	assertNum( 1,create([1],5,0,10)),
	assertHist( [0,1,0,0,0], create([2],5,0,10) ),

	% outliers
	assertHist( [1,0,0,0,1], create([-1,99],5,0,10) ),

	% https://www.crashkurs-statistik.de/histogramme/
	H2 = [172,164,160,162,173,180,158,185,171,181,162,184,177,175,177,174,158,151,192,177],
	Hist = create(H2,5,150,200),
	assertHist( [3,4,8,4,1],Hist),

	% https://www.wallstreetmojo.com/histogram-examples/
	Wtimes = [2.3,5,3.55,2.5,5.1,4.21,3.33,4.1,2.55,5.07,3.45,4.1,5.12],
	Whist = create(Wtimes,5),
%	print(Whist),
	assertHist([3,1,2,3,4],Whist),

	ok.
