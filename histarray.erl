-module(histarray).

-define( BINS,100 ).
-define( MIN,0 ).
-define( MAX,100).
-define( BINSIZE, 10).    % abs((Max-Min)/Bins) ).

-export( [bench/1]).

-compile(export_all).

%Bins: 10, List size: 100000
%counters:  total time taken 0.666062 seconds
%dict:  total time taken 1.971840 seconds
%orddict:  total time taken 0.977413 seconds
%array:  total time taken 1.349028 seconds
%gb_trees:  total time taken 1.050301 seconds
%maps:  total time taken 0.891283 seconds
%tuples:  total time taken 0.593379 seconds

%Bins: 100, List size: 100000
%counters:  total time taken 0.672598 seconds
%dict:  total time taken 1.947804 seconds
%orddict:  total time taken 0.977306 seconds
%array:  total time taken 1.358240 seconds
%gb_trees:  total time taken 1.026203 seconds
%maps:  total time taken 0.903654 seconds
%tuples:  total time taken 0.777824 seconds


% N = Schleifen
bench(N) ->
	L = randlist(100000),
	io:format("Bins: ~p, List size: ~p~n",[?BINS,length(L)]),

	io:format("counters: "),
	time(fun() -> count(L) end,N),

	io:format("dict: "),
	time(fun() -> dict(L) end,N),

	io:format("orddict: "),
	time(fun() -> orddict(L) end,N),

	io:format("array: "),
	time(fun() -> arr(L) end,N),

	io:format("gb_trees: "),
	time(fun() -> gb_trees(L) end,N),

	io:format("maps: "),
	time(fun() -> map(L) end,N),

	io:format("tuples: "),
	time(fun() -> tuple(L) end,N),

	ok.

% -----------

% create a list of size N with random float numbers between 0 and 100
randlist(N) ->
	[rand:uniform()*100 || _ <- lists:seq(1, N)].


time( F, N ) ->
	Start = os:timestamp(),
	timeloop(F,N),
	io:format(" total time taken ~f seconds~n", [timer:now_diff(os:timestamp(), Start) / 1.0e6]).


timeloop( _, 0 ) -> ok;

timeloop(F,N) ->
	%io:format("--- ~p ",[N]),
	_ = F(),
	timeloop(F,N-1).


hist([],Histarray,_) ->
	%io:format("Fin.~n"),
	Histarray;

hist([Head|Tail], Histarray, Incfun) ->
	if
		Head =< ?MIN ->
			H=Incfun(Histarray,1);
		Head >= ?MAX ->
			H=Incfun(Histarray,?BINS);
		true ->
			X = trunc(Head/?BINSIZE) - ?MIN + 1,
			%io:format("~p inc bin[~p]~n",[Head,X]),
			H=Incfun(Histarray,X)
	end,
	hist(Tail,H,Incfun).


% ------------------------
carray_tolist(Arr) ->
	Inf=counters:info(Arr),
	Size=maps:get(size,Inf),
	carray_tolist(Arr, Size, [] ).

carray_tolist(_,0,List) -> List;

carray_tolist(Arr,ArrIter,List) ->
	carray_tolist( Arr, ArrIter-1, [ counters:get(Arr,ArrIter) | List ] ).

% ---------- counters

count(L) ->
	%Counters = counters:new(?BINS,[atomics]),
	Counters = counters:new(?BINS,[write_concurrency]),
	hist(L,Counters,fun(Cref,Pos) -> counters:add(Cref,Pos,1), Cref end),
	carray_tolist(Counters).


dict(L) ->
	dict:to_list(
		hist(L,dict:new(), fun( Dict,Pos) -> dict:update_counter(Pos,1,Dict) end)
	).

orddict(L) ->
	orddict:to_list(
		hist(L,orddict:new(), fun( Dict,Pos) -> orddict:update_counter(Pos,1,Dict) end)
	).

gb_trees(L) ->
	gb_trees:to_list(
		hist(L,gb_trees:empty(), fun(Dict,Pos) ->
			case gb_trees:lookup(Pos,Dict) of
				{value,Val} ->
					gb_trees:update(Pos, Val+1,Dict);
				none ->
					gb_trees:insert(Pos,1,Dict)
			end
		end)
	).

map(L) ->
	maps:to_list(
		hist(L,#{}, fun(Dict,Pos) ->
			case maps:get(Pos,Dict,undef) of
				undef ->
					Dict#{Pos=>1};
					%maps:put(Pos,1,Dict);
				Val ->
					maps:update(Pos,Val+1,Dict)
					%Dict#{Pos=>Val+1}
			end
		end)
	).

arr(L) ->
	[ _|List ] = array:to_list(
		hist(L,array:new({default, 0}), fun( Arr,Pos) -> array:set(Pos, array:get(Pos,Arr)+1,Arr) end)
	),
	List.


tuple(L) ->
	TL = list_to_tuple([0 || _ <- lists:seq(1, ?BINS)]),
	tuple_to_list(
		hist(L,TL, fun( Tup,Pos) -> setelement(Pos, Tup, element(Pos,Tup)) end)
	).
