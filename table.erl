%
% @doc A table is a list of lists as returned by csv:read().
%
% This module provides functions to work with a list
%
-module(table).

-include_lib("eunit/include/eunit.hrl").


-export( [ 	get_col/2, get_col_if/3, print/1, print/2, add_row/2, get_row/2, subtable/2
		 ]).



-type table() :: list( list( string() | number() ) ).

-export_type( [table/0] ).


%
% return column N as a list.
%
-spec get_col( pos_integer(), table() ) -> list( string() | number() ).
get_col(N,Tbl) ->
	lists:reverse( lists:foldl( fun(Line,Acc) -> [lists:nth(N,Line)|Acc] end, [], Tbl) ).

%
% return column N as a list, but only for lines where Pred is true.
%
% fun Pred gets the actual column as a list.
%
-spec get_col_if( fun((list()) -> boolean()), pos_integer(), table() ) -> list( string() | number() ).
get_col_if( Pred, N, Tbl ) ->
	lists:reverse(
		lists:foldl( fun(Line,Acc) ->
						case Pred(Line) of
							true ->
								[lists:nth(N,Line)|Acc];
							false ->
								Acc
						end
					end,[], Tbl
				)
	).


% @doc return the nth row
%
-spec get_row(pos_integer(),table()) -> list().
get_row( N, T ) ->
	lists:nth(N,T).

% @doc add a row at at the beginning
%
% @param T is a table.
% @param L is a list
%
-spec add_row( table(), list() ) -> table().
add_row(T,L) ->
	[L|T].


% print a table
%
-spec print(table()) -> ok.
print(T) ->
	print_rows(-1,T).


% print the first N columns of a table
print(N,T) ->
	print_rows(N,T).


% @doc return a table which contains all rows from the original table where
% fun() returns true.
%
-spec subtable( fun((list()) -> boolean()), table() ) -> table().
subtable(F,T) ->
	lists:reverse(
		lists:foldl( fun(Row,Acc) ->
			case F(Row) of
				true ->
					[Row|Acc];
				false ->
					Acc
			end
		end,[], T)
	).


%
% ------- private ------
%

print_rows(N,_) when N == 0 ->
	ok;

print_rows(_,[]) ->
	ok;

print_rows(N,[H|T]) ->
	io:format("~p~n",[H]),
	print_rows(N-1,T).

% ------- unit tests ------

basic_test() ->
	T = [["A",1,"a"],
	     ["B",2,"b"],
	     ["C",3,"c"],
	     ["D",4,"d"]],

	?debugFmt("~p~n",[T]),

	C = get_col(2,T),
	?assertEqual(length(C),4),
	?assert( [1,2,3,4] == C ),

	T1 = add_row(T,["E",5,"e"]),
	?debugFmt("~p~n",[T1]),

	C1 = get_col(2,T1),
	?debugFmt("C1 ~p~n",[C1]),
	?assertEqual(length(C1),5),
	?assert( [5,1,2,3,4] == C1 ),

	R=get_row(2,T),
	?assertEqual(["B",2,"b"],R).

advanced_test() ->
	T = [["A",1,"a"],
	     ["B",2,"b"],
	     ["C",3,"c"],
	     ["D",4,"d"]],

	Sub1 = subtable( fun(R) -> lists:nth(2,R) rem 2 == 1 end, T),
	?debugFmt("~p~n",[Sub1]),
	?assertEqual(length(Sub1),2).
