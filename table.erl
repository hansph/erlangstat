-module(table).

-export( [ 	get_col/2, get_col_if/3, print/1, append_row/2
			%from_csv/1,	hd/1, select_row/2, select_col/2
		 ]).



-opaque table() :: list( list( string() | number() ) ).

-export_type( [table/0] ).


%
% return column N as a list.
%
-spec get_col( pos_integer() | string(), table() ) -> list( string() | number() ).
get_col(N,Tbl) when is_number(N) ->
	lists:reverse( lists:foldl( fun(Line,Acc) -> [lists:nth(N,Line)|Acc] end, [], Tbl) ).

%
% return column N as a list, but only for lines where Pred is true.
%
% fun Pred gets the actual column as a list.
%
-spec get_col_if( fun((pos_integer()) -> boolean()), pos_integer() | string(), table() ) -> list( string() | number() ).
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


append_row(T,L) ->
	%T ++ [L].
	lists:reverse( [[L]|T] ).
	%lists:reverse( [L] ++ T ).

print(T) ->
	print_rows(T).


% ------- private ------

print_rows([]) ->
	ok;

print_rows([H|T]) ->
	io:format("~p~n",[H]),
	print_rows(T).
