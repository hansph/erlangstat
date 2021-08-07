-module(csv).

-include_lib("eunit/include/eunit.hrl").

-export( [ read/1, read/2, write_list/2, write_list/3 ] ).

-spec read( string() ) -> list( list(string()) ).
read(Csv_filename) ->
	read(Csv_filename,[]).

%
% @doc read a CSV file into a list of lists.
%
% Options:
%	{skip,N}, skip N lines at the top, defaults to 0
%	numconv , attempt to convert all numbers to either integer or float, defaults to no conversion
%	
-spec read( string(), list(atom() | tuple()) ) -> list( list(number()|string()) ).
read(Csv_filename,Options) ->
	Skip = proplists:get_value(skip,Options,0),
	Numconv = proplists:get_value(numconv,Options),
	{_Header,Table} = lists:split(Skip,parse(Csv_filename)),
	numconf_table(Table,Numconv).

%
% @doc write a list into a file, one item per line.
%
write_list( Fname, L ) ->
	write_list( Fname, L, []).

%
% @doc write a list into a file, one item per line.
%
% Options
%	{index,N} - write a first column containing a running number starting with N,
%				defaults to do not write an index
%	{seperator,Sep} - if a index colum is written, use Sep as a seperator, defaults to ","
%

-spec write_list( string(), list(any()), list(atom() | tuple()) ) -> ok.
write_list( Fname, L, Options ) ->
	{ok,F} = file:open(Fname,[write]),
	Sep = proplists:get_value(seperator,Options,","),
	Index = proplists:get_value(index,Options,0),
	case Index of
		0 ->
			lists:foreach( fun(I) -> write_item(F,I) end, L);
		_ ->
			lists:foldl( fun(I,N) ->
							io:format(F,"~p~s ",[N,Sep]),
							write_item(F,I),
							N+1
						end, Index, L)
		end,
	file:close(F).
	

% -------- private ------------

numconf_table(Table,undefined) ->
	Table;

numconf_table(Table,true) ->
	lists:reverse(
		lists:foldl( fun(L,Acc) -> [numconv_line(L)|Acc] end, [], Table)
	).


numconv_line(L) ->
	lists:reverse(
		lists:foldl( fun(E,Acc) -> [convert_str(E)|Acc]	end, [], L)
	).

convert_str(S) ->
	%S2 = string:trim(S,both,[$\r,$\n,$\",$\ ,$\t]),
	try
		list_to_float(S)
	catch
		error:badarg ->
			try
				list_to_integer(S)
			catch
				_:_ -> S
			end
	end.

write_item(F,I) when is_list(I) ->
	io:format(F,"~s~n",[I]);

write_item(F,I) ->
	io:format(F,"~p~n",[I]).

%
% slightly changed version of https://stackoverflow.com/a/1532947
%
% handles cases where tokens (columns) are quoted or not, empty columns and
% commas embedded in quoted strings. Does not handle escaped quotes.
% leading and trailing spaces are removed (also when quoted)
%

parse(File) ->
  {ok, F} = file:open(File, [read, raw]),
  parse(F, file:read_line(F), []).

parse(F, eof, Done) ->
  file:close(F),
  lists:reverse(Done);    

parse(F, {ok,Line}, Done) ->
  parse(F, file:read_line(F), [parse_line(Line)|Done]).



parse_line(Line) ->
%?debugFmt("parse_line1: ~p",[Line]),
	Tokens = parse_line(Line, []),
	lists:reverse(
		lists:foldl( fun(T,Acc) -> [ string:trim(T) | Acc ] end,[],Tokens )
	).

parse_line([], Fields) ->
	lists:foldl( fun(T,Acc) -> [ string:trim(T) | Acc ] end,[],Fields );

parse_line("," ++ Line, Fields) -> parse_field(Line, Fields);
parse_line(Line, Fields) -> parse_field(Line, Fields).


parse_field("\"" ++ Line, Fields) -> parse_field_q(Line, [], Fields);
parse_field(Line, Fields) -> parse_field(Line, [], Fields).

parse_field("," ++ _ = Line, Buf, Fields) -> parse_line(Line, [lists:reverse(Buf)|Fields]);
parse_field([C|Line], Buf, Fields) -> parse_field(Line, [C|Buf], Fields);
parse_field([], Buf, Fields) -> parse_line([], [lists:reverse(Buf)|Fields]).

parse_field_q("\"\"" ++ Line, Buf, Fields) -> parse_field_q(Line, [$"|Buf], Fields);
parse_field_q("\"" ++ Line, Buf, Fields) -> parse_line(Line, [lists:reverse(Buf)|Fields]);
parse_field_q([C|Line], Buf, Fields) -> parse_field_q(Line, [C|Buf], Fields).

%
% -------- unit test ---------
%

parse_line_test() ->
	?assertEqual(["A","B","C"],parse_line("A,B,C",[])),
	?assertEqual(["A","B","C"],parse_line("A, B ,C",[])),
	?assertEqual(["A","","C"],parse_line("A,,C",[])),
	?assertEqual(["A","B b","C"],parse_line("A,B b,C",[])),
	?assertEqual(["A","B,b","C"],parse_line("A,\"B,b\",C",[])),
	?assertEqual(["A","B","C"],parse_line("A,\" B\",C",[])),
	?assertEqual(["A","B","C"],parse_line("A,\"B \",C",[])),

	ok.

convert_test() ->
	?assertEqual(23,convert_str("23")),
	?assertEqual(23.0,convert_str("23.0")),
	?assertEqual("Not23",convert_str("Not23")),
	?assertEqual("23Not",convert_str("23Not")).

numconv_line_test() ->
	?assertEqual([],numconv_line([])),
	?assertEqual([1,2,3],numconv_line(["1","2","3"])),
	?assertEqual(["A","b",3],numconv_line(["A","b","3"])).
