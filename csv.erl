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
%	{seperator,Sep}, defaults to {seperator,","}
%	{skip,N}, skip N lines at the top, defaults to 0
%	numconv , attempt to convert all numbers to either integer or float, defaults to no conversion
%	
-spec read( string(), list(atom() | tuple()) ) -> list( list(number()|string()) ).
read(Csv_filename,Options) ->
	Seperator = proplists:get_value(seperator,Options,","),
	not io_lib:char_list(Seperator) andalso error("bad arg: separator must be a string"),
	
	Skip = proplists:get_value(skip,Options,0),
	not is_integer(Skip) or (Skip < 0) andalso error("bad arg: skip must be a positive integer"),
	
	Numconv = proplists:get_value(numconv,Options),
	{ok,Csv} = file:open(Csv_filename,[read]),
	lists:reverse(parse(Csv,[],Seperator,Skip,Numconv)).
			

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
parse(Csv,Acc,Sep,0,undefined) ->
	case file:read_line(Csv) of
		{ok,Row} ->
			Cols = cleanup(string:lexemes(Row,[Sep]),[]),
			New_acc= [ Cols | Acc ],
			parse(Csv,New_acc,Sep,0,undefined);
		eof ->
			Acc;
		{error,_} ->
			Acc
	end;

parse(Csv,Acc,Sep,0,true) ->
	case file:read_line(Csv) of
		{ok,Row} ->
			Tokens = string:lexemes(Row,Sep),
			Cols = lists:foldl( fun(E,CAcc) -> [convert_str(E)|CAcc] end, [],Tokens),
			New_acc= [ lists:reverse(Cols) | Acc ],
			parse(Csv,New_acc,Sep,0,true);
		eof ->
			Acc;
		{error,_} ->
			Acc
	end;


parse(Csv,Acc,Sep,N,Conf) ->
	file:read_line(Csv),
	parse(Csv,Acc,Sep,N-1,Conf).


convert_str(S) ->
	S2 = string:trim(S,both,[$\r,$\n,$\",$\ ,$\t]),
	try
		list_to_float(S2)
	catch
		error:badarg ->
			try
				list_to_integer(S2)
			catch
				_:_ -> S2
			end
	end.

cleanup( [],Newcols) ->
	lists:reverse(Newcols);

cleanup([H|T],Newcols) ->
	New = [ string:trim(H,both,[$\r,$\n,$\",$\ ,$\t]) | Newcols],
	cleanup(T,New).


write_item(F,I) when is_list(I) ->
	io:format(F,"~s~n",[I]);

write_item(F,I) ->
	io:format(F,"~p~n",[I]).

% -------- test ---------
convert_test() ->
	?assertEqual(23,convert_str("23")),
	?assertEqual(23.0,convert_str("23.0")),
	?assertEqual(23.0,convert_str("23.0\n")),
	?assertEqual("Not23",convert_str("Not23")),
	?assertEqual("23Not",convert_str("23Not")).
