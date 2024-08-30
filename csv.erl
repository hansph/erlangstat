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
	true = lists:member(hd(Seperator),[$,,$:,$;]),
	
	Skip = proplists:get_value(skip,Options,0),
	not is_integer(Skip) or (Skip < 0) andalso error("bad arg: skip must be a positive integer"),
	
	Numconv = proplists:get_value(numconv,Options),
	{ok,Csv} = file:open(Csv_filename,[read]),
	case skip_lines(Csv,Skip) of
		ok -> lists:reverse(parse(Csv,[],hd(Seperator),Numconv));
		eof -> []
	end.
			

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

%
% parse( filehandle, accumulator, separator, numconf )

-spec parse( file:io_device(), list(), string(), undefined|true ) -> list().
parse(Csv,Acc,Sep,undefined) ->
	case file:read_line(Csv) of
		{ok,Row} ->
			%Cols = cleanup(string:lexemes(Row,[Sep]),[]),
			New_acc= [ parse_line(Row,Sep) | Acc ],
			parse(Csv,New_acc,Sep,undefined);
		eof ->
			Acc;
		{error,_} ->
			Acc
	end;

parse(Csv,Acc,Sep,true) ->
	case file:read_line(Csv) of
		{ok,Row} ->
			%Tokens = string:lexemes(Row,Sep),
			Tokens = parse_line(Row,Sep),
			Cols = lists:foldl( fun(E,CAcc) -> [convert_str(E)|CAcc] end, [],Tokens),
			New_acc= [ lists:reverse(Cols) | Acc ],
			parse(Csv,New_acc,Sep,true);
		eof ->
			Acc;
		{error,_} ->
			Acc
	end.

% skip N lines
skip_lines(_Csv,0) ->
	ok;

skip_lines(Csv,N) ->
	case file:read_line(Csv) of
		{ok,_} -> skip_lines(Csv,N-1);
		_ -> eof
	end.

% parse one string into tokens
% first arg is the string, second arg is the separator
% supported separators are "," ";" ":" as char()
-spec parse_line(string(),char()) -> list(string()).
parse_line([],_) ->
	[];

parse_line(Line,Sep) ->
	lists:reverse(tokenize(Line,Sep,[])).


tokenize([],_,Acc) ->
	Acc;

tokenize(Line,Sep,Acc) ->
	[S|Rest] = Line,
	%?debugFmt("tokenize: hd:~p sep:~p rest:~s Acc: ~p",[S,Sep,Rest,Acc]),
	case S of
		Sep ->
			tokenize( Rest, Sep , [ []|Acc ] );
		$\" -> % quoted text, there must be a closing \"
			case string:split( Rest, "\"" ) of
				[[],R] -> % at the beginning, so an empty field
					tokenize( R, Sep, [ [] | Acc ] );
				[Field,R2] -> % next token should be a separator, eat it
					case R2 of
						[] -> % end of string
							[trim(Field)|Acc];
						_ -> 
							[_|T] = R2,
							tokenize( T, Sep, [trim(Field)|Acc] )
					end;
				_ ->
					error("parse error: non-closed quote")
			end;
		$\  ->
			tokenize( Rest, Sep , Acc );
		_ ->
			case string:split( Line, [Sep] ) of
				[[],R] -> % sep at the beginning, so an empty field
					tokenize( R, Sep, [ [] | Acc ] );
				[Field,R2] ->
					tokenize( R2, Sep, [trim(Field)|Acc] );
				_ ->
					[trim(Line)|Acc]
			end
	end.

trim(Field) ->
	string:trim(Field,both,[$\r,$\n,$\",$\ ,$\t]).

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


write_item(F,I) when is_list(I) ->
	io:format(F,"~s~n",[I]);

write_item(F,I) ->
	io:format(F,"~p~n",[I]).

% -------- test ---------

parse_line_test() ->
	Sep=$,,
	?assertEqual(["A","B","C"],parse_line("A,B,C",Sep)),
	?assertEqual(["A","B","C"],parse_line("A, B ,C",Sep)),
	?assertEqual(["A","","C"],parse_line("A,,C",Sep)),
	?assertEqual(["A","B b","C"],parse_line("A,B b,C",Sep)),
	?assertEqual(["A","B,b","C"],parse_line("A,\"B,b\",C",Sep)),
	?assertEqual(["A","B","C"],parse_line("A,\" B\",C",Sep)),
	?assertEqual(["A","B","C"],parse_line("A,\"B \",C",Sep)),

	% other separators, only a few tests
	?assertEqual(["A,B,C"],parse_line("A,B,C",$;)),
	?assertEqual(["A","B","C"],parse_line("A;B;C",$;)),
	%?assertEqual(["A,B,C"],parse_line("A,B,C",$;)),
	ok.


convert_test() ->
	?assertEqual([],convert_str("")),
	?assertEqual(23,convert_str("23")),
	?assertEqual(23.0,convert_str("23.0")),
	?assertEqual(23.0,convert_str("23.0\n")),
	?assertEqual("Not23",convert_str("Not23")),
	?assertEqual("23Not",convert_str("23Not")).
