-module(example1).

-export( [do/0] ).

do() ->
	% read a csv table
	% example taken from https://www.opengov-muenchen.de/dataset/monatszahlen-witterung/resource/64c8c183-7fd0-4b29-9958-4169d22ee883

	% import the table, skip the header and convert numbers from string
	Csv = csv:read("210305monatszahlenwitterung.csv",[numconv,{skip,1}]),

	% we are interested in the hours of sunshine, "Sonnenscheindauer"
	% but only up to year 2019, th rest is incomplete.
	% remove also lines containing the sum of the year ("Summe")

	Filter = fun(Row) ->
				lists:nth(2,Row) == "Sonnenscheindauer" andalso
				lists:nth(4,Row) /= "Summe" andalso
				lists:nth(3,Row) < 2020
			end,
	Sunnyhours = table:subtable(Filter,Csv),

	% to be sure we did everything correct, print the first 20 lines
	table:print(20,Sunnyhours),

	% now do some science
	% for each year, calculate min,max,avg,variance
	% extract column 5 and print statistics
	[print_stats(Y,Sunnyhours) || Y <- lists:seq(2000,2019)],
	ok.


print_stats(Year,Table) ->
	io:format("~p sunny hours: ",[Year]),
	stat:print( stat:minmax( table:get_col_if( fun(R) -> lists:nth(3,R) == Year end, 5, Table) )).
