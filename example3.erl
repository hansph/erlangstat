-module(example3).

-export( [do/0] ).

do() ->
	% read a csv with data
	% data set from https://www.scribbr.com/statistics/simple-linear-regression/
	% expect the data in income.data.csv in the same directory as this example

	% import the table, skip the header and convert numbers from string
	Csv = csv:read("income.data.csv",[numconv,{skip,1}]),

	% extract cols 2 and 3
	Income = table:get_col(2,Csv),
	Happiness = table:get_col(3,Csv),
	% calculate the linear regression coefficents
	{A,B} = stat:simple_lreg( Income,Happiness ),

	io:format("A= ~p , B= ~p~n",[A,B]),
	io:format("Covariance: ~p~n",[stat:covar(Income,Happiness)]),
	io:format("Correlation: ~p~n",[stat:correlation(Income,Happiness)]).
