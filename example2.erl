-module(example2).

-export( [roll/1] ).


%
% roll 2 dices, N times
% then show histogram and ecdf
%

roll(0) ->
	ok;

roll( N ) when N > 0 ->
	% roll 2 six-sided dices, N times, store the sum of the dices
	Vals = lists:foldl( fun(_,Acc) ->
			Val = rand:uniform(6) + rand:uniform(6),
			%io:format("~p~n",[Val]),
			[ Val | Acc ]
		end, [], lists:seq(1, N)
	),
	% create a histogram, possible values are between 2 and 12
	% so we use 10 bins: 2 to 3, 3 to 4, ... 11 to 12
	H = hist:create(Vals,10,2,12),

	% print the histogram using relative (normalized) values
	hist:print(hist:normalize(H)),
	% create the ecdf list, must use the histogram and not the original data
	% because it essentially only cumulates the histogram values
	% works also with normalized values. 
	hist:ecdf(hist:values(H)).
