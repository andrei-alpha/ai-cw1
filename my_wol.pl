:- [war_of_life].
:- use_module(library(system)).
test_strategy(0, _, _) :- !.

test_strategy(N, Str1, Str2) :-
	now(Start),
	test_strategy(N, Str1, Str2, Moves, Winners),
	now(End),
	count(Winners, 'draw', DrawRes),
	write('Number of draws is:'),
	write(DrawRes),
	nl,
	count(Winners, 'b', BRes),
	write('Number of wins for player 1 is:'),
	write(BRes),
	nl,
	count(Winners, 'r', RRes),
	write('Number of wins for player 2 is:'),
	write(RRes),
	nl,
	longest(Moves, LongestRes),
	write('Longest move in a game is:'),
	write(LongestRes),
	nl,
	shortest(Moves, ShortestRes),
	write('Shortest move in a game is:'),
	write(ShortestRes),
	nl,
	average_moves(Moves, AverageRes),
	write('Average move in a game is:'),
	write(AverageRes),
	nl,
	AvgTime is (End - Start)/N,
	write('Average time taken to play a game is:'),
	write(AvgTime).

test_strategy(0, _, _, [], []).
test_strategy(N, Str1, Str2, [NumMoves|Moves], [WinningPlayer|Winners]) :-
	N > 0,
	play(verbose, Str1, Str2, NumMoves, WinningPlayer),
	NewN is N-1,
	test_strategy(NewN, Str1, Str2, Moves, Winners).

count([], _, 0).
count([Winner|Winners], Winner, NewRes) :-
	!,
	count(Winners, Winner, Res),
	NewRes is Res+1.
count([_|Winners], Winner, Res) :-
	count(Winners, Winner, Res).

longest([Moves], Moves).
longest([250|T], Res) :-
	longest(T, Res).
longest([H1, H2|T], Res) :-
	(H1 =< H2
	-> longest([H2|T], Res)
	;  longest([H1|T], Res)
	).

shortest([Moves], Moves).
shortest([H1, H2|T], Res) :-
	(H1 =< H2
	-> shortest([H1|T], Res)
	;  shortest([H2|T], Res)
	).

sum_count_moves([], 0, 0).
sum_count_moves([H|T], UpdateSum, UpdateCount) :-
	sum_count_moves(T, Sum, Count),
	UpdateSum is Sum+H,
	UpdateCount is Count+1.

average_moves(Moves, Res) :-
	sum_count_moves(Moves, Sum, Count),
	Res is Sum / Count.
