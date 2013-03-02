:- [war_of_life].
:- use_module(library(system)).

% Choose between 'verbose' or 'quite' 
verbose_level(verbose).

test_strategy(0, _, _) :- !.

% Test strategy
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
    verbose_level(level),
	play(level, Str1, Str2, NumMoves, WinningPlayer),
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

% Game strategies 
bloodlust(PlayerColour, CurrentBoardState, NewBoardState, Move).
    % TO DO:

self_preservation(PlayerColour, CurrentBoardState, NewBoardState, Move).
    % TO DO:

land_grab(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
    best_move(PlayerColor, CurrentBoardState, land_grab, NewBoardState, Move, _).

minimax(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
    best_move(PlayerColor, CurrentBoardState, minmax, NewBoardState, Move, _).

% Get oponent
oponent('b', 'r').
oponent('r', 'b').

% Decompose the board
decompose_board('b', [B,R], B, R).
decompose_board('r', [B,R], R, B).    

% Compose the board
compose_board('b', B, R, [B,R]).
compose_board('r', R, B, [B,R]).

% Score for bloodlust: the number of oponent's pieces on the board. 
get_score(PlayerColor, BoardState, bloodlust, Score).
    % TO DO:

% Score for self_preservation: the number of player's pieces on the board. 
get_score(PlayerColor, BoardState, self_preservation, Score).
    % TO DO:

% Score for land_grab: the number of player's pieces - the number of oponent's pieces.
get_score(PlayerColor, BoardState, land_grab, Score) :-
    decompose_board(PlayerColor, BoardState, PlayerPieces, OponentPieces),
    Score is lenght(PlayerPieces) - length(OponentPieces).

% Score for min-max: we look one more move ahead and use land_grab strategy 
get_score(PlayerColor, BoardState, minmax, Score) :-
    oponent(PlayerColor, OponentColor),
    best_move(OponentColor, BoardState, land_grab, _, _, OponentScore),
    Score is -OponentScore.

% Get the best move for the current strategy
best_move(PlayerColor, BoardState, Strategy, NewBoardState, Move, Score) :-
    decompose_board(PlayerColor, BoardState, PlayerPieces, OponentPieces),
    random_move(PlayerPieces, OponentPieces, Moves),
    make_moves(PlayerColor, BoardState, Strategy, Moves, Move, Score),
    alter_board(Move, PlayerPieces, NewPlayerPieces),
    compose_board(PlayerColor, NewPlayerPieces, OponentPieces, NewBoardState).
    
% Makes all the moves and returns the best one for the current strategy
make_moves(_, _, _, _, [], '?', 'undefined'). 
make_moves(PlayerColor, BoardState, Strategy, [Move|Moves], BestMove, BestScore) :-
    decompose_board(PlayerColor, BoardState, PlayerPieces, OponentPieces),
    alter_board(Move, PlayerPieces, NewPlayerPieces),
    compose_board(PlayerColor, NewPlayerPieces, OponentPieces, NewBoardState),
    next_generation(NewBoardState, NewGenBoardState),
    get_score(PlayerColor, NewGenBoardState, Strategy, Score),
    make_moves(PlayerColor, BoardState, Strategy, Moves, RemMove, RemScore),
    ( (RemScore == 'undefined'; RemScore < Score) ->
        (BestMove = Move, BestScore = Score);
        (BestMove = RemMove, BestScore = RemScore)
    ).
    
