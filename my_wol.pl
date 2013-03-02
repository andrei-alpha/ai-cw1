:- [war_of_life].
:- use_module(library(system)).

% Choose between 'verbose' or 'quiet' 
verbose_level(verbose).

test_strategy(0, _, _) :- !.

% Test strategy
test_strategy(N, Str1, Str2) :-
    format('Comparing ~w with ~w :~n~n',[Str1, Str2]),
	now(Start),
	test_strategy(N, Str1, Str2, Moves, Winners),
	now(End),
	count(Winners, 'draw', DrawRes),
	format('Number of draws is: ~w~n', [DrawRes]),
	count(Winners, 'b', BRes),
	format('Number of wins for player 1 is: ~w~n', [BRes]),
	count(Winners, 'r', RRes),
	format('Number of wins for player 2 is: ~w~n', [RRes]),
	longest(Moves, LongestRes),
	format('Longest move in a game is: ~w~n', [LongestRes]),
	shortest(Moves, ShortestRes),
	format('Shortest move in a game is: ~w~n', [ShortestRes]),
	average_moves(Moves, AverageRes),
	format('Average move in a game is: ~w~n', [AverageRes]),
	AvgTime is (End - Start)/N,
	format('Average time taken to play a game is: ~w~n',[AvgTime]).

test_strategy(0, _, _, [], []).
test_strategy(N, Str1, Str2, [NumMoves|Moves], [WinningPlayer|Winners]) :-
	N > 0,
    verbose_level(Level),
	play(Level, Str1, Str2, NumMoves, WinningPlayer),
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
bloodlust(PlayerColor, CurrentBoardState, NewBoardState, Move) :-
    best_move(PlayerColor, CurrentBoardState, bloodlust, NewBoardState, Move, _).

self_preservation(PlayerColor, CurrentBoardState, NewBoardState, Move) :-
    best_move(PlayerColor, CurrentBoardState, self_preservation, NewBoardState, Move, _).

land_grab(PlayerColor, CurrentBoardState, NewBoardState, Move) :-
    best_move(PlayerColor, CurrentBoardState, land_grab, NewBoardState, Move, _).

minimax(PlayerColor, CurrentBoardState, NewBoardState, Move) :-
    best_move(PlayerColor, CurrentBoardState, minimax, NewBoardState, Move, _).

% Get all moves
get_moves(Alive, OtherPlayerAlive, PossMoves) :-
    findall([A,B,MA,MB],(member([A,B], Alive),
                    neighbour_position(A,B,[MA,MB]),
                  \+member([MA,MB],Alive),
                  \+member([MA,MB],OtherPlayerAlive)),
     PossMoves).

% Get opponent
opponent('b', 'r').
opponent('r', 'b').

% Decompose the board
decompose_board('b', [B,R], B, R).
decompose_board('r', [B,R], R, B).    

% Compose the board
compose_board('b', B, R, [B,R]).
compose_board('r', R, B, [B,R]).

% Score for bloodlust: the number of opponent's pieces on the board. 
get_score(PlayerColor, BoardState, bloodlust, Score) :-
    decompose_board(PlayerColor, BoardState, _, OpponentPieces),
    length(OpponentPieces,NumPieces),
	Score is -NumPieces.

% Score for self_preservation: the number of player's pieces on the board. 
get_score(PlayerColor, BoardState, self_preservation, Score) :-
   	decompose_board(PlayerColor, BoardState, PlayerPieces, _),
    length(PlayerPieces, Score).

% Score for land_grab: the number of player's pieces - the number of opponent's pieces.
get_score(PlayerColor, BoardState, land_grab, Score) :-
    decompose_board(PlayerColor, BoardState, PlayerPieces, OpponentPieces),
    length(PlayerPieces, Len1),
    length(OpponentPieces, Len2),
    Score is Len1 - Len2.

% If we have no more pieces left
get_score(PlayerColor, BoardState, minimax, Score) :-
    decompose_board(PlayerColor, BoardState, PlayerPieces, []),
    !,
    length(PlayerPieces, Score).


% Score for min-max: we look one more move ahead and use land_grab strategy 
get_score(PlayerColor, BoardState, minimax, Score) :-
    opponent(PlayerColor, OpponentColor),
    best_move(OpponentColor, BoardState, land_grab, _, _, OpponentScore),
    Score is -OpponentScore.

% Get the best move for the current strategy
best_move(PlayerColor, BoardState, Strategy, NewBoardState, Move, Score) :-
    decompose_board(PlayerColor, BoardState, PlayerPieces, OpponentPieces),
    get_moves(PlayerPieces, OpponentPieces, Moves),
    make_moves(PlayerColor, BoardState, Strategy, Moves, Move, Score),
    alter_board(Move, PlayerPieces, NewPlayerPieces),
    compose_board(PlayerColor, NewPlayerPieces, OpponentPieces, NewBoardState).
    
% Makes all the moves and returns the best one for the current strategy
make_moves(_, _, _, [], '?', 'undefined'). 
make_moves(PlayerColor, BoardState, Strategy, [Move|Moves], BestMove, BestScore) :-
    decompose_board(PlayerColor, BoardState, PlayerPieces, OpponentPieces),
    alter_board(Move, PlayerPieces, NewPlayerPieces),
    compose_board(PlayerColor, NewPlayerPieces, OpponentPieces, NewBoardState),
    next_generation(NewBoardState, NewGenBoardState),
    get_score(PlayerColor, NewGenBoardState, Strategy, Score),
    make_moves(PlayerColor, BoardState, Strategy, Moves, RemMove, RemScore),
    ( (RemScore == 'undefined'; RemScore < Score) ->
        (BestMove = Move, BestScore = Score);
        (BestMove = RemMove, BestScore = RemScore)
    ).
    
