:- use_module(library(plunit)).

% Sample game state for testing
% A 6x6 board with some sample positions
initial_board([
    [(1, player1), (0, none), (0, none), (0, none), (0, none), (0, none)],
    [(0, none), (1, player2), (0, none), (0, none), (0, none), (0, none)],
    [(0, none), (0, none), (1, player1), (0, none), (0, none), (0, none)],
    [(0, none), (0, none), (0, none), (1, player2), (0, none), (0, none)],
    [(0, none), (0, none), (0, none), (0, none), (1, player1), (0, none)],
    [(0, none), (0, none), (0, none), (0, none), (0, none), (1, player2)]
]).

% Sample game state for player1's turn
initial_state(GameConfig, GameState),

% Test 1: Valid move for player1
test(valid_move) :-
    Move = ((1, 1), (1, 2)), % Move player1's piece from (1,1) to (1,2)
    move(GameState, Move, NewGameState),
    NewGameState = game_state(NewBoard, player2, [], [], human_vs_computer, easy),
    nth1(1, NewBoard, FirstRow),
    nth1(2, FirstRow, (1, player1)). % Check the new position has player1's piece

% Test 2: Invalid move (moving out of bounds)
test(invalid_move_out_of_bounds, [fail]) :-
    game_state_1(GameState),
    Move = ((1, 1), (0, 1)), % Invalid: Moving out of bounds
    move(GameState, Move, _).

% Test 3: Invalid move (not player's piece)
test(invalid_move_wrong_piece, [fail]) :-
    game_state_1(GameState),
    Move = ((2, 2), (2, 3)), % Invalid: Player1 trying to move player2's piece
    move(GameState, Move, _).

% Test 4: Invalid move (destination occupied by the same player)
test(invalid_move_occupied_same_player, [fail]) :-
    game_state_1(GameState),
    Move = ((1, 1), (3, 3)), % Invalid: Destination (3,3) occupied by player1
    move(GameState, Move, _).

% Test 5: Valid move for player2
test(valid_move_player2) :-
    initial_board(Board),
    GameState = game_state(Board, player2, [], [], human_vs_human, easy),
    Move = ((2, 2), (2, 3)), % Move player2's piece from (2,2) to (2,3)
    move(GameState, Move, NewGameState),
    NewGameState = game_state(NewBoard, player1, [], [], human_vs_human, easy),
    nth1(2, NewBoard, SecondRow),
    nth1(3, SecondRow, (1, player2)). % Check the new position has player2's piece

:- end_tests(move).
