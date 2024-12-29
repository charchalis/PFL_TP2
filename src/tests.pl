% Sample game state setup for testing
test_game_state(game_state([
    [(3, player1), (0, empty), (0, empty), (0, empty)],
    [(0, empty), (0, empty), (0, empty), (0, empty)],
    [(0, empty), (0, empty), (0, empty), (0, empty)],
    [(0, empty), (0, empty), (0, empty), (1, player2)]
], player1, [], [], human_vs_computer, easy)).

% Example move for player1 to move from (1, 1) to (2, 1)
test_move(Move) :-
    Move = move(1, 1, 1, 2).  % Move from (1, 1) to (2, 1)


test_move_execution :-
    write('popo'),nl,
    test_game_state(GameState),
    write('popo'),nl,
    test_move(Move),
    write('popo'),nl,
    move(GameState, Move, NewGameState),
    write('popo never'),nl,
    write('Old Game State: '), write(GameState), nl,
    %display_game(GameState),
    write('New Game State: '), write(NewGameState), nl,
    display_game(NewGameState).

% Example of an invalid move (moving from an empty space)
test_invalid_move :-
    test_game_state(GameState),
    Move = move(2, 2, 3, 3),  % Trying to move from an empty space
    (move(GameState, Move, _) -> write('Move succeeded!'); write('Move failed, invalid move!')), nl.

test:-
    test_move_execution.