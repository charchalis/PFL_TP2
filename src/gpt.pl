% Main predicate: Entry point for the game
play :-
    write('Welcome to Anaash!'), nl,
    display_menu,
    configure_game(GameConfig),
    initial_state(GameConfig, GameState),
    game_cycle(GameState).

% Display menu options
display_menu :-
    write('1. Human vs Human (H/H)'), nl,
    write('2. Human vs Computer (H/PC)'), nl,
    write('3. Computer vs Human (PC/H)'), nl,
    write('4. Computer vs Computer (PC/PC)'), nl.

% Configure the game based on user input
configure_game(GameConfig) :-
    write('Choose an option (1-4): '), read(Option),
    configure_game_option(Option, GameConfig).

configure_game_option(1, game_config(human, human, 1, 1)).
configure_game_option(2, game_config(human, computer, Level1, Level2)) :-
    choose_difficulty(Level2), Level1 = 1.
configure_game_option(3, game_config(computer, human, Level1, Level2)) :-
    choose_difficulty(Level1), Level2 = 1.
configure_game_option(4, game_config(computer, computer, Level1, Level2)) :-
    choose_difficulty(Level1), choose_difficulty(Level2).

choose_difficulty(Level) :-
    write('Choose difficulty (1: Random, 2: Greedy): '), read(Level).

% Game cycle
game_cycle(GameState) :-
    display_game(GameState),
    (   game_over(GameState, Winner)
    ->  format('Game Over! Winner: ~w', [Winner]), nl
    ;   play_turn(GameState, NewGameState),
        game_cycle(NewGameState)
    ).

% Initialize game state
initial_state(game_config(Player1, Player2, Level1, Level2), game_state(Board, current_player(Player1), [Player1, Player2], [Level1, Level2])) :-
    create_board(3, 3, Board). % 3x3 board for simplicity

% Create an empty board (N x M)
create_board(N, M, Board) :-
    length(Row, M), maplist(=(empty), Row),
    length(Board, N), maplist(=(Row), Board).

% Display the game state
display_game(game_state(Board, current_player(Player), _, _)) :-
    format('Player ~w\'s turn.', [Player]), nl,
    display_board(Board, 1).

% Display the board
display_board([], _).
display_board([Row|Rows], Y) :-
    reverse(Row, DisplayRow),
    write(DisplayRow), nl,
    NextY is Y + 1,
    display_board(Rows, NextY).

% Execute a move
move(game_state(Board, current_player(Player), Players, Levels), Move, game_state(NewBoard, current_player(NextPlayer), Players, Levels)) :-
    valid_moves(game_state(Board, current_player(Player), Players, Levels), ValidMoves),
    member(Move, ValidMoves),
    apply_move(Board, Move, Player, NewBoard),
    next_player(Player, Players, NextPlayer).

% Apply a move to the board
apply_move(Board, (X, Y), Player, NewBoard) :-
    nth1(Y, Board, Row),
    replace_in_list(X, Row, Player, NewRow),
    replace_in_list(Y, Board, NewRow, NewBoard).

% Replace element in a list
replace_in_list(Index, List, Element, NewList) :-
    nth1(Index, List, _, Rest),
    nth1(Index, NewList, Element, Rest).

% Determine the next player
next_player(Player, [Player1, Player2], NextPlayer) :-
    (Player = Player1 -> NextPlayer = Player2 ; NextPlayer = Player1).

% Find all valid moves
valid_moves(game_state(Board, _, _, _), ValidMoves) :-
    findall((X, Y), valid_move(Board, (X, Y)), ValidMoves).

% Check if a move is valid
valid_move(Board, (X, Y)) :-
    nth1(Y, Board, Row),
    nth1(X, Row, empty).

% Check if the game is over
game_over(game_state(Board, _, _, _), Winner) :-
    (   winning_combination(Board, Winner)
    ;   board_full(Board), Winner = draw).
    
% Check for a winning combination
winning_combination(Board, Player) :-
    row_win(Board, Player);
    col_win(Board, Player);
    diag_win(Board, Player).

% Check for row win
row_win(Board, Player) :- member(Row, Board), all_same(Row, Player).

% Check for column win
col_win(Board, Player) :- transpose(Board, Transposed), row_win(Transposed, Player).

% Check for diagonal win
diag_win(Board, Player) :-
    diag1(Board, Diag1), all_same(Diag1, Player);
    diag2(Board, Diag2), all_same(Diag2, Player).

% Check if all elements in a list are the same
all_same([H|T], H) :- maplist(=(H), T).

% Check if the board is full
board_full(Board) :- \+ (member(Row, Board), member(empty, Row)).

% Evaluate board for player
value(game_state(Board, _, _, _), Player, Value) :-
    (   winning_combination(Board, Player)
    ->  Value = 100
    ;   Value = 0).

% Choose move for AI
choose_move(GameState, Level, Move) :-
    (   Level = 1
    ->  valid_moves(GameState, ValidMoves), random_member(Move, ValidMoves)
    ;   Level = 2
    ->  valid_moves(GameState, ValidMoves),
        find_best_move(GameState, ValidMoves, Move)).

% Find the best move (greedy)
find_best_move(GameState, Moves, BestMove) :-
    maplist(evaluate_move(GameState), Moves, Scores),
    max_member(_-BestMove, Scores).

% Evaluate a move
evaluate_move(GameState, Move, Value-Move) :-
    move(GameState, Move, NewGameState),
    value(NewGameState, _, Value).
