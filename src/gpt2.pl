% Main predicate: Entry point for the game
play :-
    write('Welcome to Anaash!'), nl,
    configure_game(game_config(human_vs_human, 1), GameConfig),
    initial_state(GameConfig, GameState),
    write('yo'),nl,nl,
    game_cycle(GameState, GameConfig).

reload :-
    [gpt2].  % reload when changes are made



% Configure the game
configure_game(CurrentConfig, FinalConfig) :-
    display_menu(CurrentConfig, UpdatedConfig),
    (   UpdatedConfig = start_game(FinalConfig)
    ->  true
    ;   configure_game(UpdatedConfig, FinalConfig)
    ).

% Display the menu
display_menu(game_config(GameType, Difficulty), UpdatedConfig) :-
    write('Game Menu:'), nl,
    write('1. Configure Game Type (H/H, H/PC, PC/H, PC/PC)'), nl,
    write('2. Configure Difficulty (1: Easy, 2: Medium, 3: Hard)'), nl,
    write('3. Start Game'), nl,
    write('Enter your choice (1-3): '),
    read(Input),
    parse_choice(Input, Choice),
    handle_menu_choice(Choice, game_config(GameType, Difficulty), UpdatedConfig).

% Handle menu choices
handle_menu_choice(1, game_config(_, Difficulty), game_config(NewGameType, Difficulty)) :-
    configure_game_type(NewGameType).
handle_menu_choice(2, game_config(GameType, _), game_config(GameType, NewDifficulty)) :-
    configure_difficulty(NewDifficulty).
handle_menu_choice(3, Config, start_game(Config)) :-
    write('Starting the game...'), nl.
handle_menu_choice(_, Config, Config) :-
    write('Invalid choice. Please try again.'), nl.

% Parse the input into a valid choice
parse_choice(Input, Choice) :-
    (   integer(Input)
    ->  Choice = Input
    ;   atom_number(Input, Choice) % Convert atom to number if needed
    ).

% Configure game type
configure_game_type(GameType) :-
    write('Select Game Type:'), nl,
    write('1. Human vs Human (H/H)'), nl,
    write('2. Human vs Computer (H/PC)'), nl,
    write('3. Computer vs Human (PC/H)'), nl,
    write('4. Computer vs Computer (PC/PC)'), nl,
    write('Enter your choice (1-4): '),
    read(Choice),
    parse_game_type(Choice, GameType).

parse_game_type(1, human_vs_human).
parse_game_type(2, human_vs_computer).
parse_game_type(3, computer_vs_human).
parse_game_type(4, computer_vs_computer).

% Configure difficulty
configure_difficulty(Difficulty) :-
    write('Select Difficulty:'), nl,
    write('1. Easy (Random)'), nl,
    write('2. Medium (Greedy)'), nl,
    write('3. Hard (Advanced AI)'), nl,
    write('Enter your choice (1-3): '),
    read(Difficulty).

% Game cycle
game_cycle(GameState, GameConfig) :-
    write('game cycle'),nl,
    display_game(GameState),
    write('game cycle'),nl,
    (   game_over(GameState, Winner)
    ->  format('Game Over! Winner: ~w', [Winner]), nl
    ;   play_turn(GameState, NewGameState, GameConfig),
        game_cycle(NewGameState, GameConfig)
    ).


% Play a turn
play_turn(game_state(Board, current_player(Player), Players, _), NewGameState, game_config(GameType, Difficulty)) :-
    (   Player = human
    ->  human_move(Board, Move),
        move(game_state(Board, current_player(Player), Players, Difficulty), Move, NewGameState)
    ;   computer_move(game_state(Board, current_player(Player), Players, Difficulty), GameType, Difficulty, Move),
        move(game_state(Board, current_player(Player), Players, Difficulty), Move, NewGameState)
    ).

% Human move
human_move(Board, Move) :-
    write('Enter your move (X, Y): '),
    read(Move),
    valid_move(Board, Move).

% Computer move
computer_move(GameState, GameType, Difficulty, Move) :-
    choose_move(GameState, Difficulty, Move),
    format('Computer chooses move: ~w~n', [Move]).


% Initialize game state
initial_state(game_config(GameType, Difficulty), game_state(Board, CurrentPlayer, Players, Difficulty)) :-
    create_board(3, 3, Board), % Default board size
    setup_players(GameType, Players, CurrentPlayer).

% Set up players based on game type
setup_players(human_vs_human, [human, human], human).
setup_players(human_vs_computer, [human, computer], human).
setup_players(computer_vs_human, [computer, human], computer).
setup_players(computer_vs_computer, [computer, computer], computer).


% Create a Rows x Cols board filled with 'empty'
create_board(Rows, Cols, Board) :-
    create_rows(Rows, Cols, Board).

% Create Rows number of rows, each with Cols columns filled with 'empty'
create_rows(0, _, []). % Base case: no rows to create
create_rows(Rows, Cols, [Row | Rest]) :-
    Rows > 0,
    create_row(Cols, Row),    % Create a single row
    NewRows is Rows - 1,      % Decrement the row count
    create_rows(NewRows, Cols, Rest).

% Create a single row with Cols number of 'empty' cells
create_row(0, []). % Base case: no columns to create
create_row(Cols, [empty | Rest]) :-
    Cols > 0,
    NewCols is Cols - 1,      % Decrement the column count
    create_row(NewCols, Rest).


% Display the game state
display_game(game_state(Board, current_player(Player), Players, Difficulty)) :-
    nl,
    write('Game Board:'), nl,
    display_board(Board),
    nl,
    write('Current Player: '), write(Player), nl,
    write('Players: '), write(Players), nl,
    write('Difficulty Level: '), write(Difficulty), nl,
    nl.

% Display the game board
display_board(Board) :-
    length(Board, Rows),
    write('   '),
    display_column_indices(Board), nl,
    display_rows(Board, Rows).

% Display column indices
display_column_indices([Row | _]) :-
    length(Row, Cols),
    findall(Index, between(1, Cols, Index), Indices),
    maplist(write_index, Indices).

% Write a single index (formatting for spacing)
write_index(Index) :-
    format(' ~w ', [Index]).

% Display rows recursively
display_rows([], _).
display_rows([Row | Rest], RowNum) :-
    write(RowNum), write(' |'), % Print row number and separator
    display_row(Row), nl,
    NextRowNum is RowNum - 1,
    display_rows(Rest, NextRowNum).

% Display a single row
display_row([]).
display_row([Cell | Rest]) :-
    write_cell(Cell),
    display_row(Rest).

% Write a single cell (empty or piece)
write_cell(Cell) :-
    (   Cell = empty
    ->  write(' . ') % Empty cell represented by '.'
    ;   format(' ~w ', [Cell]) % Print piece directly
    ).