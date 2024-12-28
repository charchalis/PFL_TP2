% Anaash Game in SICStus Prolog

play :-
    welcome,
    DefaultConfig = game_config(human_vs_computer, easy, small),
    main_menu(DefaultConfig).

welcome :-
    nl,
    write('----------------------'), nl,
    write('| Welcome to Anaash! |'), nl,
    write('----------------------'), nl, nl.


reload :-
    [game].  % reload when changes are made





%%%%%%%%%%% menus %%%%%%%%%%%


main_menu(GameConfig) :-
    GameConfig = game_config(GameType, Difficulty, Size),
    write('1. start ( '), write(GameType), write(' | '), write(Difficulty), write(' | '), write(Size), write(' board )'), nl,
    write('2. game type'), nl,
    write('3. difficulty'), nl,
    write('4. board size'), nl,
    write('5. leave'), nl, nl,
    write('Choose an option (1-5): '),
    read(Option), nl,
    handle_main_menu(Option, GameConfig).

handle_main_menu(1, GameConfig):-
    write('Starting game...'), nl,
    initial_state(GameConfig, GameState),
    write(GameState),
    display_game(GameState).

handle_main_menu(2, GameConfig):-
    game_type_menu(GameConfig).

handle_main_menu(3, GameConfig):-
    difficulty_menu(GameConfig).

handle_main_menu(4, GameConfig):-
    size_menu(GameConfig).
handle_main_menu(5, GameConfig):-
    write('Bye bye.'), nl,
    halt.


game_type_menu(game_config(_, Difficulty, _)) :-
    write('1. Human vs Human (H/H)'), nl,
    write('2. Human vs Computer (H/PC)'), nl,
    write('3. Computer vs Human (PC/H)'), nl,
    write('4. Computer vs Computer (PC/PC)'), nl, nl,
    write('Choose an option (1-4): '),
    read(Option), nl,
    handle_game_type_menu(Option, GameType),
    main_menu(game_config(GameType, Difficulty, Size)).

handle_game_type_menu(1, human_vs_human).
handle_game_type_menu(2, human_vs_computer).
handle_game_type_menu(3, computer_vs_human).
handle_game_type_menu(4, computer_vs_computer).


difficulty_menu(game_config(GameType, _, Size)) :-
    write('1. easy'), nl,
    write('2. medium'), nl,
    write('3. hard'), nl,
    write('Choose an option (1-3): '),
    read(Option), nl,
    handle_difficulty_menu(Option, Difficulty),
    main_menu(game_config(GameType, Difficulty, Size)).

handle_difficulty_menu(1, easy).
handle_difficulty_menu(2, medium).
handle_difficulty_menu(3, hard).

size_menu(game_config(GameType, Difficulty, _)) :-
    write('1. small board\t(6x6)'), nl,
    write('2. big board\t(8x8)'), nl,
    write('Choose an option (1-2): '),
    read(Option), nl,
    handle_size_menu(Option, Size),
    main_menu(game_config(GameType, Difficulty, Size)).

handle_size_menu(1, small).
handle_size_menu(2, big).








%%%%%%%%%%% initial state %%%%%%%%%%%


initial_state(game_config(GameType, Difficulty, Size), GameState) :-
    % Create an empty board
    (Size = small -> BoardSize = 6 ; BoardSize = 8),
    create_empty_board(BoardSize, Board),
    % Define the current player as player1
    CurrentPlayer = player1,
    % Initialize captured pieces and pieces yet to be played
    CapturedPieces = [],
    PiecesToPlay = [],
    % Construct the game state
    GameState = game_state(
        Board,
        CurrentPlayer,
        CapturedPieces,
        PiecesToPlay,
        GameType,
        Difficulty
    ).

% Create an empty board
create_empty_board(Size, Board) :-
    create_empty_row(Size, Row),
    create_empty_rows(Size, Row, Board).

% Create an empty row of a given size
create_empty_row(Size, Row) :-
    length(Row, Size),
    fill_with_empty(Row).

% Fill a list with the atom `empty`
fill_with_empty([]).
fill_with_empty([1 | Rest]) :-
    fill_with_empty(Rest).

% Create multiple rows of the same type
create_empty_rows(0, _, []).
create_empty_rows(Size, Row, [Row | RestRows]) :-
    Size > 0,
    NewSize is Size - 1,
    create_empty_rows(NewSize, Row, RestRows).



%%%%%%%%%%% DISPLAY %%%%%%%%%%%


% Display the game state
display_game(game_state(Board, CurrentPlayer, _, _, _, _)) :-
    nl,
    write('Current Player: '), write(CurrentPlayer), nl, nl,
    display_board(Board),
    nl.

% Display the board with coordinates
display_board(Board) :-
    length(Board, Size),
    display_rows(Board, Size),
    display_column_headers(Size).

% Display column headers (1, 2, 3, ...)
display_column_headers(Size) :-
    Padding = '      ',
    write(Padding),
    display_column_headers_limiter(Size),nl,
    write(Padding),  % Padding for row numbers
    numlist(1, Size, Columns),  % Use the custom numlist
    maplist(format_column_header, Columns),
    nl.

display_column_headers_limiter(0).
display_column_headers_limiter(Size):-
    Size > 0,
    write('---'),
    NewSize is Size - 1,
    display_column_headers_limiter(NewSize).

% Format a single column header
format_column_header(Column) :-
    format(' ~w ', [Column]).

% Format a single column header
format_column_header(Column) :-
    format(' ~w ', [Column]).

% Display each row
display_rows([], _).
display_rows([Row | Rest], RowIndex) :-
    format('~2|~w | ', [RowIndex]),  % Write the row number with padding
    display_row(Row),
    nl,
    NewRowIndex is RowIndex - 1,
    display_rows(Rest, NewRowIndex).

% Display a single row
display_row([]).
display_row([Cell | Rest]) :-
    (Cell = empty -> Symbol = '.' ; Symbol = Cell),  % Use '.' for empty cells
    format(' ~w ', [Symbol]),
    display_row(Rest).


%for fidsplay column header function
% Equivalent of numlist(Start, End, List)
numlist(Start, End, List) :-
    Start =< End,
    numlist_helper(Start, End, List).

% Helper predicate to recursively build the list
numlist_helper(Current, End, [Current | Rest]) :-
    Current =< End,
    Next is Current + 1,
    numlist_helper(Next, End, Rest).
numlist_helper(Current, End, []) :-
    Current > End.

% Apply a predicate to every element in a list
maplist(_, []).
maplist(Predicate, [Head | Tail]) :-
    call(Predicate, Head),
    maplist(Predicate, Tail).





%%%%%%%%%%% TODO: %%%%%%%%%%%


move(GameState, Move, NewGameState).

valid_moves(GameState, Winner).

game_over(GameState, Winner).

value(GameState, Player, Value).

choose_move(GameState, Level, Move).