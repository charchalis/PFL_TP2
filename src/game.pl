% Anaash Game in SICStus Prolog

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).

play :-
    welcome,
    DefaultConfig = game_config(computer_vs_computer, 2, small),
    main_menu(DefaultConfig).

welcome :-
    nl,
    write('----------------------'), nl,
    write('| Welcome to Anaash! |'), nl,
    write('----------------------'), nl, nl.


reload :-
    [game]. %,
    % [tests].  % reload when changes are made

r :-
    reload,
    play.





/**
 * game_cycle/1
 * 
 * Manages the game loop by displaying the game state, checking for game over conditions,
 * and processing the next move based on the current player and game type.
 * 
 * @param GameState The current state of the game
 * 
 * The function displays the game state, checks if the game is over, and if not, determines
 * the next move and applies it, continuing the game loop with the new game state.
 */
game_cycle(GameState):- 
    GameState = game_state(_, CurrentPlayer, _, _, GameType, _),
    display_game(GameState), % Display the current game state

    game_over(GameState, Winner), % Check if the game is over
    !, % Cut to prevent backtracking
    format("Game over! Winner is: ~w~n", [Winner]), nl, nl. % Print the winner


game_cycle(GameState) :- 
    GameState = game_state(_, CurrentPlayer, _, _, GameType, _), % Get the current player and the game type
    species_identificator(CurrentPlayer, GameType, IsHuman), % Check if the current player is human
    next_move(IsHuman, GameState, Move), % Get the next move
    move(GameState, Move, NewGameState), % Apply the move
    print_move(NewGameState, Move), % Print the move
    game_cycle(NewGameState). % Continue the game loop with the new game state


print_move(GameState, ((OriginX, OriginY), (DestinationX, DestinationY))) :-
    GameState = game_state(_, CurrentPlayer, _, _, _, _), % Get the current player
    switch_player(CurrentPlayer, NextPlayer),nl, % Get the next player
    format('~w played (~w, ~w) -> (~w, ~w)', [NextPlayer, OriginX, OriginY, DestinationX, DestinationY]), nl. % Print the move


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MENU %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


main_menu(GameConfig) :-
    GameConfig = game_config(GameType, Difficulty, Size),
    write('1. start ( '), write(GameType), write(' | '), write(Difficulty), write(' | '), write(Size), write(' board )'), nl,
    write('2. game type'), nl,
    write('3. difficulty'), nl,
    write('4. board size'), nl,
    write('5. leave'), nl, nl,
    read_option('Choose an option', 1, 5, Option),
    handle_main_menu(Option, GameConfig).

handle_main_menu(1, GameConfig):-
    write('Starting game...'), nl,
    initial_state(GameConfig, GameState),
    game_cycle(GameState),
    main_menu(GameConfig).

handle_main_menu(2, GameConfig):-
    game_type_menu(GameConfig).

handle_main_menu(3, GameConfig):-
    difficulty_menu(GameConfig).

handle_main_menu(4, GameConfig):-
    size_menu(GameConfig).
handle_main_menu(5, GameConfig):-
    write('Bye bye.'), nl,
    halt.


game_type_menu(game_config(_, Difficulty, Size)) :-
    write('1. Human vs Human (H/H)'), nl,
    write('2. Human vs Computer (H/PC)'), nl,
    write('3. Computer vs Human (PC/H)'), nl,
    write('4. Computer vs Computer (PC/PC)'), nl, nl,
    read_option('Choose an option', 1, 4, Option),
    handle_game_type_menu(Option, GameType),
    main_menu(game_config(GameType, Difficulty, Size)).

handle_game_type_menu(1, human_vs_human).
handle_game_type_menu(2, human_vs_computer).
handle_game_type_menu(3, computer_vs_human).
handle_game_type_menu(4, computer_vs_computer).


difficulty_menu(game_config(GameType, _, Size)) :-
    write('1. random'), nl,
    write('2. greedy'), nl,
    write('3. fancy'), nl,
    read_option('Choose an option', 1, 3, Option),
    handle_difficulty_menu(Option, Difficulty),
    main_menu(game_config(GameType, Difficulty, Size)).

handle_difficulty_menu(1, 1).
handle_difficulty_menu(2, 2).
handle_difficulty_menu(3, 3).

size_menu(game_config(GameType, Difficulty, _)) :-
    write('1. small board\t(6x6)'), nl,
    write('2. big board\t(8x8)'), nl,
    read_option('Choose an option', 1, 2, Option),
    handle_size_menu(Option, Size),
    main_menu(game_config(GameType, Difficulty, Size)).

handle_size_menu(1, small).
handle_size_menu(2, big).


read_option(Prompt, Min, Max, Option) :-
    write(Prompt), write(' ('), write(Min), write('-'), write(Max), write(') '),
    read(Input),
    validate_option(Input, Min, Max, Option).

validate_option(Input, Min, Max, Option) :-
    integer(Input),
    Input >= Min,
    Input =< Max,
    Option = Input.

validate_option(_, Min, Max, Option) :-
    write('Invalid option. Please try again.'), nl,
    read_option('Choose an option', Min, Max, Option).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INITIAL STATE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * initial_state/2
 * 
 * Initializes the game state based on the provided game configuration.
 * 
 * @param GameConfig The configuration of the game, including game type, difficulty, and board size.
 * @param GameState The initial state of the game, including the board, current player, captured pieces, and pieces to play.
 * 
 * The function generates the board based on the specified size, sets the current player to player1,
 * initializes captured pieces and pieces to play, and constructs the initial game state.
 */
initial_state(game_config(GameType, Difficulty, Size), GameState) :-
    % Create a board
    (Size = small, BoardSize = 6 ; Size = big, BoardSize = 8),
    generate_board(BoardSize, Board),
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

% Generate the entire board with intercalated pieces
generate_board(Size, Board) :-
    generate_board_rows(Size, 1, Size, Board).

% Generate rows with alternating starting players
generate_board_rows(0, _, _, []).
generate_board_rows(RemainingRows, StartPlayerIndex, Size, [Row | RestRows]) :-
    RemainingRows > 0,
    generate_board_row(StartPlayerIndex, Size, Row),
    NextPlayerIndex is 3 - StartPlayerIndex, % Alternate between 1 and 2
    NewRemainingRows is RemainingRows - 1,
    generate_board_rows(NewRemainingRows, NextPlayerIndex, Size, RestRows).

% Generate a single row
generate_board_row(StartPlayerIndex, Size, Row) :-
    length(Row, Size),
    populate(StartPlayerIndex, Row).

% Populate a row with alternating pieces
populate(_, []).
populate(CurrentPlayerIndex, [(1, Player) | Rest]) :-
    player_from_index(CurrentPlayerIndex, Player),
    NextPlayerIndex is 3 - CurrentPlayerIndex, % Alternate between 1 and 2
    populate(NextPlayerIndex, Rest).

% Map player indices to player terms
player_from_index(1, player1).
player_from_index(2, player2).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DISPLAY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Display the game state
display_game(game_state(Board, CurrentPlayer, _, _, _, _)) :-
    nl, nl,
    write('Current Player: '),
    color_player(CurrentPlayer), nl, nl,
    display_board(Board),
    nl.

color_player(player1) :-
    write_colored_text(blue, player1).
    
color_player(player2) :-
    write_colored_text(red, player2).

% Display the board with switched X and Y axes
display_board(Board) :-
    length(Board, Size),
    write('  Y'), nl,
    display_rows(Board, Size),
    display_column_headers(Size),
    %write('nah'),nl,

    nl.

% Display column headers (1, 2, 3, ...)
display_column_headers(Size) :-
    Padding = '      ',
    write(Padding),
    display_column_headers_limiter(Size), nl,
    write(Padding),
    numlist(1, Size, Columns),
    maplist(format_column_header, Columns),
    write(' X'), nl.

display_column_headers_limiter(0).
display_column_headers_limiter(Size) :-
    Size > 0,
    write('---'),
    NewSize is Size - 1,
    display_column_headers_limiter(NewSize).

% Format a single column header
format_column_header(Column) :-
    format(' ~w ', [Column]).

% Display each row (now aligned with columns)
display_rows(_, 0).
display_rows(Board, RowIndex) :-
    RowIndex > 0,  % Proceed only if RowIndex is positive.
    format('~2|~w | ', [RowIndex]),  % Write the row number with padding
    display_row(Board, RowIndex),
    nl,
    NewRowIndex is RowIndex - 1,
    %write(NewRowIndex),write(' wuh'),nl,
    display_rows(Board, NewRowIndex).

% Display a single row by collecting column values
display_row(Board, RowIndex) :-
    maplist(nth1(RowIndex), Board, RowCells),
    display_cells(RowCells).

% Display all cells in a row
display_cells([]).
display_cells([(Stack, Player) | Rest]) :-
    write(' '),
    display_cell(Stack, Player),
    write(' '),
    display_cells(Rest).
display_cells([0 | Rest]) :-  % For empty cells
    write(' . '),
    display_cells(Rest).

% Display a single cell with color
display_cell(Stack, player1) :- write_colored_text(blue, Stack).
display_cell(Stack, player2) :- write_colored_text(red, Stack).

% Display a single cell for empty
display_cell(0, _) :- write('.').

% Color-related predicates remain unchanged
write_with_color(ColorCode, Text) :-
    format('\e[~wm~w\e[0m', [ColorCode, Text]).

color_code(black, 30).
color_code(red, 31).
color_code(green, 32).
color_code(yellow, 33).
color_code(blue, 34).
color_code(magenta, 35).
color_code(cyan, 36).
color_code(white, 37).

write_colored_text(Color, Text) :-
    color_code(Color, Code),
    write_with_color(Code, Text).

% Helper predicates for numlist
numlist(Start, End, List) :-
    Start =< End,
    numlist_helper(Start, End, List).

numlist_helper(Current, End, [Current | Rest]) :-
    Current =< End,
    Next is Current + 1,
    numlist_helper(Next, End, Rest).
numlist_helper(Current, End, []) :-
    Current > End.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MOVE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/**
 * move/3
 *
 * Updates the game state by applying a move.
 *
 * @param GameState The current state of the game, represented as a game_state/6 term.
 * @param Move The move to be applied, represented in a format understood by apply_move/5.
 * @param NewGameState The resulting state of the game after the move is applied, represented as a game_state/6 term.
 *
 */
move(GameState, Move, NewGameState) :-
    
    GameState = game_state(Board, CurrentPlayer, CapturedPieces, PiecesToPlay, GameType, Difficulty),
    NewGameState = game_state(NewBoard, NextPlayer, NewCapturedPieces, PiecesToPlay, GameType, Difficulty),

    %valid_move(Board, CurrentPlayer, Move),
    apply_move(Board, Move, NewBoard, CurrentPlayer, NewCapturedPieces),
    switch_player(CurrentPlayer, NextPlayer).



% valid_move(+Board, +CurrentPlayer, +(Origin, Destination)) is semidet.
%
% Checks if a move is valid in the current gamestate.
%
% @param Board The current state of the game board, represented as a list of lists.
% @param CurrentPlayer The player making the move.
% @param Origin The coordinates of the piece to be moved, in the form (OriginX, OriginY).
% @param Destination The coordinates of the destination cell, in the form (DestinationX, DestinationY).
%
% If all validations pass, the move is considered valid.
valid_move(Board, CurrentPlayer, (Origin, Destination)) :-
    Origin = (OriginX, OriginY),
    Destination = (DestinationX, DestinationY),
    % Ensure the move is orthogonal and one square away
    orthogonal_one_square(OriginX, OriginY, DestinationX, DestinationY),
    
    % Validate origin cell
    nth1(OriginX, Board, Row),         % Get the row at OriginX
    nth1(OriginY, Row, (OriginStack, Owner)), % Get the cell at OriginY
    (Owner = CurrentPlayer -> true ; 
        write('Invalid move: Not your piece.'), nl, fail),
    
    % Validate destination cell
    nth1(DestinationX, Board, DestRow), % Get the row at DestinationX
    nth1(DestinationY, DestRow, DestCell), % Get the cell at DestinationY

    (   forced_moves(Board, CurrentPlayer, ForcedMoves),
        %write('Forced moves: '), write(ForcedMoves), nl,
        ForcedMoves \= []
    ->  member((Origin, Destination), ForcedMoves)
    ;   
        valid_destination(Board, Origin, Destination, DestCell, CurrentPlayer, OriginStack)
    ).
    % If all validations pass, print confirmation
    % write('Valid move from ('), write(Origin), write(') to ('), write(Destination),write(')'), nl.

%Auxiliary function to check if the move is orthogonal and one square away
orthogonal_one_square(OriginX, OriginY, DestinationX, DestinationY) :-
    (DestinationX = OriginX, abs(DestinationY - OriginY) =:= 1) ;  % Horizontal move
    (DestinationY = OriginY, abs(DestinationX - OriginX) =:= 1).  % Vertical move

%Auxiliary function to find the closest stack to a given piece
closest_stack(Board, (OriginX, OriginY), (StackX, StackY)) :-
    findall((X, Y), (nth1(X, Board, Row), nth1(Y, Row, (Stack, _)), Stack > 0, (X, Y) \= (OriginX, OriginY)), Stacks),
    maplist(distance((OriginX, OriginY)), Stacks, Distances),
    min_member(MinDistance, Distances),
    nth1(Index, Distances, MinDistance),
    nth1(Index, Stacks, (StackX, StackY)).

%Auxiliary function to calculate the Manhattan distance between two points
distance((X1, Y1), (X2, Y2), Distance) :-
    Distance is abs(X1 - X2) + abs(Y1 - Y2).



% valid_destination(+Board, +Origin, +Destination, +DestinationInfo, +CurrentPlayer, +OriginStack)
%
% Checks if a move from Origin to Destination is valid based on the game rules.
%
% @param Board The current state of the game board.
% @param Origin The coordinates of the piece's current position.
% @param Destination The coordinates of the piece's intended position.
% @param DestinationInfo A tuple containing the stack size and owner at the Destination.
% @param CurrentPlayer The player making the move.
% @param OriginStack The size of the stack at the Origin.
%
% 1. If the Destination is empty (stack size 0), the move is valid if it brings the piece closer to its closest stack.
% 2. If the Destination is not empty:
%    - The move is valid if the Destination is a friendly stack with a higher or equal value than the Origin stack.
%    - The move is valid if the Destination is an enemy stack with a lower or equal value than the Origin stack.
%
valid_destination(Board, Origin, Destination, (0, empty), CurrentPlayer, _) :-
    closest_stack(Board, Origin, (StackX, StackY)),
    distance(Origin, (StackX, StackY), OriginDistance),
    distance(Destination, (StackX, StackY), DestinationDistance),
    DestinationDistance =< OriginDistance,
    !.  % Destination is empty -> valid if it brings the piece closer to its closest stack.

valid_destination(_, _, _, (DestStack, DestOwner), CurrentPlayer, OriginStack) :-
    (   % Friendly stack with higher value
        DestOwner = CurrentPlayer,
        DestStack >= OriginStack,
        DestStack > 0
    ;
        % Enemy stack with lower value
        DestOwner \= CurrentPlayer,
        DestStack =< OriginStack,
        DestStack > 0
    ),
    !.  % If either condition is true, destination is valid.

valid_destination(_, _, _, _, _, _) :-
    %write('Invalid move: Destination square does not satisfy move rules.'), nl, 
    fail.

/**
 * forced_moves/3
 * 
 * Finds all the forced moves for the current player on the given board.
 * 
 * @param Board The current state of the game board, represented as a list of lists.
 * @param CurrentPlayer The player whose forced moves are being determined.
 * @param ForcedMoves A list of forced moves for the current player. Each move is represented as a pair of coordinates ((OriginX, OriginY), (DestinationX, DestinationY)).
 * 
 * Captures and stacks are forced moves. If they are available, no other moves can be played.
 * 
 * The resulting list of forced moves is sorted to remove duplicates and checked to make sure it's not empty.
 */
forced_moves(Board, CurrentPlayer, ForcedMoves) :-
    findall(((OriginX, OriginY), (DestinationX, DestinationY)), (
        nth1(OriginX, Board, Row),
        nth1(OriginY, Row, (Stack, Owner)),
        Owner = CurrentPlayer,
        % Possible destination squares for the piece: 
        (DestinationX is OriginX + 1, DestinationY = OriginY;
         DestinationX is OriginX - 1, DestinationY = OriginY;
         DestinationX = OriginX, DestinationY is OriginY + 1;
         DestinationX = OriginX, DestinationY is OriginY - 1),
        nth1(DestinationX, Board, DestRow),
        nth1(DestinationY, DestRow, (DestStack, DestOwner)),
        DestOwner \= empty,  
        (   % Check for captures
            DestOwner \= CurrentPlayer,
            DestStack =< Stack,
            DestStack > 0
        ;   % Check for stacks
            DestOwner = CurrentPlayer,
            DestStack >= Stack,
            DestStack > 0
        )
    ), ForcedMovesUnsorted),
    sort(ForcedMovesUnsorted, ForcedMoves),
    ForcedMoves \= [].

switch_player(player1,player2).
switch_player(player2,player1).



/**
 * apply_move/5
 * 
 * Applies a move in the game by updating the board state.
 * 
 * @param Board The current state of the game board.
 * @param ((OriginX, OriginY), (DestinationX, DestinationY)) The coordinates of the origin and destination cells.
 * @param NewBoard The new state of the game board after the move is applied.
 * @param CurrentPlayer The player making the move.
 * @param NewCapturedPieces The pieces captured as a result of the move.
 * 
 * 1 - Extracts the origin cell from the board.
 * 2 - Extracts the destination cell from the board.
 * 3 - Handles the stacking logic for the destination cell:
 *      - If the destination cell is empty, the origin stack is moved to the destination.
 *      - If the destination cell is owned by the current player, the stacks are combined.
 *      - If the destination cell is owned by the opponent, the destination stack is captured.
 * 4 - Updates the destination row with the new destination cell.
 * 5 - Removes the origin stack from the origin cell.
 * 6 - Updates the origin row with the empty cell.
 */
apply_move(Board, ((OriginX, OriginY), (DestinationX, DestinationY)), NewBoard, CurrentPlayer, NewCapturedPieces) :-

    %write('applying move'),nl,
    %format('Origin: (~w, ~w), Destination: (~w, ~w)', [OriginX, OriginY, DestinationX, DestinationY]), nl,

    % Extract the Origin Cell
    nth1(OriginX, Board, OriginRow),
    nth1(OriginY, OriginRow, OriginCell),
    %format('Origin Cell: ~w', [OriginCell]), nl,

    OriginCell = (OriginStack, _Owner),
    
    % Extract the Destination Cell
    nth1(DestinationX, Board, DestRow),
    nth1(DestinationY, DestRow, DestCell),
    %format('Destination Cell: ~w', [DestCell]), nl,
    
    % Handle stacking logic for the destination
    (
        DestCell = (0,empty) ->
            NewDestCell = (OriginStack, CurrentPlayer),
            NewCapturedPieces = []
        ;
        DestCell = (DestStack, DestOwner),
        (
            DestOwner = CurrentPlayer ->
                NewStack is OriginStack + DestStack,
                NewDestCell = (NewStack, CurrentPlayer),
                NewCapturedPieces = []
            ;
                NewCapturedPieces = [(DestStack, DestOwner)],
                NewDestCell = (OriginStack, CurrentPlayer)
        )
    ),
    
    % Update the destination row
    replace_cell(DestRow, DestinationY, NewDestCell, UpdatedDestRow),
    replace_row(Board, DestinationX, UpdatedDestRow, TempBoard),

    
    % Extract the Origin Cell from TempBoard
    nth1(OriginX, TempBoard, TempOriginRow),
    nth1(OriginY, OriginRow, OriginCell),

    % Remove the origin stack
    replace_cell(TempOriginRow, OriginY, (0,empty), UpdatedOriginRow),
    replace_row(TempBoard, OriginX, UpdatedOriginRow, NewBoard).

% Replace a cell in a row
replace_cell(Row, Index, NewCell, NewRow) :-
    nth1(Index, Row, _OldCell, Rest),
    nth1(Index, NewRow, NewCell, Rest).

% Replace a row in a board
replace_row(Board, Index, NewRow, NewBoard) :-
    nth1(Index, Board, _OldRow, Rest),
    nth1(Index, NewBoard, NewRow, Rest).




% Prompt the human player for their move
prompt_for_move(GameState, Move) :-
    write('You can type "moves" to see the valid list of moves.'), nl,
    write('You can type "exit" to return to main menu.'), nl,
    write('Start position (X1, Y1):'), nl,
    read(StartPos),
    handle_start_pos(StartPos, GameState, Move).

handle_start_pos(moves, GameState, Move) :-
    valid_moves(GameState, ListOfMoves),
    write('Valid moves:'), nl,
    print_valid_moves(ListOfMoves),
    prompt_for_move(GameState, Move).

handle_start_pos(exit, GameState, _) :-
    GameState = game_state(_, _, _, _, GameType, Difficulty), nl, nl,
    % Reuse configuration to return to the menu
    main_menu(game_config(GameType, Difficulty, small)).

handle_start_pos(StartPos, GameState, Move) :-
    write('End position (X2, Y2):'), nl,
    read(EndPos),
    Input = (StartPos, EndPos),
    validate_input(GameState, Input),
    !,
    Move = Input.

handle_start_pos(_, GameState, Move) :-
    write('Invalid move. Please try again.'), nl,
    prompt_for_move(GameState, Move).

print_valid_moves([]).
print_valid_moves([(Origin, Destination) | Rest]) :-
    format('Move from ~w to ~w~n', [Origin, Destination]),
    print_valid_moves(Rest).

validate_input(GameState, Input):-
    GameState = game_state(
        Board,
        CurrentPlayer,
        CapturedPieces,
        PiecesToPlay,
        GameType,
        Difficulty
    ),
    Input = (Origin, Destination),
    valid_move(Board, CurrentPlayer, (Origin, Destination)).

% This computes the valid moves for every piece of the current player 
valid_moves(GameState, ListOfMoves) :-
    GameState = game_state(Board, CurrentPlayer, _, _, _, _),
    findall((Origin, Destination), (
        member(Row, Board),
        nth1(X, Board, Row),
        nth1(Y, Row, (PieceValue, PiecePlayer)),
        PiecePlayer = CurrentPlayer,
        Origin = (X, Y),
        % Possible destination squares for the piece: 
        (DestinationX is X + 1, Destination = (DestinationX, Y);
         DestinationX is X - 1, Destination = (DestinationX, Y);
         DestinationY is Y + 1, Destination = (X, DestinationY);
         DestinationY is Y - 1, Destination = (X, DestinationY)),
        valid_move(Board, CurrentPlayer, (Origin, Destination))
    ), Moves),
    sort(Moves, ListOfMoves).




species_identificator(_, human_vs_human, true).
species_identificator(player1, human_vs_computer, true).
species_identificator(player2, computer_vs_human, true).
species_identificator(_,_,false).

next_move(true, GameState, Move):-
    choose_move(0, GameState, Move).
next_move(false, GameState, Move):-
    GameState = game_state(_,_,_,_,_,Difficulty),
    choose_move(Difficulty, GameState, Move).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% GAME OVER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

game_over(game_state(Board, _, _, _, _, _), Winner) :-
    %write('hhere'),nl,
    count_pieces(Board, player1, Player1Count),
    %write('hhere2'),nl,
    count_pieces(Board, player2, Player2Count),
    %write('hhere3'),nl,
    winner(Player1Count, Player2Count, Winner).

winner(Player1Count, 0, player1).
winner(0, Player2Count, player2).

count_pieces(Board, Player, Count) :-
    findall(Stack, (
        nth1(_, Board, Row),
        nth1(_, Row, (Stack, Player)),
        Stack > 0
    ), Pieces),
    length(Pieces, Count).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CHOOSE MOVES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Level 0: Human player move
choose_move(0, GameState, Move) :-
    prompt_for_move(GameState, Move).



% Level 1: Random move
choose_move(1, GameState, Move):-
    valid_moves(GameState, ListOfMoves),
    %print_valid_moves(ListOfMoves),
    random_member(Move, ListOfMoves).


% choose_move(+Difficulty, +GameState, -Move)
choose_move(2, GameState, BestMove) :- % Level 2: Greedy strategy
    GameState = game_state(Board, CurrentPlayer, _, _, _, _),
    findall(Move, valid_moves_for_player(Board, CurrentPlayer, Move), Moves),
    write('Evaluating moves:'),nl,
    evaluate_moves(GameState, Moves, ScoredMoves),
    select_best_move(ScoredMoves, BestMove).









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% VALUE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Generate all valid moves for a player
valid_moves_for_player(Board, Player, (Origin, Destination)) :-
    nth1(X, Board, Row),
    nth1(Y, Row, (Stack, Owner)),
    Owner = Player,
    Origin = (X, Y),
    (DestinationX is X + 1, Destination = (DestinationX, Y);
     DestinationX is X - 1, Destination = (DestinationX, Y);
     DestinationY is Y + 1, Destination = (X, DestinationY);
     DestinationY is Y - 1, Destination = (X, DestinationY)),
    valid_move(Board, Player, (Origin, Destination)).

% Evaluate each move and assign a score using value/2
evaluate_moves(_, [], []).
evaluate_moves(GameState, [Move | RestMoves], [Score-Move | RestScoredMoves]) :-
    move(GameState, Move, NewGameState),
    value(NewGameState, Score),
    Move = ((OriginX, OriginY), DestinationX, DestinationY),
    format('(~w, ~w) -> (~w, ~w), value: ~w', [OriginX, OriginY, DestinationX, DestinationY, Score]), nl,
    evaluate_moves(GameState, RestMoves, RestScoredMoves).


select_best_move(ScoredMoves, BestMove) :-
    % Find the maximum score
    maplist(score_from_pair, ScoredMoves, Scores),
    max_in_list(Scores, MaxScore),

    % Filter the scored moves to get only the ones with the maximum score
    include(is_best_move(MaxScore), ScoredMoves, BestScoredMoves),

    % Randomly select one of the best moves
    random_member(_Score-BestMove, BestScoredMoves).

% Helper predicate to extract the score from a Score-Move pair
score_from_pair(Score-_, Score).

% Helper predicate to check if a Score-Move pair matches the max score
is_best_move(MaxScore, Score-_) :-
    Score =:= MaxScore.

% Base case: The maximum of a single-element list is that element itself.
max_in_list([X], X).

% Recursive case: Compare the head of the list with the maximum of the tail.
max_in_list([X | Rest], Max) :-
    max_in_list(Rest, TailMax),
    Max is max(X, TailMax).


% value(+GameState, -Score)
% Define a heuristic to evaluate the game state.
value(game_state(Board, CurrentPlayer, CapturedPieces, _, _, _), Score) :-

    %write('Current: '), write(CurrentPlayer), nl,
    switch_player(CurrentPlayer, RealCurrentPlayer),
    %write('Real Current: '), write(RealCurrentPlayer), nl,

    % Calculate piece values for the current player
    findall(PieceValue, (
        nth1(X, Board, Row),
        nth1(Y, Row, (Stack, Owner)),
        Owner = RealCurrentPlayer,
        positional_value(Board, X, Y, PositionalValue),
        stack_value(Stack, StackValue),
        PieceValue is PositionalValue * StackValue % Changing this to + will break the calculations for some reason
    ), CurrentPlayerPieceValues),
    sum_list(CurrentPlayerPieceValues, TotalCurrentPlayerPieceValue),

    % Calculate piece values for the opponent
    findall(PieceValue, (
        nth1(X, Board, Row),
        nth1(Y, Row, (Stack, Owner)),
        Owner \= RealCurrentPlayer,
        positional_value(Board, X, Y, PositionalValue),
        stack_value(Stack, StackValue),
        PieceValue is PositionalValue * StackValue % Changing this to + will break the calculations for some reason
    ), OpponentPieceValues),
    sum_list(OpponentPieceValues, TotalOpponentPieceValue),

    % Calculate threat penalties for the current player
    findall(ThreatPenalty, (
        nth1(X, Board, Row),
        nth1(Y, Row, (Stack, Owner)),
        Owner = RealCurrentPlayer,
        threat_value(Board, (Stack, Owner), X, Y, ThreatPenalty)
    ), CurrentPlayerThreatPenalties),
    sum_list(CurrentPlayerThreatPenalties, TotalCurrentPlayerThreatPenalty),


    % Weigh capturing more heavily than stacking and positional value
    %format('Total Current Player Piece Value: ~w, Total Opponent Piece Value: ~w', [TotalCurrentPlayerPieceValue, TotalOpponentPieceValue]), nl,
    %format('Total Current Player Threat Penalty: ~w', [TotalCurrentPlayerThreatPenalty]), nl,
    Score is (TotalCurrentPlayerPieceValue - TotalOpponentPieceValue) - TotalCurrentPlayerThreatPenalty.


stack_value(Stack, Value) :-
    Value is (Stack * (Stack//2 + 1)). % Higher stacks have more value

% Define positional value based on the piece's location
positional_value(Board, X, Y, Value) :-
    length(Board, BoardSize), % Determine the number of rows (board height)
    Center is BoardSize / 2, % Calculate the approximate center
    BoardCenterX is ceiling(Center),
    BoardCenterY is ceiling(Center),
    Distance is abs(BoardCenterX - X) + abs(BoardCenterY - Y), % Manhattan distance
    MaxDistance is BoardSize*2, % Maximum possible Manhattan distance
    Value is ceiling((MaxDistance - Distance)/2). % Quadratic weighting

% Checks if a piece is under threat
threat_value(Board, (Stack, Owner), X, Y, ThreatValue) :-
    findall(EnemyStack, (
        adjacent(X, Y, AdjX, AdjY),          % Check adjacent cells
        nth1(AdjX, Board, Row),
        nth1(AdjY, Row, (EnemyStack, EnemyOwner)),
        EnemyOwner \= Owner,                % Enemy piece
        EnemyStack >= Stack                 % Threatening stack
    ), ThreateningStacks),
    length(ThreateningStacks, ThreatCount), % Count how many threats
    ThreatValue is ThreatCount.        % Example penalty (adjustable)

adjacent(X, Y, AdjX, AdjY) :-
    (AdjX is X + 1, AdjY is Y);
    (AdjX is X - 1, AdjY is Y);
    (AdjX is X, AdjY is Y + 1);
    (AdjX is X, AdjY is Y - 1).

sum_list([], 0).
sum_list([Head | Tail], Sum) :-
    sum_list(Tail, TailSum),
    Sum is Head + TailSum.

% Check if a captured piece belongs to the player
player_captured(Player, (_, Player)).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TODO: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




choose_move(GameState, Level, Move). % this CANNOT be deleted for black magic reasons