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




game_cycle(GameState):-
    GameState = game_state(_,CurrentPlayer,_,_,GameType,_),
    display_game(GameState),
    game_over(GameState, Winner),
    !,
    format("Game over! Winner is: ~w~n", [Winner]), nl, nl.
game_cycle(GameState) :-
    GameState = game_state(_, CurrentPlayer, _, _, GameType, _),
    species_identificator(CurrentPlayer, GameType, IsHuman),
    next_move(IsHuman, GameState, Move),
    move(GameState, Move, NewGameState),
    print_move(NewGameState, Move),
    game_cycle(NewGameState).


print_move(GameState, Move) :-
    % Extract the difficulty and current player from the game state
    GameState = game_state(_, CurrentPlayer, _, _, _, Difficulty),

    % Determine the format of the move based on difficulty
    (   Difficulty == 2
    ->  % Difficulty 2 format
        Move = ((OriginX, OriginY), DestinationX, DestinationY)
    ;   % Other difficulties
        Move = ((OriginX, OriginY), (DestinationX, DestinationY))
    ),nl,

    switch_player(CurrentPlayer, NextPlayer),

    % Print the move
    format('~w played (~w, ~w) -> (~w, ~w)', [NextPlayer, OriginX, OriginY, DestinationX, DestinationY]), nl.


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
    write(Prompt),write(' ('),write(Min),write('-'),write(Max),write(') '),
    read(Input),
    (integer(Input), Input >= Min, Input =< Max ->
        Option = Input
    ;
        write('Invalid option. Please try again.'), nl,
        read_option(Prompt, Min, Max, Option)
    ).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INITIAL STATE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


initial_state(game_config(GameType, Difficulty, Size), GameState) :-
    % Create a board
    (Size = small -> BoardSize = 6 ; BoardSize = 8),
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
    nl,nl,
    write('Current Player: '),
    color_player(CurrentPlayer),nl, nl,
    display_board(Board),
    nl.

color_player(player1):-
    write_colored_text(blue, player1).
    
color_player(player2):-
    write_colored_text(red, player2).

% Display the board with coordinates
display_board(Board) :-
    length(Board, Size),
    reverse(Board, ReversedBoard),  % Reversed the list to make it consistent with the display
    write('  X'), nl,
    display_rows(ReversedBoard, Size),
    display_column_headers(Size).


% Display column headers (1, 2, 3, ...)
display_column_headers(Size) :-
    Padding = '      ',
    write(Padding),
    display_column_headers_limiter(Size),nl,
    write(Padding),  
    numlist(1, Size, Columns),  
    maplist(format_column_header, Columns),
    write(' Y'), nl.  

display_column_headers_limiter(0).
display_column_headers_limiter(Size):-
    Size > 0,
    write('---'),
    NewSize is Size - 1,
    display_column_headers_limiter(NewSize).

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
display_row([(Stack, Player) | Rest]) :-
    write(' '),
    display_cell(Stack, Player),
    write(' '),
    display_row(Rest).

% Display a single cell with color
display_cell(Stack, player1) :- write_colored_text(blue, Stack).
display_cell(Stack, player2) :- write_colored_text(red, Stack).
display_cell(0, _) :- write('.').


write_with_color(ColorCode, Text) :-
    format('\e[~wm~w\e[0m', [ColorCode, Text]).
% Define color codes
color_code(black, 30).
color_code(red, 31).
color_code(green, 32).
color_code(yellow, 33).
color_code(blue, 34).
color_code(magenta, 35).
color_code(cyan, 36).
color_code(white, 37).

% Example usage
write_colored_text(Color, Text) :-
    color_code(Color, Code),
    write_with_color(Code, Text).



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









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MOVE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


move(GameState, Move, NewGameState) :-
    
    GameState = game_state(Board, CurrentPlayer, CapturedPieces, PiecesToPlay, GameType, Difficulty),
    NewGameState = game_state(NewBoard, NextPlayer, NewCapturedPieces, PiecesToPlay, GameType, Difficulty),

    %valid_move(Board, CurrentPlayer, Move),
    apply_move(Board, Move, NewBoard, CurrentPlayer, NewCapturedPieces),
    switch_player(CurrentPlayer, NextPlayer).



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


orthogonal_one_square(OriginX, OriginY, DestinationX, DestinationY) :-
    (DestinationX = OriginX, abs(DestinationY - OriginY) =:= 1) ;  % Horizontal move
    (DestinationY = OriginY, abs(DestinationX - OriginX) =:= 1).  % Vertical move

closest_stack(Board, (OriginX, OriginY), (StackX, StackY)) :-
    findall((X, Y), (nth1(X, Board, Row), nth1(Y, Row, (Stack, _)), Stack > 0, (X, Y) \= (OriginX, OriginY)), Stacks),
    maplist(distance((OriginX, OriginY)), Stacks, Distances),
    min_member(MinDistance, Distances),
    nth1(Index, Distances, MinDistance),
    nth1(Index, Stacks, (StackX, StackY)).

distance((X1, Y1), (X2, Y2), Distance) :-
    Distance is abs(X1 - X2) + abs(Y1 - Y2).


% valid_destination(+Board, +Origin, +Destination, +DestCell, +CurrentPlayer, +OriginStack)
valid_destination(Board, Origin, Destination, (0, empty), CurrentPlayer, _) :-
    closest_stack(Board, Origin, (StackX, StackY)),
    distance(Origin, (StackX, StackY), OriginDistance),
    distance(Destination, (StackX, StackY), DestinationDistance),
    (DestinationDistance =< OriginDistance,
        %write('Valid move from ('), write(Origin), write(') to ('), write(Destination),write(')'),
        nl -> true ; 
        %write('Invalid move: Does not bring piece closer to its closest stack.'), nl, 
        fail),
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
    (validate_input(GameState, Input) -> Move = Input;
     write('Invalid move. Please try again.'), nl,
     prompt_for_move(GameState, Move)).

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

% Define game_over/2 to check the condition and determine the winner
game_over(game_state(Board, _, _, _, _, _), Winner) :-
    count_pieces(Board, player1, Player1Count),
    count_pieces(Board, player2, Player2Count),
    winner(Player1Count, Player2Count, Winner).

winner(Player1Count, 0, player1).
winner(0, Player2Count, player2).

% Helper predicate to count pieces for a player
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

% Select the best move based on score (CHOOSES RANDOM BEST MOVE) (BUG)
select_best_move(ScoredMoves, BestMove) :-
    keysort(ScoredMoves, SortedMoves),
    reverse(SortedMoves, [_HighestScore-_ | _]), % Get the highest score
    findall(Move, member(HighestScore-Move, SortedMoves), BestMoves), % Collect all moves with the highest score
    random_member(BestMove, BestMoves). % Select a random move from the best moves


% Select the best move based on score  (ALWAYS CHOOSES LAST BEST MOVE)
select_best_move(ScoredMoves, BestMove) :-
    keysort(ScoredMoves, SortedMoves),
    reverse(SortedMoves, [_HighestScore-BestMove | _]). % Extract the move with the highest score.

% value(+GameState, -Score)
% Define a heuristic to evaluate the game state.
value(game_state(_, CurrentPlayer, CapturedPieces, _, _, _), Score) :-
    % Example heuristic: Number of pieces captured
    include(player_captured(CurrentPlayer), CapturedPieces, PlayerCaptured),
    length(PlayerCaptured, Score).

% Check if a captured piece belongs to the player
player_captured(Player, (_, Player)).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TODO: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




choose_move(GameState, Level, Move). % this CANNOT be deleted for black magic reasons

