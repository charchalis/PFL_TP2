/**
 * move/3
 *
 * Updates the game state by applying a move.
 *
 * @param GameState The current state of the game, represented as a game_state/4 term.
 * @param Move The move to be applied, represented in a format understood by apply_move/4.
 * @param NewGameState The resulting state of the game after the move is applied, represented as a game_state/4 term.
 *
 */
move(GameState, Move, NewGameState) :-
    
    GameState = game_state(Board, CurrentPlayer, GameType, Difficulty),
    NewGameState = game_state(NewBoard, NextPlayer, GameType, Difficulty),

    %valid_move(Board, CurrentPlayer, Move),
    apply_move(Board, Move, NewBoard, CurrentPlayer),
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

    valid_destination(Board, Origin, Destination, DestCell, CurrentPlayer, OriginStack).
    % If all validations pass, print confirmation
    % write('Valid move from ('), write(Origin), write(') to ('), write(Destination),write(')'), nl.

%Auxiliary function to check if the move is orthogonal and one square away
orthogonal_one_square(OriginX, OriginY, DestinationX, DestinationY) :-
    (DestinationX = OriginX, abs(DestinationY - OriginY) =:= 1) ;  % Horizontal move
    (DestinationY = OriginY, abs(DestinationX - OriginX) =:= 1).  % Vertical move


/**
 * closest_stack/3
 *
 * Finds the closest stack to a given piece on the board.
 *
 * @param Board The current game board.
 * @param Origin The coordinates of the piece's origin position.
 * @param (StackX, StackY) The coordinates of the closest stack.
 */
closest_stack(Board, (OriginX, OriginY), (StackX, StackY)) :-
    findall((X, Y), (nth1(X, Board, Row), nth1(Y, Row, (Stack, _)), Stack > 0, (X, Y) \= (OriginX, OriginY)), Stacks),
    maplist(distance((OriginX, OriginY)), Stacks, Distances),
    min_member(MinDistance, Distances),
    nth1(Index, Distances, MinDistance),
    nth1(Index, Stacks, (StackX, StackY)).

/**
 * distance/3
 *
 * Calculates the Manhattan distance between two points.
 *
 * @param (X1, Y1) The coordinates of the first point.
 * @param (X2, Y2) The coordinates of the second point.
 * @param Distance The computed Manhattan distance.
 */
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
apply_move(Board, ((OriginX, OriginY), (DestinationX, DestinationY)), NewBoard, CurrentPlayer) :-

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
            NewDestCell = (OriginStack, CurrentPlayer)
        ;
        DestCell = (DestStack, DestOwner),
        (
            DestOwner = CurrentPlayer ->
                NewStack is OriginStack + DestStack,
                NewDestCell = (NewStack, CurrentPlayer)
            ;
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
    GameState = game_state(_, _, GameType, Difficulty), nl, nl,
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
        GameType,
        Difficulty
    ),
    Input = (Origin, Destination),
    valid_move(Board, CurrentPlayer, (Origin, Destination)).

/**
 * valid_moves/2
 *
 * Determines all the valid moves available to the current player based on the current game state.
 *
 * @param GameState The current state of the game, which includes the game board, current player, and other game configurations.
 * @param ListOfMoves A list of valid moves that the current player can make.
 *
 * The predicate generates a list of valid moves by examining the current game state, 
 * and it considers:
 * - The current player's pieces (e.g., checking for valid starting positions).
 * - The rules for valid movement (e.g., checking if a move is orthogonal and within range).
 * - The destination cells and their compatibility with the current player's piece (e.g., friendly or enemy pieces, stacking rules).
 * The predicate may iterate through the board and generate possible moves for the player, 
 * filtering out invalid moves (e.g., moves that result in no change or violate game rules).
 */
valid_moves(GameState, ListOfMoves) :-
    GameState = game_state(Board, CurrentPlayer, _, _),
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
    GameState = game_state(_,_,_,Difficulty),
    choose_move(Difficulty, GameState, Move).


print_move(GameState, ((OriginX, OriginY), (DestinationX, DestinationY))) :-
    GameState = game_state(_, CurrentPlayer, _, _), % Get the current player
    switch_player(CurrentPlayer, NextPlayer),nl, % Get the next player
    format('~w played (~w, ~w) -> (~w, ~w)', [NextPlayer, OriginX, OriginY, DestinationX, DestinationY]), nl. % Print the move
