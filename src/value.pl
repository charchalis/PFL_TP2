% valid_moves_for_player(+Board, +Player, -Move)
%
% This predicate generates all valid moves for a given player.
% It checks the current player's pieces on the board and generates possible destinations
% for each piece based on adjacent coordinates. 
%
% @param Board The current state of the game board.
% @param Player The player whose valid moves are being calculated (either 'player1' or 'player2').
% @param (Origin, Destination) The move, where Origin is the starting position and Destination is the target position.
valid_moves_for_player(Board, Player, (Origin, Destination)) :-
    nth1(X, Board, Row),                 % For each row on the board.
    nth1(Y, Row, (Stack, Owner)),        % For each position in the row, find the stack and the owner.
    Owner = Player,                      % Only consider pieces belonging to the current player.
    Origin = (X, Y),                     % The origin of the move is the current position of the piece.
    (DestinationX is X + 1, Destination = (DestinationX, Y);  % Generate possible moves (up, down, left, right).
     DestinationX is X - 1, Destination = (DestinationX, Y);
     DestinationY is Y + 1, Destination = (X, DestinationY);
     DestinationY is Y - 1, Destination = (X, DestinationY)),
    valid_move(Board, Player, (Origin, Destination)).  % Check if the move is valid according to the rules.


% evaluate_moves(+GameState, +Moves, -ScoredMoves)
%
% This predicate evaluates each move by calculating a score for each one.
% It uses a heuristic function defined in the `value/2` predicate to score the game state
% after performing each move.
%
% @param GameState The current game state before the move.
% @param Moves A list of possible moves to evaluate.
% @param ScoredMoves A list of moves paired with their corresponding scores.
evaluate_moves(_, [], []).  % Base case: no moves to evaluate, return empty list.
evaluate_moves(GameState, [Move | RestMoves], [Score-Move | RestScoredMoves]) :-
    move(GameState, Move, NewGameState),   % Perform the move and generate the new game state.
    value(NewGameState, Score),             % Evaluate the new game state to assign a score.
    Move = ((OriginX, OriginY), DestinationX, DestinationY),  % Unify the move components.
    format('(~w, ~w) -> (~w, ~w), value: ~w', [OriginX, OriginY, DestinationX, DestinationY, Score]), nl,  % Print the move and its score.
    evaluate_moves(GameState, RestMoves, RestScoredMoves).  % Recursively evaluate the rest of the moves.


% select_best_move(+ScoredMoves, -BestMove)
%
% This predicate selects the best move from a list of scored moves by picking the one with the highest score.
% It may select randomly from among the best-scoring moves.
%
% @param ScoredMoves A list of moves paired with their corresponding scores.
% @param BestMove The move with the highest score.
select_best_move(ScoredMoves, BestMove) :-
    maplist(score_from_pair, ScoredMoves, Scores),  % Extract the scores from the list of Score-Move pairs.
    max_in_list(Scores, MaxScore),  % Find the maximum score from the list.
    include(is_best_move(MaxScore), ScoredMoves, BestScoredMoves),  % Filter moves with the maximum score.
    random_member(_Score-BestMove, BestScoredMoves).  % Randomly select one of the best-scoring moves.


% score_from_pair(+Score-Move, -Score)
%
% This helper predicate extracts the score from a Score-Move pair.
%
% @param Score-Move The pair consisting of a score and a move.
% @param Score The extracted score.
score_from_pair(Score-_, Score).


% is_best_move(+MaxScore, +Score-Move)
%
% This helper predicate checks if a move has the maximum score.
%
% @param MaxScore The maximum score.
% @param Score-Move The Score-Move pair to check.
is_best_move(MaxScore, Score-_) :-
    Score =:= MaxScore.  % The move has the maximum score if its score is equal to MaxScore.


% max_in_list(+List, -Max)
%
% This predicate finds the maximum element in a list of numbers.
% The maximum is determined by recursively comparing the elements of the list.
%
% @param List The list of numbers to search for the maximum.
% @param Max The maximum number in the list.
max_in_list([X], X).  % Base case: the only element in the list is the maximum.
max_in_list([X | Rest], Max) :-
    max_in_list(Rest, TailMax),  % Recursively find the maximum in the tail of the list.
    Max is max(X, TailMax).  % Compare the current element with the maximum of the tail.


% value(+GameState, -Score)
%
% This predicate calculates a heuristic score for a given game state.
% The score is calculated based on the piece values, positional values, stack values, and threats for each player.
%
% @param GameState The current game state.
% @param Score The calculated score for the game state.
value(game_state(Board, CurrentPlayer, _, _), Score) :-
    switch_player(CurrentPlayer, RealCurrentPlayer),  % Switch the current player to the real player.
    
    % Calculate the value of pieces for the current player.
    findall(PieceValue, (
        nth1(X, Board, Row),
        nth1(Y, Row, (Stack, Owner)),
        Owner = RealCurrentPlayer,  % Only consider pieces belonging to the real current player.
        positional_value(Board, X, Y, PositionalValue),  % Get the positional value.
        stack_value(Stack, StackValue),  % Get the stack value.
        PieceValue is PositionalValue * StackValue  % Combine the positional and stack values.
    ), CurrentPlayerPieceValues),
    sum_list(CurrentPlayerPieceValues, TotalCurrentPlayerPieceValue),  % Sum all piece values for the current player.

    % Calculate the value of pieces for the opponent.
    findall(PieceValue, (
        nth1(X, Board, Row),
        nth1(Y, Row, (Stack, Owner)),
        Owner \= RealCurrentPlayer,  % Only consider pieces belonging to the opponent.
        positional_value(Board, X, Y, PositionalValue),  % Get the positional value.
        stack_value(Stack, StackValue),  % Get the stack value.
        PieceValue is PositionalValue * StackValue  % Combine the positional and stack values.
    ), OpponentPieceValues),
    sum_list(OpponentPieceValues, TotalOpponentPieceValue),  % Sum all piece values for the opponent.

    % Calculate threat penalties for the current player.
    findall(ThreatPenalty, (
        nth1(X, Board, Row),
        nth1(Y, Row, (Stack, Owner)),
        Owner = RealCurrentPlayer,  % Only consider pieces belonging to the current player.
        threat_value(Board, (Stack, Owner), X, Y, ThreatPenalty)  % Get the threat value for the piece.
    ), CurrentPlayerThreatPenalties),
    sum_list(CurrentPlayerThreatPenalties, TotalCurrentPlayerThreatPenalty),  % Sum all threat penalties for the current player.

    % Calculate the final score, weighing the piece values and threat penalties.
    Score is (TotalCurrentPlayerPieceValue - TotalOpponentPieceValue) - TotalCurrentPlayerThreatPenalty.


% stack_value(+Stack, -Value)
%
% This predicate calculates the value of a stack based on its size.
% Higher stacks are given more value.
%
% @param Stack The number of pieces in the stack.
% @param Value The calculated value for the stack.
stack_value(Stack, Value) :-
    Value is (Stack * (Stack // 2 + 1)).  % Value increases with the size of the stack.


% positional_value(+Board, +X, +Y, -Value)
%
% This predicate calculates the positional value of a piece based on its location on the board.
% Pieces closer to the center of the board are valued higher.
%
% @param Board The current state of the game board.
% @param X The X-coordinate of the piece.
% @param Y The Y-coordinate of the piece.
% @param Value The calculated positional value.
positional_value(Board, X, Y, Value) :-
    length(Board, BoardSize),  % Get the board size (number of rows).
    Center is BoardSize / 2,  % Calculate the approximate center of the board.
    BoardCenterX is ceiling(Center),  % Get the center coordinates.
    BoardCenterY is ceiling(Center),
    Distance is abs(BoardCenterX - X) + abs(BoardCenterY - Y),  % Manhattan distance from the center.
    MaxDistance is BoardSize * 2,  % Maximum possible Manhattan distance.
    Value is ceiling((MaxDistance - Distance) / 2).  % Higher values are assigned to pieces near the center.


% threat_value(+Board, +Piece, +X, +Y, -ThreatValue)
%
% This predicate calculates the threat value for a piece based on how many enemy pieces threaten it.
%
% @param Board The current state of the game board.
% @param Piece The piece under evaluation.
% @param X The X-coordinate of the piece.
% @param Y The Y-coordinate of the piece.
% @param ThreatValue The calculated threat value.
threat_value(Board, (Stack, Owner), X, Y, ThreatValue) :-
    findall(EnemyStack, (
        adjacent(X, Y, AdjX, AdjY),  % Check adjacent cells.
        nth1(AdjX, Board, Row),
        nth1(AdjY, Row, (EnemyStack, EnemyOwner)),
        EnemyOwner \= Owner,  % Only consider enemy pieces.
        EnemyStack >= Stack  % The enemy piece must have a stack size equal to or greater than the current piece.
    ), ThreateningStacks),
    length(ThreateningStacks, ThreatCount),  % Count how many enemy pieces threaten the current piece.
    ThreatValue is ThreatCount.  % The threat value is the number of enemy threats.


% adjacent(+X, +Y, -AdjX, -AdjY)
%
% This predicate generates the coordinates of the adjacent cells (up, down, left, right).
%
% @param X, Y The coordinates of the current cell.
% @param AdjX, AdjY The coordinates of the adjacent cell.
adjacent(X, Y, AdjX, AdjY) :-
    (AdjX is X + 1, AdjY is Y);
    (AdjX is X - 1, AdjY is Y);
    (AdjX is X, AdjY is Y + 1);
    (AdjX is X, AdjY is Y - 1).


% sum_list(+List, -Sum)
%
% This predicate sums the elements of a list.
%
% @param List The list of numbers to sum.
% @param Sum The total sum of the list.
sum_list([], 0).  % Base case: an empty list sums to 0.
sum_list([Head | Tail], Sum) :-
    sum_list(Tail, TailSum),  % Recursively sum the tail of the list.
    Sum is Head + TailSum.  % Add the head of the list to the sum.


% player_captured(+Player, +Piece)
%
% This predicate checks if a piece belongs to a player.
% It is used to determine if a piece has been captured by the player.
%
% @param Player The player who may have captured the piece.
% @param Piece The piece under evaluation.
player_captured(Player, (_, Player)).
