% Level 0: Human player move
%
% The choose_move predicate handles the move for a human player (difficulty level 0).
% The human player is prompted to make a move through the prompt_for_move predicate.
choose_move(0, GameState, Move) :-
    prompt_for_move(GameState, Move). % Prompt the human player for their move.


% Level 1: Random move
%
% This version of choose_move is used for a random move (difficulty level 1).
% It calculates all valid moves for the current game state using valid_moves/2, 
% then randomly selects one of them using random_member/2.
choose_move(1, GameState, Move) :-
    valid_moves(GameState, ListOfMoves), % Get all valid moves.
    %print_valid_moves(ListOfMoves), % (Optional) Uncomment to print valid moves (for debugging).
    random_member(Move, ListOfMoves).  % Choose a random move from the list of valid moves.


% Level 2: Greedy strategy
%
% In this version, the AI uses a simple greedy strategy to choose the best move.
% It evaluates all valid moves for the current player and then selects the best one 
% based on the value predicate.
choose_move(2, GameState, BestMove) :- % Level 2 is a greedy strategy.
    GameState = game_state(Board, CurrentPlayer, _, _), % Extract the current game state.
    
    % Find all valid moves for the current player using valid_moves_for_player/3.
    findall(Move, valid_moves_for_player(Board, CurrentPlayer, Move), Moves),
    
    write('Evaluating moves:'), nl, % Print a message indicating that moves are being evaluated.
    
    % Evaluate each move and assign a score to it using evaluate_moves/3.
    evaluate_moves(GameState, Moves, ScoredMoves),
    
    % Select the best move based on the scores.
    select_best_move(ScoredMoves, BestMove). % Select the move with the highest score.



choose_move(GameState, Level, Move). % this cannot be deleted for black magic reasons