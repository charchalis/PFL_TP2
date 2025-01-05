% game_over/2
%
% This predicate checks if the game is over and determines the winner.
% It checks the counts of pieces for each player and compares them.
% If one player has no pieces remaining, the other player is declared the winner.
%
% @param game_state(Board, _, _, _) The current state of the game, with the board representing the state of the game pieces.
% @param Winner The player who won the game, either 'player1' or 'player2'.
game_over(game_state(Board, _, _, _), Winner) :-
    count_pieces(Board, player1, Player1Count), % Count the number of pieces for player1.
    count_pieces(Board, player2, Player2Count), % Count the number of pieces for player2.
    winner(Player1Count, Player2Count, Winner).  % Determine the winner based on the piece counts.


% winner/2
%
% This predicate determines the winner based on the number of pieces left for each player.
% If one player's count is 0, the other player is the winner.
%
% @param Player1Count The number of pieces for player1.
% @param Player2Count The number of pieces for player2.
% @param Winner The player who is declared the winner.
winner(Player1Count, 0, player1).  % If player2 has no pieces left, player1 wins.
winner(0, Player2Count, player2).  % If player1 has no pieces left, player2 wins.


% count_pieces/3
%
% This predicate counts the number of pieces for a given player on the board.
% It searches through the board, checking all cells for a stack owned by the specified player (where Stack > 0).
%
% @param Board The current state of the game board, a list of rows containing stacks.
% @param Player The player whose pieces are being counted (either 'player1' or 'player2').
% @param Count The number of pieces belonging to the specified player.
count_pieces(Board, Player, Count) :-
    findall(Stack, (
        nth1(_, Board, Row),  % Iterate through each row on the board.
        nth1(_, Row, (Stack, Player)),  % Look for the stack owned by the specified player.
        Stack > 0  % Only consider non-zero stacks (i.e., stacks that have pieces).
    ), Pieces),  % Collect all the stacks that satisfy the conditions.
    length(Pieces, Count).  % Return the total number of pieces.
