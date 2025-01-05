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
    % Construct the game state
    GameState = game_state(
        Board,
        CurrentPlayer,
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

