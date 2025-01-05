% Anaash Game in SICStus Prolog


% Importing necessary modules
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).

% Importing game-specific modules
:- use_module('menu.pl'). % Manages the game's main menu
:- use_module('initial_state.pl'). % Handles the initialization of the game state
:- use_module('display_game.pl'). % Manages displaying the game state to the player
:- use_module('move.pl'). % Handles the mechanics of making moves in the game
:- use_module('game_over.pl'). % Checks if the game has ended and determines the winner
:- use_module('value.pl'). % Defines game-related value functions
:- use_module('choose_move.pl'). % Implements logic for choosing the next move




% play/0 - Entry point of the game
% This is the entry point for starting the game. It is called to initialize and begin the game.
% The predicate performs a series of steps to set up the initial state of the game, display the welcome message,
% and launch the main menu where the game settings can be further customized or used to start the game.
play :-
    welcome,  % Display the welcome message to the user.
    DefaultConfig = game_config(computer_vs_computer, 2, small),  % Set the default game configuration:
        % computer_vs_computer: This indicates the game is between two computer players.
        % 2: The difficulty level is set to 2 (greedy).
        % small: The board size is small.
    main_menu(DefaultConfig).  % Launch the main menu, passing the default configuration for the game.




% Displays the welcome message
welcome :-
    nl,
    write('----------------------'), nl,
    write('| Welcome to Anaash! |'), nl,
    write('----------------------'), nl, nl.


% Reloads the game
reload :-
    [game].



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
    GameState = game_state(_, CurrentPlayer, GameType, _),
    display_game(GameState), % Display the current game state
    game_over(GameState, Winner), % Check if the game is over
    !, % Cut to prevent backtracking
    format("Game over! Winner is: ~w~n", [Winner]), nl, nl. % Print the winner

% If the game is not over, continue processing moves
game_cycle(GameState) :- 
    GameState = game_state(_, CurrentPlayer, GameType, _), % Get the current player and the game type
    species_identificator(CurrentPlayer, GameType, IsHuman), % Check if the current player is human
    next_move(IsHuman, GameState, Move), % Get the next move
    move(GameState, Move, NewGameState), % Apply the move
    print_move(NewGameState, Move), % Print the move
    game_cycle(NewGameState). % Continue the game loop with the new game state




