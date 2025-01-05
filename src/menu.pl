% Main menu loop
main_menu(GameConfig) :-
    GameConfig = game_config(GameType, Difficulty, Size), % Decompose the current game configuration
    % Display menu options with current settings
    write('1. start ('), write(GameType), write(' | '), write(Difficulty), write(' | '), write(Size), write(' board )'), nl,
    write('2. game type'), nl,
    write('3. difficulty'), nl,
    write('4. board size'), nl,
    write('5. leave'), nl, nl,
    % Read user's choice and handle it
    read_option('Choose an option', 1, 5, Option),
    handle_main_menu(Option, GameConfig). % Process the selected option

% Handle menu options
handle_main_menu(1, GameConfig):- 
    % Start the game
    write('Starting game...'), nl,
    initial_state(GameConfig, GameState), % Initialize the game state based on the config
    game_cycle(GameState), % Start the game cycle
    main_menu(GameConfig). % Return to the main menu after the game

handle_main_menu(2, GameConfig):- 
    % Navigate to game type menu
    game_type_menu(GameConfig).

handle_main_menu(3, GameConfig):- 
    % Navigate to difficulty menu
    difficulty_menu(GameConfig).

handle_main_menu(4, GameConfig):- 
    % Navigate to board size menu
    size_menu(GameConfig).

handle_main_menu(5, _):- 
    % Exit the game
    write('Bye bye.'), nl,
    halt. % Terminate the program

% Game type selection menu
game_type_menu(game_config(_, Difficulty, Size)) :-
    % Display game type options
    write('1. Human vs Human (H/H)'), nl,
    write('2. Human vs Computer (H/PC)'), nl,
    write('3. Computer vs Human (PC/H)'), nl,
    write('4. Computer vs Computer (PC/PC)'), nl, nl,
    % Read user's choice and update the configuration
    read_option('Choose an option', 1, 4, Option),
    handle_game_type_menu(Option, GameType),
    main_menu(game_config(GameType, Difficulty, Size)). % Return to the main menu with the updated game type

% Handle specific game type options
handle_game_type_menu(1, human_vs_human).
handle_game_type_menu(2, human_vs_computer).
handle_game_type_menu(3, computer_vs_human).
handle_game_type_menu(4, computer_vs_computer).

% Difficulty selection menu
difficulty_menu(game_config(GameType, _, Size)) :-
    % Display difficulty options
    write('1. random'), nl,
    write('2. greedy'), nl,
    % Read user's choice and update the configuration
    read_option('Choose an option', 1, 2, Option),
    handle_difficulty_menu(Option, Difficulty),
    main_menu(game_config(GameType, Difficulty, Size)). % Return to the main menu with the updated difficulty

% Handle specific difficulty options
handle_difficulty_menu(1, 1). % Random difficulty
handle_difficulty_menu(2, 2). % Greedy difficulty

% Board size selection menu
size_menu(game_config(GameType, Difficulty, _)) :-
    % Display board size options
    write('1. small board\t(6x6)'), nl,
    write('2. big board\t(8x8)'), nl,
    % Read user's choice and update the configuration
    read_option('Choose an option', 1, 2, Option),
    handle_size_menu(Option, Size),
    main_menu(game_config(GameType, Difficulty, Size)). % Return to the main menu with the updated board size

% Handle specific board size options
handle_size_menu(1, small). % Small board
handle_size_menu(2, big).   % Big board

% Reads and validates user input within a range
read_option(Prompt, Min, Max, Option) :-
    write(Prompt), write(' ('), write(Min), write('-'), write(Max), write(') '), % Display the prompt
    read(Input), % Read user input
    validate_option(Input, Min, Max, Option). % Validate the input

% Validates user input as an integer within the range
validate_option(Input, Min, Max, Option) :-
    integer(Input), % Check if the input is an integer
    Input >= Min, % Ensure input is within the minimum bound
    Input =< Max, % Ensure input is within the maximum bound
    Option = Input. % Assign the valid input to Option

% Handles invalid input by prompting the user again
validate_option(_, Min, Max, Option) :-
    write('Invalid option. Please try again.'), nl, % Display error message
    read_option('Choose an option', Min, Max, Option). % Recursively prompt for valid input
