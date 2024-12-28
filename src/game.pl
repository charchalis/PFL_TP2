% Anaash Game in SICStus Prolog

play :-
    welcome,
    DefaultConfig = game_config(human_vs_computer, easy, small),
    write('popo'),
    main_menu(DefaultConfig).

welcome :-
    nl,
    write('----------------------'), nl,
    write('| Welcome to Anaash! |'), nl,
    write('----------------------'), nl, nl.


reload :-
    [game].  % reload when changes are made




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
    initial_state(GameConfig, GameState).

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

initial_state(GameConfig, GameState).

display_game(GameState).

move(GameState, Move, NewGameState).

valid_moves(GameState, Winner).

game_over(GameState, Winner).

value(GameState, Player, Value).

choose_move(GameState, Level, Move).