% Anaash Game in SICStus Prolog

initial_state(GameConfig, GameState).

display_game(GameState).

move(GameState, Move, NewGameState).

valid_moves(GameState, Winner).

game_over(GameState, Winner).

value(GameState, Player, Value).

choose_move(GameState, Level, Move).