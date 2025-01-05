% Display the current game state
display_game(game_state(Board, CurrentPlayer, _, _)) :-
    nl, nl, % Newlines for formatting
    write('Current Player: '), % Display the current player
    color_player(CurrentPlayer), nl, nl, % Display the player's name in their color
    display_board(Board), % Display the game board
    nl.

% Display the current player's name in color
color_player(player1) :-
    write_colored_text(red, player1). % Player 1 is represented in red

color_player(player2) :-
    write_colored_text(blue, player2). % Player 2 is represented in blue

% Display the board with axes
display_board(Board) :-
    length(Board, Size), % Get the size of the board
    write('  Y'), nl, % Label the vertical (Y) axis
    display_rows(Board, Size), % Display the board rows
    display_column_headers(Size), % Display the column headers for the X axis
    nl.

% Display column headers (X-axis labels: 1, 2, 3, ...)
display_column_headers(Size) :-
    Padding = '      ', % Padding for alignment
    write(Padding),
    display_column_headers_limiter(Size), nl, % Draw column header separators
    write(Padding),
    numlist(1, Size, Columns), % Generate a list of column numbers
    maplist(format_column_header, Columns), % Format and display column headers
    write(' X'), nl. % Label the horizontal (X) axis

% Draw a row of separators for column headers
display_column_headers_limiter(0). % Base case: no more columns to display
display_column_headers_limiter(Size) :-
    Size > 0,
    write('---'), % Separator for each column
    NewSize is Size - 1,
    display_column_headers_limiter(NewSize). % Recur for the remaining columns

% Format and display a single column header
format_column_header(Column) :-
    format(' ~w ', [Column]). % Display column number with spaces for alignment

% Display all rows of the board
display_rows(_, 0). % Base case: no more rows to display
display_rows(Board, RowIndex) :-
    RowIndex > 0, % Proceed only if RowIndex is positive
    format('~2|~w | ', [RowIndex]), % Display the row number with alignment
    display_row(Board, RowIndex), % Display the cells in the current row
    nl, % Newline for the next row
    NewRowIndex is RowIndex - 1, % Move to the next row
    display_rows(Board, NewRowIndex). % Recur for the remaining rows

% Display all cells in a specific row
display_row(Board, RowIndex) :-
    maplist(nth1(RowIndex), Board, RowCells), % Collect all cells in the row
    display_cells(RowCells). % Display the row's cells

% Display a list of cells in a row
display_cells([]). % Base case: no more cells to display
display_cells([(Stack, Player) | Rest]) :-
    write(' '), % Add spacing
    display_cell(Stack, Player), % Display the cell with player and stack
    write(' '), % Add spacing
    display_cells(Rest). % Recur for the remaining cells

% Display empty cells
display_cells([0 | Rest]) :-
    write(' . '), % Represent an empty cell as '.'
    display_cells(Rest).

% Display a single cell with color and stack information
display_cell(Stack, player1) :- write_colored_text(red, Stack). % Player 1 in red
display_cell(Stack, player2) :- write_colored_text(blue, Stack). % Player 2 in blue

% Display an empty cell
display_cell(0, _) :- write('.'). % Represent an empty cell as '.'

% Write text with color formatting
write_with_color(ColorCode, Text) :-
    format('\e[~wm~w\e[0m', [ColorCode, Text]). % ANSI escape sequence for colors

% Map colors to ANSI color codes
color_code(black, 30).
color_code(red, 31).
color_code(green, 32).
color_code(yellow, 33).
color_code(blue, 34).
color_code(magenta, 35).
color_code(cyan, 36).
color_code(white, 37).

% Display text in a specific color
write_colored_text(Color, Text) :-
    color_code(Color, Code), % Get the ANSI code for the color
    write_with_color(Code, Text). % Write the text in the specified color

% Generate a list of numbers from Start to End
numlist(Start, End, List) :-
    Start =< End, % Ensure the start is less than or equal to the end
    numlist_helper(Start, End, List). % Delegate to the helper predicate

% Helper predicate for numlist
numlist_helper(Current, End, [Current | Rest]) :-
    Current =< End, % Ensure the current value is within bounds
    Next is Current + 1, % Increment the current value
    numlist_helper(Next, End, Rest). % Recur for the remaining range
numlist_helper(Current, End, []) :-
    Current > End. % Base case: stop when the current value exceeds the end
