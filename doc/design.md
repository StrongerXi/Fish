# Data
- A `referee` is a `(game_tree, player_color external_player map)`
  - A `external_player` is one of
    - `AI (id, player_color)`
    - `Remote (id, socket in/out channels)`
      - An `id` is an integer

- A `game_tree` is a `(game_state, current_player)`
  - A `current_player` is a natural number

- A `game_state` is a `(board, player_list)`
  - A `player_list` is a `player_state list`
    - A `player_state` is a `(player_color, score, penguin list)`
      - A `player_color` is basically an enum
      - A `score` is a natural number
    - A `penguin` is a `(int, position)`
      - `int` represents the # of fish it's currently holding
    - A `position` is a `(row, col)`, 2 integers
  - A `board` is a collection of `tile`
    - A `tile` has how many fish is on it
  

# High level control flow

## Server
1. `signup_handler` gets and confirm a minimum number of connections (client sign-ups)
2. `tournament_manager` 
  - receives those connections
  - assign each a player id and turn it into an external player representation
  - divide them into groups and start the tournament
3. `referee` handles a single game for a group of players
  - Assign colors to players.
  - Initial penguin placement round
  - Actual game (with penguin movement)
  - End of game, report result to observers
4. `tournament_manager` 
  - wait for all games to finish
  - collect results and either start up a new round (goto 2), or declare final
    winner.

## Client
- Connect to server, and synchronously respond to each request from server.
